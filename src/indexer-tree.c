/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

/* This file implements a "hash tree" data structures for encoding the
   contents of a file.  This structure is also known as "Merkle's Hash
   Trees", named after its original author [1].

   [1]  Ralph C. Merkel, Protocols for Public Key Cryptosystems,
        IEEE Symp. on Security and Privacy, pp. 122--134, 1980.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/indexers.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>

/* Glibc's obstacks */
/* #include <obstack.h> */

/* libgcrypt */
#include <gcrypt.h>

#if HAVE_NETINET_IN_H
/* `htons ()' and friends */
# include <netinet/in.h>
#else
# error "Where can I find `htons ()'?"
#endif




/* Declare and define the `chop_tree_indexer_t' class and its run-time
   representation CHOP_TREE_INDEXER_CLASS.  */
CHOP_DECLARE_RT_CLASS (tree_indexer, indexer,

		       /* Hash method and message digest size (in bytes) */
		       size_t             indexes_per_block;

		       /* For debugging purposes */
		       chop_log_t         log;);

static errcode_t
chop_tree_index_blocks (chop_indexer_t *indexer,
			chop_chopper_t *input,
			chop_block_indexer_t *block_indexer,
			chop_block_store_t *output,
			chop_block_store_t *metadata,
			chop_index_handle_t *handle);

static errcode_t
chop_tree_fetch_stream (struct chop_indexer *,
			const chop_index_handle_t *,
			chop_block_fetcher_t *,
			chop_block_store_t *,
			chop_block_store_t *,
			chop_stream_t *);

extern const chop_class_t chop_tree_stream_class;

static errcode_t
iht_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_tree_indexer_t *htree;

  htree = (chop_tree_indexer_t *)object;
  htree->indexer.index_blocks = chop_tree_index_blocks;
  htree->indexer.fetch_stream = chop_tree_fetch_stream;
  htree->indexer.stream_class = &chop_tree_stream_class;

  htree->indexes_per_block = 0;

  return chop_log_init ("hash-tree-indexer", &htree->log);
}

static void
iht_dtor (chop_object_t *object)
{
  chop_tree_indexer_t *htree;

  htree = (chop_tree_indexer_t *)object;
  htree->indexes_per_block = 0;
  chop_object_destroy ((chop_object_t *)&htree->log);
}

static errcode_t
iht_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  errcode_t err = 0;
  chop_tree_indexer_t *ti = (chop_tree_indexer_t *)object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	char buf[50];

	snprintf (buf, sizeof (buf), "%x", ti->indexes_per_block);
	err = chop_buffer_push (buffer, buf, strlen (buf) + 1);

	break;
      }

    default:
      err = CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

static errcode_t
iht_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;
  chop_tree_indexer_t *ti = (chop_tree_indexer_t *)object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	char *end;
	unsigned long int indices_per_block;

	indices_per_block = strtoul (buffer, &end, 16);
	if (end == buffer)
	  return CHOP_DESERIAL_CORRUPT_INPUT;

	err = chop_object_initialize ((chop_object_t *)ti,
				      &chop_tree_indexer_class);
	if (err)
	  return err;

	ti->indexes_per_block = indices_per_block;
	*bytes_read = end - buffer;

	break;
      }

    default:
      *bytes_read = 0;
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

CHOP_DEFINE_RT_CLASS (tree_indexer, indexer,
		      iht_ctor, iht_dtor,
		      NULL, NULL, /* No copy/equalp */
		      iht_serialize, iht_deserialize);



/* Internal data structures.  */

/* This represents a key block or "i-node", i.e. a block whose content is a
   vector of keys of data blocks or key blocks.  */
typedef struct key_block
{
  /* Pointer to the key block that contains a pointer to it */
  struct key_block *parent;

  /* KEY_COUNT concatenated block keys.  This contains a header which is
     KEY_BLOCK_HEADER_SIZE bytes long.  */
#define KEY_BLOCK_HEADER_SIZE  (10)
#define KEY_BLOCK_MAGIC_1      'H'
#define KEY_BLOCK_MAGIC_2      'T'
  chop_buffer_t keys;
  size_t        key_count;

  /* Depth of this block's subtree */
  size_t depth;

  /* Pointer to the indexer's log (for debugging purposes) */
  chop_log_t *log;
} key_block_t;


/* A tree of key blocks.  */
typedef struct key_block_tree
{
  /* The current bottom-most key block */
  key_block_t *current;

  /* Number of keys per key block */
  size_t indexes_per_block;

  /* Meta-data block store: the block store where key blocks are flushed */
  chop_block_store_t *metadata_store;

  /* Pointer to the indexer's log (for debugging purposes) */
  chop_log_t *log;
} key_block_tree_t;


#if 0
/* A vector of zeros.  */
static char zero_vector[4000] = { 0, };
#endif


/* Initialize key block tree TREE.  */
static inline void
chop_block_tree_init (key_block_tree_t *tree, size_t indexes_per_block,
		      chop_block_store_t *metadata_store, chop_log_t *log)
{
  tree->current = NULL;
  tree->indexes_per_block = indexes_per_block;
  tree->metadata_store = metadata_store;
  tree->log = log;
}

/* Fill in the KEYS field of BLOCK with a header.  This should be called by
   CHOP_KEY_BLOCK_FLUSH, right before BLOCK is actually written.  */
static inline void
chop_key_block_fill_header (key_block_t *block)
{
  size_t count = block->key_count;
  size_t depth = block->depth;
  unsigned char *header, *start;

  header = start = (unsigned char *)chop_buffer_content (&block->keys);

  assert (chop_buffer_size (&block->keys) >= KEY_BLOCK_HEADER_SIZE);

  *(header++) = KEY_BLOCK_MAGIC_1;
  *(header++) = KEY_BLOCK_MAGIC_2;

  *(header++) = (count & 0xff);  count >>= 8;
  *(header++) = (count & 0xff);  count >>= 8;
  *(header++) = (count & 0xff);  count >>= 8;
  *(header++) = (count & 0xff);  count >>= 8;
  assert (!count);

  *(header++) = (depth & 0xff);  depth >>= 8;
  *(header++) = (depth & 0xff);  depth >>= 8;
  assert (!depth);

  assert (header - start <= KEY_BLOCK_HEADER_SIZE);

  /* Don't leave uninitialized bytes.  */
  memset (header, 0,
	  KEY_BLOCK_HEADER_SIZE - (header - start));
}


/* Flush BLOCK to METADATA and return its index in INDEX.  The memory used by
   BLOCK can now be reused.  */
static errcode_t
chop_key_block_flush (key_block_t *block,
		      chop_block_indexer_t *block_indexer,
		      chop_block_store_t *metadata,
		      chop_index_handle_t *index)
{
  errcode_t err;

  chop_log_printf (block->log,
		   "key_block_flush: block %p with %u keys being flushed "
		   "to store %p\n",
		   block, block->key_count, metadata);

  chop_key_block_fill_header (block);

  /* FIXME:  Maybe we should pad BLOCK->KEYS with zero and create fixed-size
     blocks.  */
/*   block_size = KEY_BLOCK_HEADER_SIZE + (block->key_count * block->key_size); */

  err = chop_block_indexer_index (block_indexer, metadata,
				  chop_buffer_content (&block->keys),
				  chop_buffer_size (&block->keys),
				  index);

  return err;
}

/* Initialize the key block pointed to by BLOCK.  */
static inline errcode_t
chop_key_block_init (size_t indexes_per_block,
		     chop_log_t *log,
		     key_block_t *block)
{
  errcode_t err;
  char fake_header[KEY_BLOCK_HEADER_SIZE] = { 0, };

  err = chop_buffer_init (&block->keys, indexes_per_block * 20);
  if (err)
    return err;

  /* Reserve room for the key block header.  It will eventually get filled by
     `chop_key_block_fill_header ()'.  */
  err = chop_buffer_push (&block->keys, fake_header,
			  KEY_BLOCK_HEADER_SIZE);
  if (err)
    return err;

  block->parent = NULL;
  block->depth = 0;
  block->key_count = 0;
  block->log = log;

  return 0;
}

/* Shortcut to partly re-initialize BLOCK in order to quickly re-use it
   in-place afterwards.  */
static inline void
chop_key_block_reinit (key_block_t *block)
{
  char fake_header[KEY_BLOCK_HEADER_SIZE] = { 0, };

  chop_buffer_clear (&block->keys);

  chop_buffer_push (&block->keys, fake_header,
		    KEY_BLOCK_HEADER_SIZE);

  block->key_count = 0;
}

/* Allocate and initialize a new key block and return it in BLOCK.  */
static inline errcode_t
chop_key_block_new (size_t indexes_per_block,
		    chop_log_t *log,
		    key_block_t **block)
{
  errcode_t err;

  /* XXX: Use obstacks.  */
  *block = (key_block_t *) chop_malloc (sizeof (key_block_t),
					&chop_tree_indexer_class);
  if (!*block)
    return ENOMEM;

  err = chop_key_block_init (indexes_per_block,
			     log, *block);
  if (err)
    {
      chop_free (*block, &chop_tree_indexer_class);
      *block = NULL;
    }

  return err;
}

static inline void
chop_key_block_destroy (key_block_t *block)
{
  chop_buffer_return (&block->keys);
  block->parent = NULL;
  block->log = NULL;
  block->depth = block->key_count = 0;
}


/* Append INDEX to BLOCK which can contain at most INDEXES_PER_BLOCK block keys.
   When full, BLOCK is written to METADATA and its contents are reset.  */
static errcode_t
chop_key_block_add_index (key_block_t *block, size_t indexes_per_block,
			  chop_block_store_t *metadata,
			  chop_block_indexer_t *block_indexer,
			  const chop_index_handle_t *index)
{
  errcode_t err;
  chop_buffer_t buffer;

  if (block->key_count + 1 >= indexes_per_block)
    {
      /* This key block is full:
	 1.  flush it;
	 2.  add its key to its parent key block;
	 3.  reset it and append KEY to it.  */
      const chop_class_t *index_class;
      key_block_t *parent = block->parent;
      chop_index_handle_t *block_index;

      index_class = chop_object_get_class ((chop_object_t *)index);
      block_index = chop_class_alloca_instance (index_class);
      err = chop_key_block_flush (block, block_indexer, metadata, block_index);
      if (err)
	return err;

      if (!parent)
	{
	  /* BLOCK is orphan: create him a parent key block.  */
	  err = chop_key_block_new (indexes_per_block,
				    block->log, &parent);
	  if (err)
	    {
	      chop_object_destroy ((chop_object_t *)block_index);
	      return err;
	    }

	  parent->depth = block->depth + 1;
	  block->parent = parent;
	}

      /* Recursive call.  */
      err = chop_key_block_add_index (parent, indexes_per_block, metadata,
				      block_indexer, block_index);
      if (err)
	{
	  chop_object_destroy ((chop_object_t *)block_index);
	  return err;
	}

      /* Re-initialize it.  */
      chop_key_block_reinit (block);

      chop_object_destroy ((chop_object_t *)block_index);
    }

  /* Append a binary serialization of INDEX.  */

#if 0
  /* FIXME: This assertion must eventually vanish because it assumes that all
     indices have the same serialized size.  */
  assert (chop_buffer_size (&block->keys)
	  == KEY_BLOCK_HEADER_SIZE
	  + (block->key_count * chop_index_handle_binary_size (index)));
#endif

  chop_buffer_init (&buffer, chop_index_handle_binary_size (index));
  err = chop_object_serialize ((chop_object_t *)index, CHOP_SERIAL_BINARY,
			       &buffer);
  if (err)
    {
      chop_log_printf (block->log, "failed to binary-serialize `%s' index",
		       chop_class_name
		       (chop_object_get_class ((chop_object_t *)index)));
      goto finish;
    }

  assert (chop_buffer_size (&buffer) == chop_index_handle_binary_size (index));
  err = chop_buffer_append (&block->keys,   /* FIXME: Inefficient */
			    chop_buffer_content (&buffer),
			    chop_buffer_size (&buffer));

  if (!err)
    block->key_count++;

 finish:
  chop_buffer_return (&buffer);

  return err;
}

/* Append INDEX to TREE.  */
static errcode_t
chop_block_tree_add_index (key_block_tree_t *tree,
			   chop_block_indexer_t *block_indexer,
			   const chop_index_handle_t *index)
{
  errcode_t err = 0;
  key_block_t *current;

  if (!tree->current)
    {
      /* Allocate a new block tree */
      err = chop_key_block_new (tree->indexes_per_block,
				tree->log, &tree->current);
      if (err)
	return err;

      tree->current->depth = 0;
    }

  current = tree->current;

  err = chop_key_block_add_index (current, tree->indexes_per_block,
				  tree->metadata_store,
				  block_indexer, index);

  return err;
}


/* Flush all the pending key blocks of TREE and return the key of the
   top-level key block in ROOT_INDEX.  */
static errcode_t
chop_block_tree_flush (key_block_tree_t *tree,
		       chop_block_indexer_t *block_indexer,
		       chop_index_handle_t *root_index)
{
  errcode_t err = 0;
  key_block_t *block;
  size_t depth = 0, last_depth = 0;

  for (block = tree->current;
       block != NULL;
       block = block->parent)
    {
      /* Destroy the previous index.  */
      chop_object_destroy ((chop_object_t *) root_index);

      depth++;
      last_depth = block->depth;
      err = chop_key_block_flush (block, block_indexer,
				  tree->metadata_store,
				  root_index);
      if (err)
	break;

      if (block->parent)
	/* Add the key of the newly flushed block to its parent */
	err = chop_key_block_add_index (block->parent, tree->indexes_per_block,
					tree->metadata_store,
					block_indexer, root_index);
    }

  if (!err)
    chop_log_printf (tree->log,
		     "block_tree_flush: hash tree depth: %u\n", depth);
  else
    chop_log_printf (tree->log,
		     "block_tree_flush: failed: %s\n",
		     error_message (err));

  if (depth)
    assert (last_depth == depth - 1);

  return err;
}

/* Free the memory associated with TREE.  */
static void
chop_block_tree_free (key_block_tree_t *tree)
{
  key_block_t *block, *parent;

  for (block = tree->current;
       block != NULL;
       block = parent)
    {
      parent = block->parent;
      chop_key_block_destroy (block);
      chop_free (block, &chop_tree_indexer_class);
    }
}


/* Implementation of the indexer interface.  */


static errcode_t tree_stream_read (chop_stream_t *, char *,
					size_t, size_t *);

static void tree_stream_close (chop_stream_t *);

extern const chop_class_t chop_tree_stream_class;


errcode_t
chop_tree_indexer_open (size_t indexes_per_block,
			chop_indexer_t *indexer)
{
  errcode_t err;
  chop_tree_indexer_t *htree;

  err = chop_object_initialize ((chop_object_t *)indexer,
				&chop_tree_indexer_class);
  if (err)
    return err;

  htree = (chop_tree_indexer_t *)indexer;

  htree->indexes_per_block = indexes_per_block;

#if 0
#define obstack_chunk_alloc malloc
#define obstack_chunk_free  free
  obstack_alloc_failed_handler = &chop_obstack_alloc_failed_handler;
  obstack_init (&htree->key_obstack);
#endif

  return 0;
}

chop_log_t *
chop_tree_indexer_log (chop_indexer_t *indexer)
{
  chop_tree_indexer_t *htree;

  /* Gratuitous overhead.  */
  if (!chop_object_is_a ((chop_object_t *)indexer,
			 &chop_tree_indexer_class))
    return NULL;

  htree = (chop_tree_indexer_t *)indexer;

  return (&htree->log);
}


static errcode_t
chop_tree_index_blocks (chop_indexer_t *indexer,
			chop_chopper_t *input,
			chop_block_indexer_t *block_indexer,
			chop_block_store_t *output,
			chop_block_store_t *metadata,
			chop_index_handle_t *index)
{
  errcode_t err = 0;
  int first = 1;
  chop_tree_indexer_t *htree = (chop_tree_indexer_t *)indexer;
  size_t amount, total_amount = 0;
  chop_buffer_t buffer;
  key_block_tree_t tree;

  chop_block_tree_init (&tree, htree->indexes_per_block,
			metadata, &htree->log);

  err = chop_buffer_init (&buffer,
			  chop_chopper_typical_block_size (input));
  if (err)
    return err;

  /* Read blocks from INPUT until the underlying stream returns
     CHOP_STREAM_END.  Keep a copy of each block key.  */
  while (1)
    {
      chop_buffer_clear (&buffer);
      err = chop_chopper_read_block (input, &buffer, &amount);
      if (err)
	break;

      if (!amount)
	continue;

      total_amount += amount;

      if (CHOP_EXPECT_FALSE (first))
	first = 0;
      else
	/* Destroy the index of the previous block.  */
	chop_object_destroy ((chop_object_t *) index);

      /* Store this block and get its index */
      err = chop_block_indexer_index (block_indexer, output,
				      chop_buffer_content (&buffer),
				      chop_buffer_size (&buffer),
				      index);
      if (err)
	break;

      /* Add this block key to our block key tree */
      chop_block_tree_add_index (&tree, block_indexer, index);
    }

  if ((err == CHOP_STREAM_END) && (total_amount > 0))
    /* Flush the key block tree and get its key.  Here, we get the
       top-level index handle.  */
    err = chop_block_tree_flush (&tree, block_indexer, index);

  /* Free memory associated with TREE */
  chop_block_tree_free (&tree);

  chop_buffer_return (&buffer);

  if (total_amount == 0)
    /* Nothing was read so INDEX is kept uninitialized.  */
    err = CHOP_INDEXER_EMPTY_SOURCE;

  return err;
}



/* Hash tree stream implementation (for retrieval).  */


typedef struct decoded_block
{
  /* The raw buffer and its size */
  chop_buffer_t buffer;

  /* Offset within this raw buffer */
  size_t offset;

  /* Non-zero if this represents a key block (aka. "inode") */
  int is_key_block;

  /* If this is a key block, this represents its height within the tree,
     starting from the lowest key blocks (data blocks being bottommost).  */
  size_t depth;

  /* If this is a key block, this is the total number of children it has.  */
  size_t key_count;

  /* If this is a key block, this points to its current child.  We don't keep
     all blocks in memory.  */
  struct decoded_block *current_child;

  /* If this is a key block, this is the number of CURRENT_CHILD, i.e. an
     integer between zero and KEY_COUNT.  */
  size_t current_child_number;

  /* The parent block */
  struct decoded_block *parent;

  /* If this is a data block, this represents its offset within the stream
     being decoded.  */
  size_t stream_offset;

  /* Pointer to the stream's log */
  chop_log_t *log;
} decoded_block_t;

typedef struct decoded_block_tree
{
  chop_block_store_t *data_store;
  chop_block_store_t *metadata_store;
  chop_index_handle_t *index;
  chop_block_fetcher_t *fetcher;

  decoded_block_t *top_level;
  size_t current_offset;
  chop_log_t *log;
} decoded_block_tree_t;




/* Hash tree stream objects are returned by CHOP_INDEXER_FETCH_STREAM when
   reading from a hash-tree-indexed stream.  */
CHOP_DECLARE_RT_CLASS (tree_stream, stream,
		       decoded_block_tree_t tree;
		       chop_log_t log;);



static inline errcode_t
chop_decoded_block_tree_init (decoded_block_tree_t *tree,
			      chop_block_store_t *data_store,
			      chop_block_store_t *metadata_store,
			      const chop_index_handle_t *handle,
			      chop_block_fetcher_t *fetcher,
			      chop_log_t *log)
{
  errcode_t err;
  const chop_class_t *handle_class =
    chop_object_get_class ((chop_object_t *)handle);

  tree->data_store = data_store;
  tree->metadata_store = metadata_store;

  tree->index = chop_malloc (chop_class_instance_size (handle_class),
			     handle_class);
  if (!tree->index)
    return ENOMEM;

  err = chop_object_copy ((const chop_object_t *)handle,
			  (chop_object_t *)tree->index);
  if (err)
    {
      chop_free (tree->index, handle_class);
      return err;
    }

  /* FIXME:  We'd better copy FETCHER here.  */
  tree->fetcher = fetcher;

  tree->top_level = NULL;
  tree->current_offset = 0;

  tree->log = log;

  return 0;
}


/* Free resources associated with TREE.  */
static void
chop_decoded_block_tree_free (decoded_block_tree_t *tree)
{
  decoded_block_t *block, *next;

  for (block = tree->top_level;
       block != NULL;
       block = next)
    {
      next = block->current_child;
      chop_buffer_return (&block->buffer);
      chop_free (block, &chop_tree_stream_class);
    }

  tree->top_level = NULL;

  chop_object_destroy ((chop_object_t *)tree->index);
  chop_free (tree->index, &chop_tree_stream_class);
  tree->index = NULL;
}


/* Constructor.  */
static errcode_t
tree_stream_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_tree_stream_t *stream = (chop_tree_stream_t *)object;

  stream->stream.read = tree_stream_read;
  stream->stream.close = tree_stream_close;

  stream->tree.index = NULL;

  return chop_log_init ("hash-tree-stream", &stream->log);
}

/* Destructor.  */
static void
tree_stream_dtor (chop_object_t *object)
{
  chop_tree_stream_t *tstream = (chop_tree_stream_t *)object;

  chop_object_destroy ((chop_object_t *)&tstream->log);
}



CHOP_DEFINE_RT_CLASS (tree_stream, stream,
		      tree_stream_ctor, tree_stream_dtor,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);


/* The entry point.  On success, return zero and set OUTPUT to a stream
   representing the contents of the hash-tree-encoded stream pointed to by
   HANDLE.  */
static errcode_t
chop_tree_fetch_stream (struct chop_indexer *indexer,
			     const chop_index_handle_t *handle,
			     chop_block_fetcher_t *fetcher,
			     chop_block_store_t *input,
			     chop_block_store_t *metadata,
			     chop_stream_t *output)
{
  errcode_t err;
  chop_tree_stream_t *tstream;
  chop_tree_indexer_t *htree = (chop_tree_indexer_t *)indexer;

  /* OUTPUT is assumed to be as large as `chop_tree_stream_t' */
  chop_object_initialize ((chop_object_t *)output,
			  &chop_tree_stream_class);
  tstream = (chop_tree_stream_t *)output;

  /* Set up the log (for debugging purposes) that had been initialized in the
     constructor.  */
  chop_log_mimic (&tstream->log, &htree->log, 0);

  err = chop_decoded_block_tree_init (&tstream->tree, input, metadata,
				      handle, fetcher,
				      &tstream->log);

  chop_log_printf (&htree->log, "fetching stream");

  return err;
}



/* Decode BLOCK's header (which was created by CHOP_KEY_BLOCK_FILL_HEADER).
   BLOCK is assumed to be a key block.  This functions sets BLOCK's KEY_COUNT
   and DEPTH fields.  */
static inline errcode_t
chop_decoded_block_decode_header (decoded_block_t *block)
{
  char magic[2];
  const unsigned char *buffer =
    (unsigned char *)chop_buffer_content (&block->buffer);
  assert (block->is_key_block);

  if (chop_buffer_size (&block->buffer) < KEY_BLOCK_HEADER_SIZE)
    {
      chop_log_printf (block->log, "meta-data block is too small");
      return CHOP_INDEXER_ERROR;
    }

  magic[0] = *(buffer++);
  magic[1] = *(buffer++);

  if ((magic[0] != KEY_BLOCK_MAGIC_1) || (magic[1] != KEY_BLOCK_MAGIC_2))
    {
      chop_log_printf (block->log,
		       "invalid key block magic numbers: 0x%02x%02x",
		       magic[0], magic[1]);
      block->current_child_number = block->key_count = block->depth = 0;
      return CHOP_INDEXER_ERROR;
    }
  else
    {
      block->key_count  = ((size_t)*(buffer++));
      block->key_count |= ((size_t)*(buffer++)) <<  8;
      block->key_count |= ((size_t)*(buffer++)) << 16;
      block->key_count |= ((size_t)*(buffer++)) << 24;

      block->depth  = ((size_t)*(buffer++));
      block->depth |= ((size_t)*(buffer++)) << 8;
    }

  block->offset = KEY_BLOCK_HEADER_SIZE;
  block->current_child_number = 0;

  chop_log_printf (block->log, "decoded block: keys=%u, depth=%u",
		   block->key_count, block->depth);

  return 0;
}

static inline errcode_t
chop_decoded_block_new (decoded_block_t **block, chop_log_t *log)
{
  errcode_t err;

  *block = chop_malloc (sizeof (decoded_block_t),
			&chop_tree_stream_class);
  if (!*block)
    return ENOMEM;

  (*block)->current_child = (*block)->parent = NULL;
  (*block)->current_child_number = 0;

  err = chop_buffer_init (&(*block)->buffer, 0);
  if (err)
    return err;

  (*block)->log = log;

  return 0;
}

/* Return non-zero if children of BLOCK are key blocks.  */
#define CHILDREN_ARE_KEY_BLOCKS(_block) \
  (((_block)->is_key_block && ((_block)->depth > 0)) ? 1 : 0)

/* Fetch block with index INDEX from STORE and update BLOCK accordingly.
   PARENT, if not NULL, is assumed to be the parent of the block being
   fetched.  */
static errcode_t
chop_decoded_block_fetch (chop_block_store_t *store,
			  const chop_index_handle_t *index,
			  chop_block_fetcher_t *fetcher,
			  decoded_block_t *parent, decoded_block_t *block)
{
  errcode_t err;
  size_t block_size;
  int is_key_block;

  if (parent)
    is_key_block = CHILDREN_ARE_KEY_BLOCKS (parent);
  else
    is_key_block = 1;

  chop_buffer_clear (&block->buffer);
  err = chop_block_fetcher_fetch (fetcher, index, store,
				  &block->buffer, &block_size);
  if (err)
    {
      /* chop_buffer_return (&block->buffer); */
      return err;
    }

  block->is_key_block = is_key_block;
  block->offset = 0;
  if (is_key_block)
    err = chop_decoded_block_decode_header (block);

  block->parent = parent;

  /* Keep the CURRENT_CHILD pointer as is in order to be able to reuse
     `decoded_block_t' objects accross calls to
     CHOP_DECODED_BLOCK_NEXT_CHILD.  */

  return err;
}

/* Update BLOCK's CURRENT_CHILD pointer to its next block if any.
   CHOP_STREAM_END is returned if BLOCK and his parents don't have any
   further block.  This propagates the request recursively to BLOCK's
   parents.  However, no new block object is allocated: they are simply
   reused.  */
static errcode_t
chop_decoded_block_next_child (decoded_block_t *block,
			       chop_block_fetcher_t *fetcher,
			       chop_block_store_t *metadata_store,
			       chop_block_store_t *data_store)
{
  errcode_t err;
  chop_log_t *log = block->log;

 start:
  if ((block->current_child_number >= block->key_count)
      || (block->offset >= chop_buffer_size (&block->buffer)))
    {
      /* We're done with this block so let's reuse it with the next block */
      if (!block->parent)
	{
	  /* BLOCK is the top-level key block and there's nothing left in
	     it.  */
	  chop_log_printf (log, "root block: end of stream (offset: %u/%u)",
			   block->offset, chop_buffer_size (&block->buffer));
	  return CHOP_STREAM_END;
	}
      else
	{
	  /* Propagate this request to BLOCK's parent */
	  decoded_block_t *parent = block->parent;
	  err = chop_decoded_block_next_child (block->parent,
					       fetcher,
					       metadata_store, data_store);
	  if (err)
	    return err;

	  /* At this point, the contents of BLOCK have changed, i.e. BLOCK
	     has been re-used to represent the new child of its parent.  The
	     CURRENT_CHILD pointer of PARENT should still point to BLOCK and
	     vice-versa.  */
	  assert (parent->current_child == block);
	  assert (block->parent == parent);

	  /* Restart the procedure with the new content of BLOCK.  */
	  goto start;
	}
    }
  else
    {
      /* Read a block index and update BLOCK's CURRENT_CHILD pointer.  */
      size_t available, serialized_size;
      chop_block_store_t *the_store;
      const chop_class_t *index_class;
      chop_index_handle_t *index;
      char *pos;

      pos = (char *)chop_buffer_content (&block->buffer) + block->offset;
      available = chop_buffer_size (&block->buffer) - block->offset;

      index_class = chop_block_fetcher_index_handle_class (fetcher);
      index = chop_class_alloca_instance (index_class);
      err = chop_object_deserialize ((chop_object_t *)index, index_class,
				     CHOP_SERIAL_BINARY,
				     pos, available,
				     &serialized_size);
      if (err)
	{
	  chop_log_printf (block->log, "failed to binary-deserialize "
			   "index handle");
	  return err;
	}

      block->offset += serialized_size;
      if (!block->current_child)
	{
	  err = chop_decoded_block_new (&block->current_child, block->log);
	  if (err)
	    goto finish;
	}

      /* Pick up the right block store */
      if (CHILDREN_ARE_KEY_BLOCKS (block))
	the_store = metadata_store;
      else
	the_store = data_store;

      chop_log_printf (log, "fetching new child block (current depth: %u)",
		       block->is_key_block ? block->depth : 0);
      err = chop_decoded_block_fetch (the_store, index,
				      fetcher,
				      block, block->current_child);
      if (err)
	goto finish;

      block->current_child_number++;

    finish:
      chop_object_destroy ((chop_object_t *)index);
    }

  return err;
}


/* Read at most SIZE bytes from BLOCK.  If BLOCK is a key block, pass this
   request to its current child block.  If BLOCK is a data block, read as
   much as possible from it and ask its parents to jump to the next data
   block (without allocating new block objects) if BLOCK has been read
   entirely.  */
static errcode_t
chop_decoded_block_read (decoded_block_t *block,
			 chop_block_store_t *metadata_store,
			 chop_block_store_t *data_store,
			 chop_block_fetcher_t *fetcher,
			 char *buffer, size_t size, size_t *read)
{
  errcode_t err = 0;

  if (block->is_key_block)
    {
      decoded_block_t *child;
      if (!block->current_child)
	{
	  /* Fetch this block's first child */
	  err = chop_decoded_block_next_child (block, fetcher,
					       metadata_store, data_store);
	  if (err)
	    return err;

	  block->current_child->current_child = NULL;
	}

      /* Pass this request to BLOCK's current child */
      child = block->current_child;
      err = chop_decoded_block_read (child, metadata_store, data_store,
				     fetcher, buffer, size, read);
    }
  else
    {
      /* BLOCK is a data block */
      assert (block->parent);

      *read = 0;
      while (*read < size)
	{
	  const char *block_buf;
	  size_t amount, available, to_read = size - *read;

	  if (block->offset >= chop_buffer_size (&block->buffer))
	    {
	      /* We're done with this block.  Re-use it for the next block.  */
	      err = chop_decoded_block_next_child (block->parent,
						   fetcher,
						   metadata_store, data_store);
	      if (err)
		{
		  if (err == CHOP_STREAM_END)
		    {
		      if (*read == 0)
			/* Only return CHOP_STREAM_END if really nothing was
			   read; otherwise, wait till the next request.  */
			return err;
		    }
		  else
		    return err;
		}
	    }

	  block_buf = chop_buffer_content (&block->buffer) + block->offset;
	  available = chop_buffer_size (&block->buffer) - block->offset;
	  amount = (available > to_read) ? to_read : available;

	  memcpy (buffer + *read, block_buf, amount);
	  block->offset += amount;
	  *read += amount;

	  if (err == CHOP_STREAM_END)
	    {
	      /* We'll return CHOP_STREAM_END next time */
	      err = 0;
	      break;
	    }
	}
    }

  return err;
}

/* Read at most SIZE bytes from TREE's underlying metadata block store.  On
   success, zero is returned and READ is set to the number of bytes actually
   read.  CHOP_STREAM_END is returned on end-of-stream.  */
static errcode_t
chop_decoded_block_tree_read (decoded_block_tree_t *tree,
			      char *buffer, size_t size,
			      size_t *read)
{
  errcode_t err;

  chop_log_printf (tree->log, "reading %u bytes", size);
  if (!tree->top_level)
    {
      /* Fetch the top-level key block (or "inode").  */
      err = chop_decoded_block_new (&tree->top_level, tree->log);
      if (err)
	return err;

      err = chop_decoded_block_fetch (tree->metadata_store, tree->index,
				      tree->fetcher,
				      NULL /* No parent block */,
				      tree->top_level);
      if (err)
	return err;
    }

  *read = 0;
  err = chop_decoded_block_read (tree->top_level,
				 tree->metadata_store, tree->data_store,
				 tree->fetcher,
				 buffer, size, read);
  tree->current_offset += *read;

  return err;
}


/* The stream `read' method for hash tree streams.  */
static errcode_t
tree_stream_read (chop_stream_t *stream, char *buffer, size_t size,
		       size_t *read)
{
  chop_tree_stream_t *tstream = (chop_tree_stream_t *)stream;

  assert (tstream->tree.index);
  assert (tstream->tree.fetcher);

  return (chop_decoded_block_tree_read (&tstream->tree,
					buffer, size, read));
}

/* The stream `close' method for hash tree streams.  */
static void
tree_stream_close (chop_stream_t *stream)
{
  chop_tree_stream_t *tstream = (chop_tree_stream_t *)stream;

  chop_decoded_block_tree_free (&tstream->tree);
}


