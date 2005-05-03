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


/* Define CHOP_INDEXER_CLASS.  */
CHOP_DEFINE_RT_CLASS (indexer, object,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);



/* Declare and define the `chop_hash_tree_indexer_t' class and its run-time
   representation CHOP_HASH_TREE_INDEXER_CLASS.  */
CHOP_DECLARE_RT_CLASS (hash_tree_indexer, indexer,

		       /* Hash method and message digest size (in bytes) */
		       chop_hash_method_t hash_method;
		       int                gcrypt_hash_method;
		       size_t             key_size;
		       size_t             keys_per_block;

		       /* For debugging purposes */
		       chop_log_t         log;);

CHOP_DEFINE_RT_CLASS (hash_tree_indexer, indexer,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);



/* Internal class declarations.  */

/* Declare `chop_chk_index_handle_t' that inherits from the
   `chop_index_handle_t' class.  */
CHOP_DECLARE_RT_CLASS (chk_index_handle, index_handle,
		       char hash_key[30];
		       size_t hash_size;
		       char block_id[30];
		       size_t block_id_size;);

/* Declare `chop_hash_index_handle' that inherits from the
   `chop_index_handle_t' class.  */
CHOP_DECLARE_RT_CLASS (hash_index_handle, index_handle,
		       chop_hash_method_t hash_method;
		       size_t hash_size;
		       char hash[30];);



/* Internal exported class definitions.  */

/* Serialize OBJECT (which is assumed to be a `chop_index_handle_t' object)
   into BUFFER according to METHOD.  */
static errcode_t
hash_index_handle_serialize (const chop_object_t *object,
			     chop_serial_method_t method,
			     chop_buffer_t *buffer)
{
  chop_hash_index_handle_t *handle =
    (chop_hash_index_handle_t *)object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	/* Return something like
	   "SHA1:eabe1ca1f3c7ca148ce2fe5954f52ef9a0f0082a".  */
	char *hex;
	size_t hash_method_len;
	const char *hash_method = chop_hash_method_name (handle->hash_method);

	hash_method_len = strlen (hash_method);
	hex = alloca (hash_method_len + 1 + (handle->hash_size * 2) + 1);
	memcpy (hex, hash_method, hash_method_len);
	hex[hash_method_len] = ':';
	chop_buffer_to_hex_string (handle->hash, handle->hash_size,
				   &hex[hash_method_len + 1]);

	chop_buffer_push (buffer, hex, strlen (hex) + 1);
	return 0;
      }

    case CHOP_SERIAL_BINARY:
      {
	char hash_method = (char)handle->hash_method;
	chop_buffer_push (buffer, &hash_method, 1);
	chop_buffer_append (buffer, handle->hash, handle->hash_size);
	return 0;
      }

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
hash_index_handle_deserialize (const char *buffer, size_t size,
			       chop_serial_method_t method,
			       chop_object_t *object)
{
  errcode_t err;
  chop_hash_index_handle_t *handle =
    (chop_hash_index_handle_t *)object;

  switch (method)
    {
      case CHOP_SERIAL_ASCII:
	{
	  char *colon;
	  char *hash_name;
	  size_t hash_name_len;

	  if (size < 5)
	    return CHOP_DESERIAL_TOO_SHORT;

	  colon = strchr (buffer, ':');
	  if (!colon)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  hash_name_len = colon - buffer;
	  hash_name = alloca (hash_name_len + 1);
	  if (!hash_name)
	    return ENOMEM;

	  memcpy (hash_name, buffer, hash_name_len);
	  hash_name[hash_name_len] = '\0';
	  err = chop_hash_method_lookup (hash_name, &handle->hash_method);
	  if (err)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  handle->hash_size = chop_hash_size (handle->hash_method);

	  if (size < hash_name_len)
	    return CHOP_DESERIAL_TOO_SHORT;

	  chop_hex_string_to_buffer (buffer + hash_name_len + 1,
				     size - hash_name_len -1,
				     handle->hash);

	  return 0;
	}

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

CHOP_DEFINE_RT_CLASS (index_handle, object,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);

CHOP_DEFINE_RT_CLASS (chk_index_handle, index_handle,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);

CHOP_DEFINE_RT_CLASS (hash_index_handle, index_handle,
		      NULL, NULL, /* No constructor/destructor */
		      hash_index_handle_serialize,
		      hash_index_handle_deserialize);


/* Internal data structures.  */

/* This represents a key block or "i-node", i.e. a block whose content is a
   vector of keys of data blocks or key blocks.  */
typedef struct key_block
{
  /* Pointer to the key block that contains a pointer to it */
  struct key_block *parent;

  /* KEY_COUNT concatenated block keys, each of which should be KEY_SIZE
     long.  This contains a header which is KEY_BLOCK_HEADER_SIZE bytes
     long.  */
#define KEY_BLOCK_HEADER_SIZE  (8)
  char *keys;
  size_t key_count;
  size_t key_size;

  /* Depth of this block's subtree */
  size_t depth;

  /* Hash method for key block keys */
  int gcry_hash_method;

  /* Pointer to the indexer's log (for debugging purposes) */
  chop_log_t *log;
} key_block_t;

/* A tree of key blocks.  */
typedef struct key_block_tree
{
  /* The current bottom-most key block */
  key_block_t *current;

  /* Number of keys per key block */
  size_t keys_per_block;

  /* Key size and algorithm */
  size_t key_size;
  int gcry_hash_method;

  /* Meta-data block store: the block store where key blocks are flushed */
  chop_block_store_t *metadata_store;

  /* Pointer to the indexer's log (for debugging purposes) */
  chop_log_t *log;
} key_block_tree_t;


/* Initialize key block tree TREE.  */
static inline void
chop_block_tree_init (key_block_tree_t *tree, size_t keys_per_block,
		      int key_method, size_t key_size,
		      chop_block_store_t *store, chop_log_t *log)
{
  tree->current = NULL;
  tree->keys_per_block = keys_per_block;
  tree->key_size = key_size;
  tree->gcry_hash_method = key_method;
  tree->metadata_store = store;
  tree->log = log;
}

/* Fill in the KEYS field of BLOCK with a header.  This should be called by
   CHOP_KEY_BLOCK_FLUSH, right before BLOCK is actually written.  */
static inline void
chop_key_block_fill_header (key_block_t *block)
{
  size_t count = block->key_count;
  size_t depth = block->depth;

  block->keys[0] = (count & 0xff);  count >>= 8;
  block->keys[1] = (count & 0xff);  count >>= 8;
  block->keys[2] = (count & 0xff);  count >>= 8;
  block->keys[3] = (count & 0xff);  count >>= 8;
  assert (!count);

  block->keys[4] = (depth & 0xff);  depth >>= 8;
  block->keys[5] = (depth & 0xff);  depth >>= 8;
  assert (!depth);
}

/* Flush BLOCK to METADATA and return its key in KEY.  The memory used by
   BLOCK can now be reused.  */
static errcode_t
chop_key_block_flush (key_block_t *block, chop_block_store_t *metadata,
		      chop_block_key_t *key)
{
  errcode_t err;
  size_t block_size;
  char *hash = (char *)chop_block_key_buffer (key);

  chop_log_printf (block->log,
		   "key_block_flush: block %p with %u keys being flushed "
		   "to store %p\n",
		   block, block->key_count, metadata);

  chop_key_block_fill_header (block);

  /* FIXME:  Maybe we should pad BLOCK->KEYS with zero and create fixed-size
     blocks.  */
  block_size = KEY_BLOCK_HEADER_SIZE + (block->key_count * block->key_size);
  gcry_md_hash_buffer (block->gcry_hash_method, hash,
		       block->keys, block_size);

  /* Write this key block to backing store */
  err = chop_store_write_block (metadata, key, block->keys, block_size);

  return 0;
}

/* Allocate and initialize a new key block and return it in BLOCK.  */
static inline errcode_t
chop_key_block_new (size_t keys_per_block,
		    int hash_method, size_t key_size, chop_log_t *log,
		    key_block_t **block)
{
  *block = (key_block_t *)malloc (sizeof (key_block_t));
  if (!*block)
    return ENOMEM;

  (*block)->keys = malloc (KEY_BLOCK_HEADER_SIZE +
			   (keys_per_block * key_size));
  if (!(*block)->keys)
    {
      free (*block);
      return ENOMEM;
    }

  (*block)->parent = NULL;
  (*block)->depth = 0;
  (*block)->key_count = 0;
  (*block)->key_size = key_size;
  (*block)->gcry_hash_method = hash_method;
  (*block)->log = log;

  return 0;
}

/* Append KEY to BLOCK which can contain at most KEYS_PER_BLOCK block keys.
   When full, BLOCK is written to METADATA and its contents are reset.  */
static errcode_t
chop_key_block_add_key (key_block_t *block, size_t keys_per_block,
			chop_block_store_t *metadata,
			const chop_block_key_t *key)
{
  errcode_t err;
  size_t offset;

  if (block->key_count + 1 >= keys_per_block)
    {
      /* This key block is full:
	 1.  flush it;
	 2.  add its key to its parent key block;
	 3.  reset it and append KEY to it.  */
      key_block_t *parent = block->parent;
      chop_block_key_t block_key;
      char *key_buf = alloca (block->key_size);
      if (!key_buf)
	return ENOMEM;

      chop_block_key_init (&block_key, key_buf, block->key_size, NULL, NULL);
      chop_key_block_flush (block, metadata, &block_key);

      if (!parent)
	{
	  /* BLOCK is orphan: create him a parent key block.  */
	  err = chop_key_block_new (keys_per_block,
				    block->gcry_hash_method, block->key_size,
				    block->log, &parent);
	  if (err)
	    return err;

	  parent->depth = block->depth + 1;
	  block->parent = parent;
	}

      err = chop_key_block_add_key (parent, keys_per_block, metadata,
				    &block_key);
      if (err)
	return err;

      block->key_count = 0;
    }

  /* Append the content of KEY.  */
  offset = (block->key_size * block->key_count) + KEY_BLOCK_HEADER_SIZE;
  assert (chop_block_key_size (key) == block->key_size);
  memcpy (&block->keys[offset],
	  chop_block_key_buffer (key),
	  block->key_size);
  block->key_count++;

  return 0;
}

/* Append KEY to TREE.  */
static errcode_t
chop_block_tree_add_key (key_block_tree_t *tree,
			 const chop_block_key_t *key)
{
  errcode_t err = 0;
  key_block_t *current;

  if (!tree->current)
    {
      /* Allocate a new block tree */
      err = chop_key_block_new (tree->keys_per_block,
				tree->gcry_hash_method, tree->key_size,
				tree->log, &tree->current);
      if (err)
	return err;

      tree->current->depth = 0;
    }

  current = tree->current;

  err = chop_key_block_add_key (current, tree->keys_per_block,
				tree->metadata_store, key);

  return err;
}


/* Flush all the pending key blocks of TREE and return the key of the
   top-level key block in ROOT_KEY.  */
static errcode_t
chop_block_tree_flush (key_block_tree_t *tree, chop_block_key_t *root_key)
{
  errcode_t err = 0;
  key_block_t *block;
  size_t depth = 0, last_depth;

  for (block = tree->current;
       block != NULL;
       block = block->parent)
    {
      depth++;
      last_depth = block->depth;
      err = chop_key_block_flush (block, tree->metadata_store, root_key);
      if (err)
	break;

      if (block->parent)
	/* Add the key of the newly flushed block to its parent */
	err = chop_key_block_add_key (block->parent, tree->keys_per_block,
				      tree->metadata_store, root_key);
    }

  chop_log_printf (tree->log,
		   "block_tree_flush: hash tree depth: %u\n", depth);
  if (depth)
    assert (last_depth == depth - 1);

  return err;
}

/* Free the memory associated with TREE.  */
static void
chop_block_tree_free (key_block_tree_t *tree)
{
  key_block_t *block;

  for (block = tree->current;
       block != NULL;
       block = block->parent)
    {
      /* FIXME:  Do something!  */
    }
}


/* Implementation of the indexer interface.  */

static errcode_t
chop_hash_tree_index_blocks (chop_indexer_t *indexer,
			     chop_chopper_t *input,
			     chop_block_store_t *output,
			     chop_block_store_t *metadata,
			     chop_index_handle_t *handle);

static errcode_t
chop_hash_tree_fetch_stream (struct chop_indexer *,
			     const chop_index_handle_t *,
			     chop_block_store_t *,
			     chop_block_store_t *,
			     chop_stream_t *);

static errcode_t hash_tree_stream_read (chop_stream_t *, char *,
					size_t, size_t *);

static void hash_tree_stream_close (chop_stream_t *);

extern const chop_class_t chop_hash_tree_stream_class;


errcode_t
chop_hash_tree_indexer_open (chop_hash_method_t content_hash_method,
			     chop_hash_method_t key_hash_method,
			     size_t keys_per_block,
			     chop_indexer_t *indexer)
{
  errcode_t err;
  chop_hash_tree_indexer_t *htree;

  if (content_hash_method != CHOP_HASH_NONE)
    /* FIXME:  Implement content-hash keys.  */
    return CHOP_ERR_NOT_IMPL;

  chop_object_initialize ((chop_object_t *)indexer,
			  &chop_hash_tree_indexer_class);

  htree = (chop_hash_tree_indexer_t *)indexer;
  err = chop_log_init ("hash-tree", &htree->log);
  if (err)
    return err;

  htree->indexer.index_blocks = chop_hash_tree_index_blocks;
  htree->indexer.fetch_stream = chop_hash_tree_fetch_stream;
  htree->indexer.stream_class = &chop_hash_tree_stream_class;
  htree->indexer.index_handle_class = &chop_hash_index_handle_class;

  htree->hash_method = key_hash_method;
  htree->gcrypt_hash_method = chop_hash_method_gcrypt_name (key_hash_method);
  htree->key_size = chop_hash_size (key_hash_method);
  htree->keys_per_block = keys_per_block;

#if 0
#define obstack_chunk_alloc malloc
#define obstack_chunk_free  free
  obstack_alloc_failed_handler = &chop_obstack_alloc_failed_handler;
  obstack_init (&htree->key_obstack);
#endif

  return 0;
}

chop_log_t *
chop_hash_tree_indexer_log (chop_indexer_t *indexer)
{
  chop_hash_tree_indexer_t *htree;

  /* Gratuitous overhead.  */
  if (!chop_object_is_a ((chop_object_t *)indexer,
			 &chop_hash_tree_indexer_class))
    return NULL;

  htree = (chop_hash_tree_indexer_t *)indexer;

  return (&htree->log);
}

static errcode_t
chop_hash_tree_index_block (chop_hash_tree_indexer_t *htree,
			    chop_block_store_t *store,
			    const char *buffer,
			    size_t size,
			    chop_block_key_t *key)
{
  errcode_t err;
  char *hash = (char *)chop_block_key_buffer (key);

  /* Compute this buffer's key */
  gcry_md_hash_buffer (htree->gcrypt_hash_method, hash,
		       buffer, size);

  /* Store this block in the output block store */
  err = chop_store_write_block (store, key, buffer, size);

  return err;
}

static errcode_t
chop_hash_tree_index_blocks (chop_indexer_t *indexer,
			     chop_chopper_t *input,
			     chop_block_store_t *output,
			     chop_block_store_t *metadata,
			     chop_index_handle_t *handle)
{
  errcode_t err = 0;
  chop_hash_tree_indexer_t *htree = (chop_hash_tree_indexer_t *)indexer;
  size_t amount;
  chop_buffer_t buffer;
  key_block_tree_t tree;
  chop_block_key_t key;
  char *key_buf = (char *)alloca (htree->key_size);


  chop_block_tree_init (&tree, htree->keys_per_block,
			htree->gcrypt_hash_method, htree->key_size,
			metadata, &htree->log);
  chop_block_key_init (&key, key_buf, htree->key_size, NULL, NULL);

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

      /* Store this block and get its key */
      err = chop_hash_tree_index_block ((chop_hash_tree_indexer_t *)indexer,
					output,
					chop_buffer_content (&buffer),
					chop_buffer_size (&buffer),
					&key);
      if (err)
	break;

      /* Add this block key to our block key tree */
      chop_block_tree_add_key (&tree, &key);
    }

  if (err == CHOP_STREAM_END)
    {
      /* Initialize the (serializable) handle object.  */
      size_t key_size = htree->key_size;
      chop_hash_index_handle_t *hhandle = (chop_hash_index_handle_t *)handle;
      chop_object_initialize ((chop_object_t *)handle,
			      &chop_hash_index_handle_class);
      hhandle->hash_size = key_size;
      hhandle->hash_method = htree->hash_method;

      /* Flush the key block tree and get its key */
      chop_block_tree_flush (&tree, &key);

      /* Copy the top-level key into the handle */
      memcpy (hhandle->hash, chop_block_key_buffer (&key), key_size);
    }

  /* Free memory associated with TREE */
  chop_block_tree_free (&tree);

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

  /* If this is a key block, this is the number of children it has.  */
  size_t key_count;

  /* If this is a key block, this points to its current child.  We don't keep
     all blocks in memory.  */
  struct decoded_block *current_child;

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
  chop_index_handle_t *handle;
  decoded_block_t *top_level;
  size_t current_offset;
  size_t key_size;
  chop_log_t *log;
} decoded_block_tree_t;



/* Hash tree stream objects are returned by CHOP_INDEXER_FETCH_STREAM when
   reading from a hash-tree-indexed stream.  */
CHOP_DECLARE_RT_CLASS (hash_tree_stream, stream,
		       decoded_block_tree_t tree;
		       chop_log_t log;);

static inline void
chop_decoded_block_tree_init (decoded_block_tree_t *tree,
			      chop_block_store_t *data_store,
			      chop_block_store_t *metadata_store,
			      const chop_index_handle_t *handle,
			      size_t key_size,
			      chop_log_t *log)
{
  const chop_class_t *handle_class =
    chop_object_get_class ((chop_object_t *)handle);

  tree->data_store = data_store;
  tree->metadata_store = metadata_store;
  tree->handle = malloc (chop_class_instance_size (handle_class));
  assert (tree->handle);  /* XXX: This is bad */
  memcpy (tree->handle, handle, chop_class_instance_size (handle_class));
  tree->top_level = NULL;
  tree->current_offset = 0;
  tree->key_size = key_size;

  tree->log = log;
}

/* Constructor.  */
static void
hash_tree_stream_init (chop_object_t *object, const chop_class_t *class)
{
  chop_hash_tree_stream_t *stream = (chop_hash_tree_stream_t *)object;

  stream->stream.read = hash_tree_stream_read;
  stream->stream.close = hash_tree_stream_close;

  stream->tree.handle = NULL;

  chop_log_init ("hash-tree-stream", &stream->log);
}


CHOP_DEFINE_RT_CLASS (hash_tree_stream, object,
		      hash_tree_stream_init, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);


/* The entry point.  On success, return zero and set OUTPUT to a stream
   representing the contents of the hash-tree-encoded stream pointed to by
   HANDLE.  */
static errcode_t
chop_hash_tree_fetch_stream (struct chop_indexer *indexer,
			     const chop_index_handle_t *handle,
			     chop_block_store_t *input,
			     chop_block_store_t *metadata,
			     chop_stream_t *output)
{
  errcode_t err;
  chop_hash_tree_stream_t *tstream;
  const chop_class_t *handle_class;
  chop_index_handle_t *handle_copy;
  chop_hash_tree_indexer_t *htree = (chop_hash_tree_indexer_t *)indexer;

  /* OUTPUT is assumed to be as large as `chop_hash_tree_stream_t' */
  chop_object_initialize ((chop_object_t *)output,
			  &chop_hash_tree_stream_class);
  tstream = (chop_hash_tree_stream_t *)output;

  /* Copy HANDLE */
  handle_class = chop_object_get_class ((chop_object_t *)handle);
  handle_copy = malloc (chop_class_instance_size (handle_class));
  if (!handle_copy)
    return ENOMEM;
  memcpy (handle_copy, handle, chop_class_instance_size (handle_class));

  chop_decoded_block_tree_init (&tstream->tree, input, metadata,
				handle, htree->key_size, &htree->log);

  /* Initialize the log (for debugging purposes) */
  err = chop_log_init ("hash-tree-stream", &tstream->log);
  if (err)
    return err;

  chop_log_mimic (&tstream->log, &htree->log, 0);

  chop_log_printf (&htree->log, "fetching stream");

  return err;
}



/* Decode BLOCK's header (which was created by CHOP_KEY_BLOCK_FILL_HEADER).
   BLOCK is assumed to be a key block.  This functions sets BLOCK's KEY_COUNT
   and DEPTH fields.  */
static inline void
chop_decoded_block_decode_header (decoded_block_t *block)
{
  const char *buffer = chop_buffer_content (&block->buffer);
  assert (block->is_key_block);

  block->key_count  = ((size_t)*(buffer++));
  block->key_count |= ((size_t)*(buffer++)) <<  8;
  block->key_count |= ((size_t)*(buffer++)) << 16;
  block->key_count |= ((size_t)*(buffer++)) << 24;

  block->depth  = ((size_t)*(buffer++));
  block->depth |= ((size_t)*(buffer++)) << 8;

  block->offset = KEY_BLOCK_HEADER_SIZE;

  chop_log_printf (block->log, "decoded block: keys=%u, depth=%u",
		   block->key_count, block->depth);
}

static inline errcode_t
chop_decoded_block_new (decoded_block_t **block, chop_log_t *log)
{
  errcode_t err;

  *block = malloc (sizeof (decoded_block_t));
  if (!*block)
    return ENOMEM;

  (*block)->current_child = (*block)->parent = NULL;
  err = chop_buffer_init (&(*block)->buffer, 0);
  if (err)
    return err;

  (*block)->log = log;

  return 0;
}

/* Return non-zero if children of BLOCK are key blocks.  */
#define CHILDREN_ARE_KEY_BLOCKS(_block) \
  (((_block)->is_key_block && ((_block)->depth > 0)) ? 1 : 0)

/* Fetch block with key KEY from STORE and update BLOCK accordingly.  PARENT,
   if not NULL, is assumed to be the parent of the block being fetched.  */
static inline errcode_t
chop_decoded_block_fetch (chop_block_store_t *store,
			  const chop_block_key_t *key,
			  decoded_block_t *parent, decoded_block_t *block)
{
  errcode_t err;
  size_t read;
  int is_key_block;

  if (parent)
    is_key_block = CHILDREN_ARE_KEY_BLOCKS (parent);
  else
    is_key_block = 1;

  err = chop_store_read_block (store, key, &block->buffer, &read);
  if ((err) && (err != ENOMEM))
    return err;  /* FIXME:  Free things */

  block->is_key_block = is_key_block;
  block->offset = 0;
  if (is_key_block)
    chop_decoded_block_decode_header (block);

  block->parent = parent;

  /* Keep the CURRENT_CHILD pointer as is in order to be able to reuse
     `decoded_block_t' objects accross calls to
     CHOP_DECODED_BLOCK_NEXT_CHILD.  */
/*   block->current_child = NULL; */

  return 0;
}

/* Update BLOCK's CURRENT_CHILD pointer to its next block if any.
   CHOP_STREAM_END is returned if BLOCK and his parents don't have any
   further block.  This propagates the request recursively to BLOCK's
   parents.  However, no new block object is allocated: they are simply
   reused.  */
static errcode_t
chop_decoded_block_next_child (decoded_block_t *block, size_t key_size,
			       chop_block_store_t *metadata_store,
			       chop_block_store_t *data_store)
{
  errcode_t err;
  chop_log_t *log = block->log;

 start:
  if (block->offset >= chop_buffer_size (&block->buffer))
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
	  err = chop_decoded_block_next_child (block->parent, key_size,
					       metadata_store, data_store);
	  if (err)
	    return err;

	  /* At this point, the contents of BLOCK have changed, i.e. BLOCK
	     has been re-used to represent the new child of its parent.  The
	     CURRENT_CHILD pointer of PARENT should still point to BLOCK and
	     vice-versa.  */
	  assert (parent->current_child == block);
	  assert (block->parent == parent);
	  block = parent->current_child;

	  /* Restart the procedure with the new content of BLOCK.  */
	  goto start;
	}
    }
  else
    {
      /* Update BLOCK's CURRENT_CHILD pointer */
      chop_block_key_t key;
      chop_block_store_t *the_store;
      char *pos, *key_buf = alloca (key_size);

      pos = (char *)chop_buffer_content (&block->buffer) + block->offset;

      assert (block->offset + key_size <= chop_buffer_size (&block->buffer));
      memcpy (key_buf, pos, key_size);
      block->offset += key_size;
      chop_block_key_init (&key, key_buf, key_size, NULL, NULL);

      if (!block->current_child)
	{
	  err = chop_decoded_block_new (&block->current_child, block->log);
	  if (err)
	    return err;
	}

      /* Pick up the right block store */
      if (CHILDREN_ARE_KEY_BLOCKS (block))
	the_store = metadata_store;
      else
	the_store = data_store;

      chop_log_printf (log, "fetching new child block (current depth: %u)",
		       block->is_key_block ? block->depth : 0);
      err = chop_decoded_block_fetch (the_store, &key,
				      block, block->current_child);
      if (err)
	return err;
    }

  return 0;
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
			 size_t key_size,
			 char *buffer, size_t size, size_t *read)
{
  errcode_t err = 0;

  if (block->is_key_block)
    {
      decoded_block_t *child;
      if (!block->current_child)
	{
	  /* Fetch this block's first child */
	  err = chop_decoded_block_next_child (block, key_size,
					       metadata_store, data_store);
	  if (err)
	    return err;

	  block->current_child->current_child = NULL;
	}

      /* Pass this request to BLOCK's current child */
      child = block->current_child;
      err = chop_decoded_block_read (child, metadata_store, data_store,
				     key_size, buffer, size, read);
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
	      /* We're donc with this block.  Re-use it for the next block.  */
	      err = chop_decoded_block_next_child (block->parent, key_size,
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
      chop_hash_index_handle_t *hhandle;
      chop_block_key_t key;
      char *key_buf = alloca (tree->key_size);

      err = chop_decoded_block_new (&tree->top_level, tree->log);
      if (err)
	return err;

      assert (chop_object_get_class ((chop_object_t *)tree->handle) ==
	      &chop_hash_index_handle_class);  /* FIXME */

      hhandle = (chop_hash_index_handle_t *)tree->handle;
      memcpy (key_buf, hhandle->hash, tree->key_size);
      chop_block_key_init (&key, key_buf, tree->key_size, NULL, NULL);

      err = chop_decoded_block_fetch (tree->metadata_store, &key,
				      NULL /* No parent block */,
				      tree->top_level);
      if (err)
	return err;
    }

  *read = 0;
  err = chop_decoded_block_read (tree->top_level,
				 tree->metadata_store, tree->data_store,
				 tree->key_size,
				 buffer, size, read);
  tree->current_offset += *read;

  return err;
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
      free (block);
    }

  tree->top_level = NULL;
}


/* The stream `read' method for hash tree streams.  */
static errcode_t
hash_tree_stream_read (chop_stream_t *stream, char *buffer, size_t size,
		       size_t *read)
{
  chop_hash_tree_stream_t *tstream = (chop_hash_tree_stream_t *)stream;

  assert (tstream->tree.handle);

  return (chop_decoded_block_tree_read (&tstream->tree, buffer, size, read));
}

/* The stream `close' method for hash tree streams.  */
static void
hash_tree_stream_close (chop_stream_t *stream)
{
  chop_hash_tree_stream_t *tstream = (chop_hash_tree_stream_t *)stream;

  chop_decoded_block_tree_free (&tstream->tree);
}

