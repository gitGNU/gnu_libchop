#include <chop/chop.h>
#include <chop/indexer-hash-tree.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>

/* Glibc's obstacks */
#include <obstack.h>

/* libgcrypt */
#include <gcrypt.h>



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
	char *hex = alloca (handle->hash_size * 2 + 1);
	chop_buffer_to_hex_string (handle->hash, handle->hash_size,
				   hex);
	chop_buffer_push (buffer, hex, strlen (hex));
	return 0;
      }

    case CHOP_SERIAL_BINARY:
      chop_buffer_push (buffer, handle->hash, handle->hash_size);
      return 0;

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return CHOP_ERR_NOT_IMPL;
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
		      NULL /* No deserializer (FIXME) */);


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
} key_block_tree_t;


/* Initialize key block tree TREE.  */
static inline void
chop_block_tree_init (key_block_tree_t *tree, size_t keys_per_block,
		      int key_method, size_t key_size,
		      chop_block_store_t *store)
{
  tree->current = NULL;
  tree->keys_per_block = keys_per_block;
  tree->key_size = key_size;
  tree->gcry_hash_method = key_method;
  tree->metadata_store = store;
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

  fprintf (stderr, "key_block_flush: block %p with %u keys being flushed "
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
		    int hash_method, size_t key_size,
		    key_block_t **block)
{
  *block = (key_block_t *)malloc (sizeof (key_block_t));
  if (!*block)
    return ENOMEM;

  (*block)->keys = malloc (KEY_BLOCK_HEADER_SIZE +
			   (keys_per_block * key_size));
  if (!(*block)->keys)
    return ENOMEM;

  (*block)->parent = NULL;
  (*block)->depth = 0;
  (*block)->key_count = 0;
  (*block)->key_size = key_size;
  (*block)->gcry_hash_method = hash_method;

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
				    &parent);
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
				&tree->current);
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
    }

  fprintf (stderr, "block_tree_flush: hash tree depth: %u\n", depth);
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


static void
chop_obstack_alloc_failed_handler (void)
{
  exit (77);
}

errcode_t
chop_hash_tree_indexer_open (chop_hash_method_t content_hash_method,
			     chop_hash_method_t key_hash_method,
			     size_t keys_per_block,
			     chop_hash_tree_indexer_t *htree)
{
  htree->indexer.index_blocks = chop_hash_tree_index_blocks;
  htree->indexer.fetch_stream = chop_hash_tree_fetch_stream;
  htree->indexer.stream_class = NULL;  /* FIXME */
  htree->indexer.index_handle_class = &chop_hash_index_handle_class;

  htree->hash_method = key_hash_method;
  htree->gcrypt_hash_method = chop_hash_gcrypt_name (key_hash_method);
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
chop_hash_tree_fetch_stream (struct chop_indexer *indexer,
			     const chop_index_handle_t *handle,
			     chop_block_store_t *input,
			     chop_block_store_t *metadata,
			     chop_stream_t *output)
{
  return CHOP_ERR_NOT_IMPL;
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

  if (!key_buf)
    return ENOMEM;

  chop_block_tree_init (&tree, htree->keys_per_block,
			htree->gcrypt_hash_method, htree->key_size,
			metadata);
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
      size_t key_size = ((chop_hash_tree_indexer_t *)indexer)->key_size;
      chop_hash_index_handle_t *hhandle = (chop_hash_index_handle_t *)handle;
      chop_object_initialize ((chop_object_t *)handle,
			      &chop_hash_index_handle_class);
      hhandle->hash_size = key_size;

      /* Flush the key block tree and get its key */
      chop_block_tree_flush (&tree, &key);

      /* Copy the top-level key into the handle */
      memcpy (hhandle->hash, chop_block_key_buffer (&key), key_size);
    }

  /* Free memory associated with TREE */
  chop_block_tree_free (&tree);

  return err;
}

#if 0
static __inline__ void
show_hash_tree (chop_hash_tree_indexer_t *htree)
{
  chop_block_key_t **block_keys;
  char hex[1024];
  unsigned block_num;

  block_keys = (chop_block_key_t **)chop_buffer_content (&htree->block_keys);
  printf ("store-hash-tree:  stored %u blocks\n",
	  htree->block_count);
  for (block_num = 0; block_num < htree->block_count; block_num++)
    {
      chop_block_key_t *key = block_keys[block_num];
      chop_block_key_to_hex_string (key, hex);
      printf ("block 0x%04x: %s\n", block_num, hex);
    }
}


static inline void
hash_tree_free (chop_hash_tree_indexer_t *htree)
{
  unsigned block_num;
  chop_block_key_t **block_keys =
    (chop_block_key_t **)chop_buffer_content (&htree->block_keys);

  for (block_num = 0; block_num < htree->block_count; block_num++)
    {
      chop_block_key_free (block_keys[block_num]);
      free (block_keys[block_num]);
    }

  chop_buffer_return (&htree->block_keys);
}
#endif
