#include <chop/chop.h>
#include <chop/indexer-hash-tree.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

/* Glibc's obstacks */
#include <obstack.h>

/* libgcrypt */
#include <gcrypt.h>


CHOP_DEFINE_RT_CLASS (index_handle, object,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);

CHOP_DEFINE_RT_CLASS (chk_index_handle, index_handle,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);


static errcode_t
chop_hash_tree_index_blocks (chop_indexer_t *indexer,
			     chop_chopper_t *input,
			     chop_block_store_t *output,
			     chop_index_handle_t *handle);

static errcode_t
chop_hash_tree_fetch_stream (struct chop_indexer *,
			     const chop_index_handle_t *,
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
			     chop_hash_tree_indexer_t *htree)
{
  htree->indexer.index_blocks = chop_hash_tree_index_blocks;
  htree->indexer.fetch_stream = chop_hash_tree_fetch_stream;
  htree->indexer.sizeof_stream = 12;  /* FIXME */
  htree->indexer.sizeof_index_handle = 12;

  htree->hash_method = key_hash_method;
  htree->gcrypt_hash_method = chop_hash_gcrypt_name (key_hash_method);
  htree->key_size = chop_hash_size (key_hash_method);
  htree->block_count = 0;

#if 0
#define obstack_chunk_alloc malloc
#define obstack_chunk_free  free
  obstack_alloc_failed_handler = &chop_obstack_alloc_failed_handler;
  obstack_init (&store->key_obstack);
#endif

  chop_buffer_init (&htree->block_keys, 32);

  return 0;
}

static void
dispose_key_copy (char *key_content, void *unused)
{
  free (key_content);
}

static errcode_t
chop_hash_tree_index_block (chop_hash_tree_indexer_t *htree,
			    chop_block_store_t *store,
			    const char *buffer,
			    size_t size)
{
  errcode_t err;

  /* Copy KEY and store it in our block key vector STORE->BLOCK_KEYS */
  chop_block_key_t *key_copy;
  char *hash = calloc (1, htree->key_size);
  key_copy = calloc (1, sizeof (chop_block_key_t)); /* FIXME: use an obstack */
  if ((!hash) || (!key_copy))
    return ENOMEM;

  chop_block_key_init (key_copy, hash, htree->key_size,
		       &dispose_key_copy, NULL);

  /* Compute this buffer's key */
  gcry_md_hash_buffer (htree->gcrypt_hash_method, hash,
		       buffer, size);

  /* Add this block key to the key vector */
  htree->block_count++;
  err = chop_buffer_append (&htree->block_keys,
			    (const char *)&key_copy,
			    sizeof (chop_block_key_t *));

  /* Store this block in the output block store */
  err = chop_store_write_block (store, key_copy, buffer, size);

  return err;
}

static errcode_t
chop_hash_tree_fetch_stream (struct chop_indexer *indexer,
			     const chop_index_handle_t *handle,
			     chop_block_store_t *input,
			     chop_stream_t *output)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_hash_tree_index_blocks (chop_indexer_t *indexer,
			     chop_chopper_t *input,
			     chop_block_store_t *output,
			     chop_index_handle_t *handle)
{
  errcode_t err = 0;
  size_t amount;
  chop_buffer_t buffer;

  err = chop_buffer_init (&buffer,
			  chop_chopper_typical_block_size (input));
  if (err)
    return err;

  /* Read blocks from INPUT until the underlying stream returns
     CHOP_STREAM_END.  */
  while (1)
    {
      chop_buffer_clear (&buffer);
      err = chop_chopper_read_block (input, &buffer, &amount);
      if (err)
	break;

      if (!amount)
	continue;

      err = chop_hash_tree_index_block ((chop_hash_tree_indexer_t *)indexer,
					output,
					chop_buffer_content (&buffer),
					chop_buffer_size (&buffer));
      if (err)
	break;
    }

  /* Initialize the (serializable) handle object.
     FIXME:  We should return the handle of the top-level inode block.  */
  chop_object_initialize ((chop_object_t *)handle, &chop_index_handle_class);

  return err;
}

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

