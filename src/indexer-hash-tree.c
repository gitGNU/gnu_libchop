#include <chop/chop.h>
#include <chop/indexer-hash-tree.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

/* Glibc's obstacks */
#include <obstack.h>

/* libgcrypt */
#include <gcrypt.h>



/* Internal class declarations.  */

/* Declare `chop_chk_index_handle_t' that inherits from the
   `chop_index_handle_t' class.  */
CHOP_DECLARE_RT_CLASS (chk_index_handle, index_handle,
		       char hash_key[20];
		       char block_id[20];);

/* Declare `chop_hash_index_handle' that inherits from the
   `chop_index_handle_t' class.  */
CHOP_DECLARE_RT_CLASS (hash_index_handle, index_handle,
		       size_t hash_size;
		       char hash[30];);



/* Internal class definitions.  */

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
  htree->indexer.stream_class = NULL;  /* FIXME */
  htree->indexer.index_handle_class = &chop_hash_index_handle_class;

  htree->hash_method = key_hash_method;
  htree->gcrypt_hash_method = chop_hash_gcrypt_name (key_hash_method);
  htree->key_size = chop_hash_size (key_hash_method);
  htree->block_count = 0;

#define obstack_chunk_alloc malloc
#define obstack_chunk_free  free
  obstack_alloc_failed_handler = &chop_obstack_alloc_failed_handler;
  obstack_init (&htree->key_obstack);

  chop_buffer_init (&htree->block_keys, 32);

  return 0;
}

static void
dispose_key_copy (char *key_buffer, void *obstack)
{
  obstack_free ((struct obstack *)obstack, key_buffer);
}

static errcode_t
chop_hash_tree_index_block (chop_hash_tree_indexer_t *htree,
			    chop_block_store_t *store,
			    const char *buffer,
			    size_t size)
{
  errcode_t err;

  /* Copy KEY and store it in our block key vector STORE->BLOCK_KEYS.
     Note: obstack-allocated objects need to be deallocated in reverse order!
     */
  chop_block_key_t *key_copy;
  char *hash = obstack_alloc (&htree->key_obstack, htree->key_size);
  key_copy = obstack_alloc (&htree->key_obstack,
			    sizeof (chop_block_key_t));
  if ((!hash) || (!key_copy))
    return ENOMEM;

  chop_block_key_init (key_copy, hash, htree->key_size,
		       &dispose_key_copy, &htree->key_obstack);

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
  /* FIXME:  Make this function reentrant */
  errcode_t err = 0;
  size_t amount;
  chop_buffer_t buffer;

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

      err = chop_hash_tree_index_block ((chop_hash_tree_indexer_t *)indexer,
					output,
					chop_buffer_content (&buffer),
					chop_buffer_size (&buffer));
      if (err)
	break;
    }

  {
    /* Initialize the (serializable) handle object.  */
    size_t key_size = ((chop_hash_tree_indexer_t *)indexer)->key_size;
    chop_hash_index_handle_t *hhandle = (chop_hash_index_handle_t *)handle;
    chop_object_initialize ((chop_object_t *)handle,
			    &chop_hash_index_handle_class);
    hhandle->hash_size = key_size;
    /* FIXME:  We should return the handle of the top-level inode block.  */
    memcpy (hhandle->hash, "012345678901234567890123456789", key_size);
  }

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

