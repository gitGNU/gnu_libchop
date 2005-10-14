/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <errno.h>
#include <assert.h>

#ifdef DEBUG
# include <stdio.h>
#endif


static __inline__ errcode_t
chop_hash_tree_indexer_open_alloc (chop_hash_method_t content_hash_method,
				   chop_hash_method_t key_hash_method,
				   chop_cipher_handle_t cipher,
				   size_t keys_per_block,
				   chop_indexer_t **indexer)
{
  errcode_t err;

  *indexer = scm_malloc (chop_class_instance_size (&chop_hash_tree_indexer_class));

  err = chop_hash_tree_indexer_open (content_hash_method, key_hash_method,
				     cipher,
				     keys_per_block, *indexer);
  if (err)
    {
      free (*indexer);
      *indexer = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_indexer_index_blocks_alloc  (chop_indexer_t *indexer,
				  chop_chopper_t *input,
				  chop_block_store_t *datastore,
				  chop_block_store_t *metadatastore,
				  chop_index_handle_t **handle)
{
  errcode_t err;
  const chop_class_t *handle_class;

  handle_class = chop_indexer_index_handle_class (indexer);
  *handle = scm_malloc (chop_class_instance_size (handle_class));

  err = chop_indexer_index_blocks (indexer, input,
				   datastore, metadatastore, *handle);

  /* The CHOP_STREAM_END is somewhat the "normal" case so we just return 0
     here so that `indexer-index-blocks' ends up only returning the index
     handle.  */
  err = ((err == CHOP_STREAM_END) ? 0 : err);
  if (err)
    {
      free (*handle);
      *handle = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_indexer_fetch_stream_alloc (chop_indexer_t *indexer,
				 const chop_index_handle_t *handle,
				 chop_block_store_t *datastore,
				 chop_block_store_t *metadatastore,
				 chop_stream_t **stream)
{
  errcode_t err;
  const chop_class_t *stream_class;

  stream_class = chop_indexer_stream_class (indexer);
  *stream = scm_malloc (chop_class_instance_size (stream_class));

  err = chop_indexer_fetch_stream (indexer, handle,
				   datastore, metadatastore, *stream);
  if (err)
    {
      free (*stream);
      *stream = NULL;
    }

  return err;
}

/* Convenience function.  */
static __inline__ errcode_t
chop_index_handle_ascii_serialize (const chop_index_handle_t *handle,
				   char **serialization)
{
  errcode_t err;
  chop_buffer_t buffer;

  chop_buffer_init (&buffer, 0);

  err = chop_object_serialize ((chop_object_t *)handle,
			       CHOP_SERIAL_ASCII, &buffer);
  if (!err)
    {
      *serialization = strdup (chop_buffer_content (&buffer));
      if (!*serialization)
	err = ENOMEM;
    }
  else
    *serialization = NULL;

  chop_buffer_return (&buffer);

  return err;
}

static __inline__ errcode_t
chop_index_handle_ascii_deserialize (chop_indexer_t *indexer,
				     const char *ascii_handle,
				     chop_index_handle_t **handle)
{
  errcode_t err;
  const chop_class_t *handle_class;

  handle_class = chop_indexer_index_handle_class (indexer);
  *handle = scm_malloc (chop_class_instance_size (handle_class));

#ifdef DEBUG
  fprintf (stderr, "%s: deserializing index handle, class `%s', size %u\n",
	   __FUNCTION__,
	   chop_class_name (handle_class),
	   chop_class_instance_size (handle_class));
#endif

  err = chop_object_deserialize ((chop_object_t *)*handle, handle_class,
				 CHOP_SERIAL_ASCII,
				 ascii_handle, strlen (ascii_handle));
  if (err)
    {
      free (*handle);
      *handle = NULL;
    }

  return err;
}
