/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <errno.h>
#include <assert.h>

static void
chop_indexer_close_dealloc (chop_indexer_t *indexer)
{
  if (indexer)
    {
      /* chop_indexer_close (indexer); */ /* FIXME:  This should exist! */
      free (indexer);
    }
}

static void
chop_index_handle_close_dealloc (chop_index_handle_t *handle)
{
  if (handle)
    {
      /* FIXME: chop_index_handle_close (handle); */
      free (handle);
    }
}

static __inline__ errcode_t
chop_hash_tree_indexer_open_alloc (chop_hash_method_t content_hash_method,
				   chop_hash_method_t key_hash_method,
				   size_t keys_per_block,
				   chop_indexer_t **indexer)
{
  errcode_t err;

  *indexer = malloc (chop_class_instance_size (&chop_hash_tree_indexer_class));
  if (!*indexer)
    return ENOMEM;

  err = chop_hash_tree_indexer_open (content_hash_method, key_hash_method,
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
  *handle = malloc (chop_class_instance_size (handle_class));
  if (!*handle)
    return ENOMEM;

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
  *stream = malloc (chop_class_instance_size (stream_class));
  if (!*stream)
    return ENOMEM;

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
  *handle = malloc (chop_class_instance_size (handle_class));
  if (!*handle)
    return ENOMEM;

  err = chop_object_deserialize (*handle, handle_class,
				 CHOP_SERIAL_ASCII,
				 ascii_handle, strlen (ascii_handle));
  if (err)
    {
      free (*handle);
      *handle = NULL;
    }

  return err;
}
