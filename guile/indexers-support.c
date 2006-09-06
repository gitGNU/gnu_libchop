/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <errno.h>
#include <assert.h>

#ifdef DEBUG
# include <stdio.h>
#endif


static __inline__ errcode_t
chop_tree_indexer_open_alloc (size_t keys_per_block,
			      chop_indexer_t **indexer)
{
  errcode_t err;

  *indexer = scm_malloc (chop_class_instance_size (&chop_tree_indexer_class));

  err = chop_tree_indexer_open (keys_per_block, *indexer);
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
				  chop_block_indexer_t *block_indexer,
				  chop_block_store_t *datastore,
				  chop_block_store_t *metadatastore,
				  chop_index_handle_t **handle)
{
  errcode_t err;
  const chop_class_t *handle_class;

  handle_class = chop_block_indexer_index_handle_class (block_indexer);
  *handle = scm_malloc (chop_class_instance_size (handle_class));

  err = chop_indexer_index_blocks (indexer, input, block_indexer,
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
				 chop_block_fetcher_t *fetcher,
				 chop_block_store_t *datastore,
				 chop_block_store_t *metadatastore,
				 chop_stream_t **stream)
{
  errcode_t err;
  const chop_class_t *stream_class;

  stream_class = chop_indexer_stream_class (indexer);
  *stream = scm_malloc (chop_class_instance_size (stream_class));

  err = chop_indexer_fetch_stream (indexer, handle, fetcher,
				   datastore, metadatastore, *stream);
  if (err)
    {
      free (*stream);
      *stream = NULL;
    }

  return err;
}


static __inline__ errcode_t
chop_ascii_serialize_index_tuple_alloc (chop_index_handle_t *index,
					chop_indexer_t *indexer,
					chop_block_indexer_t *block_indexer,
					char **serial)
{
  errcode_t err;
  chop_buffer_t buffer;

  err = chop_buffer_init (&buffer, 200);
  if (err)
    return err;

  err = chop_ascii_serialize_index_tuple (index, indexer, block_indexer,
					  &buffer);
  if (err)
    return err;

  *serial = scm_malloc (chop_buffer_size (&buffer));
  memcpy (*serial, chop_buffer_content (&buffer), chop_buffer_size (&buffer));

  chop_buffer_return (&buffer);

  return err;
}

static __inline__ errcode_t
chop_ascii_deserialize_index_tuple_alloc (const char *serial,
					  chop_index_handle_t **index,
					  chop_indexer_t **indexer,
					  chop_block_fetcher_t **fetcher,
					  unsigned *bytes_read)
{
  errcode_t err;
  size_t offset = 0;
  const chop_class_t *indexer_class, *fetcher_class, *handle_class;

  err = chop_ascii_deserialize_index_tuple_s1 (serial, strlen (serial),
					       &indexer_class,
					       &fetcher_class, &handle_class,
					       &offset);
  if (err)
    return err;

  *index = scm_malloc (chop_class_instance_size (handle_class));
  *indexer = scm_malloc (chop_class_instance_size (indexer_class));
  *fetcher = scm_malloc (chop_class_instance_size (fetcher_class));

  *bytes_read = 0;
  err = chop_ascii_deserialize_index_tuple_s2 (serial + offset,
					       strlen (serial) - offset,
					       indexer_class,
					       fetcher_class, handle_class,
					       *indexer, *fetcher, *index,
					       bytes_read);
  *bytes_read += offset;

  if (err)
    {
      free (*index);    *index = NULL;
      free (*fetcher);  *fetcher = NULL;
      free (*indexer);  *indexer = NULL;
      return err;
    }

  return err;
}

