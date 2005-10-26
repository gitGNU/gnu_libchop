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
