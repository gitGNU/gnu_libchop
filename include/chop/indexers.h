
/* Indexers */

#include <chop/chop.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/streams.h>
#include <chop/serializable.h>

typedef struct chop_indexer chop_indexer_t;

/* Declare the (serializable) `chop_index_handle_t' class that inherits from
   `chop_object_t' and does not contain any additional field.  */
CHOP_DECLARE_RT_CLASS (index_handle, object, /**/);


struct chop_indexer
{
  errcode_t (* index_blocks) (struct chop_indexer *,
			      chop_chopper_t *,
			      chop_block_store_t *,
			      chop_block_store_t *,
			      chop_index_handle_t *);

  errcode_t (* fetch_stream) (struct chop_indexer *,
			      const chop_index_handle_t *,
			      chop_block_store_t *,
			      chop_block_store_t *,
			      chop_stream_t *);

  const chop_class_t *stream_class;
  const chop_class_t *index_handle_class;
};



/* Use INDEXER to index and store the blocks from INPUT in block store
   DATASTORE.  Block meta-information (e.g. a block list, a block tree) will
   be stored to METADATASTORE.  On success, return 0 and set HANDLE to an
   index handle necessary and sufficient to retrieve all the indexed blocks
   from DATASTORE.  HANDLE must point to an (uninitialized) memory area whose
   size should that of instances of the class returned by the
   CHOP_INDEXER_INDEX_HANDLE_CLASS for INDEXER.  */
static __inline__ errcode_t
chop_indexer_index_blocks (chop_indexer_t *__indexer,
			   chop_chopper_t *__input,
			   chop_block_store_t *__datastore,
			   chop_block_store_t *__metadatastore,
			   chop_index_handle_t *__handle)
{
  return (__indexer->index_blocks (__indexer, __input,
				   __datastore, __metadatastore,
				   __handle));
}

/* Use INDEXER to retrieve the stream pointed to by HANDLE from block store
   DATASTORE and meta-data store METADATASTORE.  On success, return zero and
   set OUTPUT to the corresponding stream object.  OUTPUT must point to an
   (uninitialized) memory area whose size should that of instances of the
   class returned by the CHOP_INDEXER_STREAM_CLASS for INDEXER.  */
static __inline__ errcode_t
chop_indexer_fetch_stream (chop_indexer_t *__indexer,
			   const chop_index_handle_t *__handle,
			   chop_block_store_t *__datastore,
			   chop_block_store_t *__metadatastore,
			   chop_stream_t *__output)
{
  return (__indexer->fetch_stream (__indexer, __handle,
				   __datastore, __metadatastore,
				   __output));
}


/* Methods for caller-management of memory allocation.  */

/* Return the class of index handles produced by INDEXER (by its INDEX_BLOCKS
   method).  */
static __inline__ const chop_class_t *
chop_indexer_index_handle_class (const chop_indexer_t *__indexer)
{
  return (__indexer->index_handle_class);
}

/* Return the class of streams produced by INDEXER (by its FETCH_STREAM
   method).  */
static __inline__ const chop_class_t *
chop_indexer_stream_class (const chop_indexer_t *__indexer)
{
  return (__indexer->stream_class);
}

#define chop_indexer_alloca_stream(__indexer)				    \
((chop_stream_t *)							    \
 chop_class_alloca_instance (((chop_indexer_t *)(__indexer))->stream_class))


#define chop_indexer_alloca_index_handle(__indexer)				   \
((chop_index_handle_t *)							   \
 chop_class_alloca_instance (((chop_indexer_t *)(__indexer))->index_handle_class))


