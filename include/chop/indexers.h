
/* Indexers */

#include <chop/chop.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/streams.h>
#include <chop/serializable.h>

typedef struct chop_indexer chop_indexer_t;

/* Declare the (serializable) `chop_index_handle_t' class that inherits from
   `chop_object_t' and does not contain any additional field.  */
CHOP_DECLARE_RT_CLASS (index_handle, object,);

/* Declare `chop_chk_index_handle_t' that inherits from the
   `chop_index_handle_t' class.  */
CHOP_DECLARE_RT_CLASS (chk_index_handle, object,
		       char hash_key[20];
		       char block_id[20]);


struct chop_indexer
{
  errcode_t (* index_blocks) (struct chop_indexer *,
			      chop_chopper_t *,
			      chop_block_store_t *,
			      chop_index_handle_t *);

  errcode_t (* fetch_stream) (struct chop_indexer *,
			      const chop_index_handle_t *,
			      chop_block_store_t *,
			      chop_stream_t *);

  size_t sizeof_stream;
  size_t sizeof_index_handle;
};



/* Use INDEXER to index and store the blocks from INPUT in block store
   OUTPUT.  On success, return 0 and set HANDLE to an index handle necessary
   and sufficient to retrieve all the indexed blocks from OUTPUT.  HANDLE
   must point to an (uninitialized) memory area whose size should be equal to
   that returned by the CHOP_INDEXER_SIZE_OF_HANDLE method for INDEXER.  */
static __inline__ errcode_t
chop_indexer_index_blocks (chop_indexer_t *__indexer,
			   chop_chopper_t *__input,
			   chop_block_store_t *__output,
			   chop_index_handle_t *__handle)
{
  return (__indexer->index_blocks (__indexer, __input,
				   __output, __handle));
}

/* Use INDEXER to retrieve the stream pointed to by HANDLE from block store
   INPUT.  On success, return zero and set OUTPUT to the corresponding stream
   object.  OUTPUT must point to an (uninitialized) memory area whose size
   should be equal to that returned by the CHOP_INDEXER_SIZE_OF_STREAM method
   for INDEXER.  */
static __inline__ errcode_t
chop_indexer_fetch_stream (chop_indexer_t *__indexer,
			   const chop_index_handle_t *__handle,
			   chop_block_store_t *__input,
			   chop_stream_t *__output)
{
  return (__indexer->fetch_stream (__indexer, __handle,
				   __input, __output));
}


/* Methods for caller-management of memory allocation.  */

#define _make_sizeof_getter(_type)					\
static __inline__ size_t						\
chop_indexer_size_of_ ## _type (const chop_indexer_t *__indexer)	\
{									\
  return (__indexer->sizeof_ ## _type);					\
}

_make_sizeof_getter (stream);
_make_sizeof_getter (index_handle);

#undef _make_sizeof_getter

#define chop_indexer_alloca_stream(__indexer)				\
((chop_stream_t *)							\
 alloca (chop_indexer_size_of_stream ((chop_indexer_t *)(__indexer))))

#define chop_indexer_alloca_index_handle(__indexer)			      \
((chop_index_handle_t *)						      \
  alloca (chop_indexer_size_of_index_handle ((chop_indexer_t *)(__indexer))))




extern errcode_t chop_index_handle_serialize (const chop_index_handle_t *,
					      chop_serial_method_t method,
					      chop_buffer_t *buffer);

extern errcode_t chop_index_handle_deserialize (const char *buffer,
						size_t size,
						chop_serial_method_t method,
						chop_index_handle_t *);


/* extern errcode_t */
/* chop_register_index_handle_class (chop_index_handle_class_t *); */

/* extern void */
/* chop_index_handle_class_init (chop_index_handle_class_t *, */
/* 			      const char *name, */
/* 			      size_t size_of); */

