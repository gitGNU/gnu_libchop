#ifndef __CHOP_INDEXERS_H__
#define __CHOP_INDEXERS_H__

/* Stream indexers.  */

#include <chop/chop.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/streams.h>
#include <chop/block-indexers.h>
#include <chop/objects.h>
#include <chop/cipher.h>


/* Declare the `chop_indexer_t' class that inherits from `chop_object_t'.
   Note that indexers have no `close ()' method:  they must eventually be
   destroyed using `chop_object_destroy ()'.  */
CHOP_DECLARE_RT_CLASS (indexer, object,
		       errcode_t (* index_blocks) (struct chop_indexer *,
						   chop_chopper_t *,
						   chop_block_indexer_t *,
						   chop_block_store_t *,
						   chop_block_store_t *,
						   chop_index_handle_t *);

		       errcode_t (* fetch_stream) (struct chop_indexer *,
						   const chop_index_handle_t *,
						   chop_block_fetcher_t *,
						   chop_block_store_t *,
						   chop_block_store_t *,
						   chop_stream_t *);

		       const chop_class_t *stream_class;);




/* Use INDEXER to index and store the blocks from INPUT in block store
   DATASTORE, using BLOCK_INDEXER to index individual blocks.  Block
   meta-information (e.g. a block list, a block tree) will be stored to
   METADATASTORE.  On success, return 0 and set HANDLE to an index handle
   necessary and sufficient to retrieve all the indexed blocks from
   DATASTORE.  HANDLE must point to an (uninitialized) memory area whose size
   should that of instances of the class returned by the
   CHOP_BLOCK_INDEXER_INDEX_HANDLE_CLASS for BLOCK_INDEXER.  */
static __inline__ errcode_t
chop_indexer_index_blocks (chop_indexer_t *__indexer,
			   chop_chopper_t *__input,
			   chop_block_indexer_t *__block_indexer,
			   chop_block_store_t *__datastore,
			   chop_block_store_t *__metadatastore,
			   chop_index_handle_t *__handle)
{
  return (__indexer->index_blocks (__indexer, __input, __block_indexer,
				   __datastore, __metadatastore,
				   __handle));
}

/* Use INDEXER to retrieve the stream pointed to by HANDLE from block store
   DATASTORE and meta-data store METADATASTORE, using FETCHER to fetch
   individual blocks.  On success, return zero and set OUTPUT to the
   corresponding stream object.  OUTPUT must point to an (uninitialized)
   memory area whose size should that of instances of the class returned by
   the CHOP_INDEXER_STREAM_CLASS for INDEXER.  */
static __inline__ errcode_t
chop_indexer_fetch_stream (chop_indexer_t *__indexer,
			   const chop_index_handle_t *__handle,
			   chop_block_fetcher_t *__fetcher,
			   chop_block_store_t *__datastore,
			   chop_block_store_t *__metadatastore,
			   chop_stream_t *__output)
{
  return (__indexer->fetch_stream (__indexer, __handle, __fetcher,
				   __datastore, __metadatastore,
				   __output));
}


/* Methods for caller-management of memory allocation.  */

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



/* Convenience method for user interfaces.  */

/* Deserialize BUFFER, a SIZE-byte buffer containing an ASCII string, and
   return the block fetcher class it represents in FETCHER_CLASS and the
   corresponding index handle class in HANDLE_CLASS.  On success, also set
   BYTES_READ to the amount of bytes read starting from BUFFER, and return
   zero.  The user may then process with stage 2 of the deserialization by
   (i) allocating instances of FETCHER_CLASS and HANDLE_CLASS and (ii) start
   stage 2 at BUFFER + BYTES_READ.  */
errcode_t
chop_ascii_deserialize_index_tuple_s1 (const char *buffer, size_t size,
				       const chop_class_t **fetcher_class,
				       const chop_class_t **handle_class,
				       size_t *bytes_read);

/* Perform stage 2 of an index tuple deserialization, i.e. read BUFFER and
   deserialize an instance of FETCHER_CLASS into FETCHER and an instance of
   INDEX_CLASS into INDEX (see above).  */
errcode_t
chop_ascii_deserialize_index_tuple_s2 (const char *buffer, size_t size,
				       const chop_class_t *fetcher_class,
				       const chop_class_t *index_class,
				       chop_block_fetcher_t *fetcher,
				       chop_index_handle_t *index,
				       size_t *bytes_read);

/* Fill in BUFFER with an ASCII serialization of a block fetcher
   corresponding to BLOCK_INDEXER and a serialization of index handle
   INDEX.  */
extern errcode_t
chop_ascii_serialize_index_tuple (const chop_index_handle_t *index,
				  const chop_block_indexer_t *block_indexer,
				  chop_buffer_t *buffer);


/* The class of the hash tree indexer, an actual indexer implementation.  */
extern const chop_class_t chop_tree_indexer_class;

#include <chop/hash.h>
#include <chop/logs.h>

/* FIXME:  Update comment.  */
/* Initialize the hash tree indexer HTREE.  Blocks will be symmetrically
   ciphered using a hash produced by the CONTENT_HASH_METHOD algorithm.
   Block keys are then computed using KEY_HASH_METHOD.  KEYS_PER_BLOCK is the
   maximum number of block keys that should be stored in each key block (or
   "inode") when indexing streams.  HTREE must point to a memory region as
   large as needed by instances of CHOP_TREE_INDEXER_CLASS.
   CIPHER_HANDLE may be either CHOP_CIPHER_HANDLE_NIL, in which case blocks
   will not be ciphered, or an open cipher handle in which case it will be
   used to cipher blocks individually.  */
extern errcode_t chop_tree_indexer_open (size_t indexes_per_block,
					 chop_indexer_t *htree);

/* Return the log attached to INDEXER, assuming INDEXER's class is
   CHOP_TREE_INDEXER_CLASS.  */
extern chop_log_t *chop_tree_indexer_log (chop_indexer_t *indexer);


#endif
