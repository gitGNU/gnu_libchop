#ifndef __CHOP_BLOCK_INDEXERS_H__
#define __CHOP_BLOCK_INDEXERS_H__

/* This code is here as a reminder of what can be done to decouple single
   block indexing/fetching from whole stream indexing/fetching.  */
#error "Unused exploratory code!"

/* Block indexers really care about indexing single blocks and the opposite,
   that is, fetching a block given its index.  */

#include <chop/chop.h>
#include <chop/buffers.h>
#include <chop/serializable.h>

_CHOP_BEGIN_DECLS

CHOP_DECLARE_RT_CLASS (block_indexer, object,
		       const chop_class_t *index_handle_class;

		       errcode_t (* index_block) (struct chop_block_indexer *,
						  chop_store_t *,
						  const char *,
						  size_t,
						  chop_index_handle_t *));

CHOP_DECLARE_RT_CLASS (block_fetcher, object,
		       const chop_class_t *index_handle_class;

		       errcode_t (* fetch_block) (struct chop_block_indexer *,
						  const chop_index_handle_t *,
						  chop_store_t *,
						  chop_buffer_t *,
						  size_t *));

CHOP_DECLARE_RT_CLASS (index_handle_class, class,
		       const chop_class_t *block_indexer_class);

CHOP_DECLARE_RT_CLASS (index_handle, object, /* */);

/* This implies that subclasses (e.g. `chk_index_handle') will have to be
   declared as classes whose meta-class is `index_handle_class', and defined
   with `block_indexer_class' pointing to the right class.  */



/* Main problem:  deserializing an index handle

   1.  Deserialize the block indexer _class_ (which then allows to know the
       index handle class).

   2.  Deserialize an _instance_ of this block indexer class (e.g. chk with
       SHA1, SHA1, and Blowfish).

   3.  Deserialize the index handle itself (e.g. an id/cipher key pair).

   OTOH, we don't necessarily want to expose everything in an serialized
   index handle.  For instance, when doing CHK, we don't need the serialized
   handle to contain the name of the hash methods used, but we do need the
   name of the ciphering algorithm.

   Now, if the hash methods aren't available in the serialized handle, then
   the block_indexer we'll get by deserializing it will only be able to
   _fetch_ blocks, not to index other blocks using the same parameters.

   So we should be able to say "serialize this block_indexer instance so that
   enough information is available to provide such capability".  The
   capabilities of a block_indexer are fetching and indexing (the latter
   requires more information -- parameters -- than the former).  Or we can
   separate the `block_indexer' class into two classes:  `block_indexer' and
   `block_fetcher'.

   Sample interface available below.  */

#if 0  /* Consequence:  the new stream indexers.  */

CHOP_DECLARE_RT_CLASS (indexer, object,
		       errcode_t (* index_blocks) (struct chop_indexer *,
						   chop_chopper_t *,
						   chop_block_indexer_t *,
						   chop_block_store_t *,
						   chop_block_store_t *,
						   chop_index_handle_t *);

		       errcode_t (* fetch_stream) (struct chop_indexer *,
						   const chop_index_handle_t *,
						   chop_block_store_t *,
						   chop_block_store_t *,
						   chop_stream_t *);

		       const chop_class_t *index_handle_class;
		       const chop_class_t *stream_class;);

/* Simpler interface.  */
extern errcode_t
chop_hash_tree_indexer_open (size_t indexes_per_block,
			     chop_indexer_t *indexer);

#endif


/* Implementations.  */

extern chop_class_t chop_hash_block_indexer_class;
extern chop_class_t chop_chk_block_indexer_class;

extern errcode_t
chop_hash_block_indexer_open (chop_hash_method_t hash_method,
			      chop_block_indexer_t *indexer);

extern errcode_t
chop_chk_block_indexer_open (chop_hash_method_t key_hash_method,
			     chop_cipher_handle_t cipher_handle,
			     chop_block_indexer_t *indexer);



/* Methods.  */

static __inline__ const chop_class_t *
chop_block_indexer_index_handle_class (const chop_block_indexer_t *__indexer)
{
  return (__indexer->index_handle_class);
}

static __inline__ errcode_t
chop_block_indexer_index (chop_block_indexer_t *__indexer,
			  const char *__buffer, size_t __size,
			  chop_index_handle_t *__handle)
{
  return (__indexer->index_block (__indexer, __buffer, __size, __handle));
}

static __inline__ errcode_t
chop_block_indexer_fetch (chop_block_indexer_t *__indexer,
			  const chop_index_handle_t *__handle,
			  chop_buffer_t *__buffer, size_t *__size)
{
  return (__indexer->fetch_block (__indexer, __handle, __buffer, __size));
}


/* Serialization/deserialization issues (see comments above for the
   rationale).  */

extern const chop_index_handle_class_t *
chop_index_handle_class_lookup (const char *nickname);

extern const chop_class_t *
chop_index_handle_class_block_indexer_class (chop_index_handle_class_t *__class)
{
  return (__class->block_indexer_class);
}

extern errcode_t
chop_index_handle_deserialize (const chop_index_handle_class_t *class,
			       const char *buffer, size_t size, size_t *used,
			       chop_index_handle_t *handle);

_CHOP_END_DECLS

#endif
