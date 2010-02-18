/* libchop -- a utility library for distributed storage
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef __CHOP_BLOCK_INDEXERS_H__
#define __CHOP_BLOCK_INDEXERS_H__

/* Block indexers really care about indexing single blocks and the opposite,
   that is, fetching a block given its index.  We actually have two different
   classes (block indexers and block fetchers) only for practical purposes:
   usually, less information is required to fetch a block (IOW, to
   deserialize a fetcher) than to actually index it (the block indexer may
   need to be told about algorithms to use that the fetcher doesn't have to
   know).  See below a description of the whole serialization issue.  */

#include <chop/chop.h>
#include <chop/hash.h>
#include <chop/cipher.h>
#include <chop/stores.h>
#include <chop/buffers.h>
#include <chop/objects.h>

_CHOP_BEGIN_DECLS


/* Index handles.  */

CHOP_DECLARE_RT_CLASS (index_handle, object,
		       size_t size;);


/* Return the size (in bytes) that the binary serialization of HANDLE is
   supposed to need.  */
static __inline__ size_t
chop_index_handle_binary_size (const chop_index_handle_t *__handle)
{
  return (__handle->size);
}



/* Block indexers and fetchers.  */

struct chop_block_fetcher;

CHOP_DECLARE_RT_CLASS (block_indexer, object,
		       const chop_class_t *index_handle_class;
		       const chop_class_t *block_fetcher_class;

		       chop_error_t (* index_block) (struct
						     chop_block_indexer *,
						     chop_block_store_t *,
						     const char *,
						     size_t,
						     chop_index_handle_t *);

		       chop_error_t (* init_fetcher) (const struct
						      chop_block_indexer *,
						      struct
						      chop_block_fetcher *););

CHOP_DECLARE_RT_CLASS (block_fetcher, object,
		       const chop_class_t *index_handle_class;

		       chop_error_t (* fetch_block) (struct chop_block_fetcher *,
						     const chop_index_handle_t *,
						     chop_block_store_t *,
						     chop_buffer_t *,
						     size_t *););



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



/* Methods.  */

/* Return the index handle class associated to INDEXER's class.  */
static __inline__ const chop_class_t *
chop_block_indexer_index_handle_class (const chop_block_indexer_t *__indexer)
{
  return (__indexer->index_handle_class);
}

/* Return the block fetcher class associated to INDEXER's class.  */
static __inline__ const chop_class_t *
chop_block_indexer_fetcher_class (const chop_block_indexer_t *__indexer)
{
  return (__indexer->block_fetcher_class);
}

/* Initialize FETCHER as a block fetcher corresponding to block indexer
   INDEXER.  FETCHER must point to a memory area large enough to contain an
   instance of the fetcher class associated to INDEXER's class.  */
static __inline__ chop_error_t
chop_block_indexer_initialize_fetcher (const chop_block_indexer_t *__indexer,
				       chop_block_fetcher_t *__fetcher)
{
  return (__indexer->init_fetcher (__indexer, __fetcher));
}

/* Using INDEXER, index the SIZE-byte long data block pointed to by BUFFER to
   STORE.  On success, return zero and initialize HANDLE with an index handle
   sufficient to restore that block from STORE using the corresponding
   fetcher.  Otherwise, an error is returned.  */
static __inline__ chop_error_t
chop_block_indexer_index (chop_block_indexer_t *__indexer,
			  chop_block_store_t *__store,
			  const char *__buffer, size_t __size,
			  chop_index_handle_t *__handle)
{
  return (__indexer->index_block (__indexer, __store,
				  __buffer, __size, __handle));
}

/* Using FETCHER, fetch from STORE the data block whose index handle is
   HANDLE.  On success, fill in BUFFER with its contents and set SIZE to its
   size in bytes.  */
static __inline__ chop_error_t
chop_block_fetcher_fetch (chop_block_fetcher_t *__fetcher,
			  const chop_index_handle_t *__handle,
			  chop_block_store_t *__store,
			  chop_buffer_t *__buffer, size_t *__size)
{
  return (__fetcher->fetch_block (__fetcher, __handle, __store,
				  __buffer, __size));
}

/* Return the index handle class that is associated with the class of block
   fetcher F.  */
static __inline__ const chop_class_t *
chop_block_fetcher_index_handle_class (const chop_block_fetcher_t *__f)
{
  return (__f->index_handle_class);
}

/* Allocate on the stack enough room to store an index handle of the index
   handle class associated to INDEXER's class.  */
#define chop_block_indexer_alloca_index_handle(__indexer)				   \
((chop_index_handle_t *)							   \
 chop_class_alloca_instance (((chop_block_indexer_t *)(__indexer))->index_handle_class))

/* Allocate on the stack enough room to store a block fetcher of the block
   fetcher class associated to INDEXER's class.  */
#define chop_block_indexer_alloca_fetcher(__indexer)				   \
((chop_block_fetcher_t *)							   \
 chop_class_alloca_instance (((chop_block_indexer_t *)(__indexer))->block_fetcher_class))




/* Implementations of these interfaces.  */

extern const chop_class_t chop_hash_block_indexer_class;
extern const chop_class_t chop_chk_block_indexer_class;
extern const chop_class_t chop_uuid_block_indexer_class;
extern const chop_class_t chop_integer_block_indexer_class;

extern const chop_class_t chop_hash_block_fetcher_class;
extern const chop_class_t chop_chk_block_fetcher_class;
extern const chop_class_t chop_uuid_block_fetcher_class;
extern const chop_class_t chop_integer_block_fetcher_class;


/* Initialize INDEXER as a hash block indexer.  INDEXER will use HASH_METHOD
   to compute the identifier of the given blocks and will use that identifier
   when storing the block.  It leaves the block contents unchanged.  This
   technique is known as ``content-based addressing'', or
   ``compare-by-hash''.   */
extern chop_error_t
chop_hash_block_indexer_open (chop_hash_method_t hash_method,
			      chop_block_indexer_t *indexer);

/* Initialize BLOCK_INDEXER as a CHK (meaning ``content-hash key'') block
   indexer.  BLOCK_INDEXER will cipher data blocks using CIPHER_HANDLE and
   the block's hash yielded by KEY_HASH_METHOD (this is symmetric
   ciphering).  Finally, BLOCK_INDEXER will compute the block's identifier
   using BLOCK_ID_HASH_METHOD.  This technique is known as ``convergent
   encryption'' and the index yielded is sometimes referred to as a
   ``content-hash key'' (in GNUnet/FreeNet terminology).  */
extern chop_error_t
chop_chk_block_indexer_open (chop_cipher_handle_t cipher_handle,
			     int owns_cipher_handle,
			     chop_hash_method_t key_hash_method,
			     chop_hash_method_t block_id_hash_method,
			     chop_block_indexer_t *block_indexer);

/* Initialize BLOCK_INDEXER as a UUID block indexer.  In other words,
   BLOCK_INDEXER will then yield DCE compatible Universally Unique
   Identifiers for each block, using `libuuid' (provided it was available at
   compilation time).  Therefore, BLOCK_INDEXER will not have a
   single-instance storage property, unlike the `chk' and `hash' block
   indexers.  */
extern chop_error_t
chop_uuid_block_indexer_open (chop_block_indexer_t *block_indexer);

/* Initialize INDEXER as an indexer that will simply return consecutive
   32-bit integers as block IDs, starting from START.  */
extern chop_error_t
chop_integer_block_indexer_open (unsigned long start,
				 chop_block_indexer_t *indexer);


/* If FETCHER is an instance of CHOP_HASH_BLOCK_FETCHER_CLASS, then return
   its associated log, otherwise return NULL.  */
extern chop_log_t *chop_hash_block_fetcher_log (chop_block_fetcher_t *fetcher);

/* If FETCHER is an instance of CHOP_INTEGER_BLOCK_FETCHER_CLASS, then return
   its associated log, otherwise return NULL.  */
extern chop_log_t *
chop_integer_block_fetcher_log (chop_block_fetcher_t *fetcher);

/* If FETCHER is an instance of CHOP_UUID_BLOCK_FETCHER_CLASS, then return
   its associated log, otherwise return NULL.  */
extern chop_log_t *
chop_uuid_block_fetcher_log (chop_block_fetcher_t *fetcher);


_CHOP_END_DECLS

#endif
