/* A block indexer that generates DCE compatible Universally Unique
   Identifiers using libuuid.  A lot of code is borrowed from
   `block-indexer-hash.c'.

   This block indexer, unlike the `hash' and `chk' block indexers, does not
   have the single-instance storage property since every block automatically
   gets assigned a new ID.  */

#include <chop/chop.h>
#include <chop/block-indexers.h>

#include <uuid/uuid.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>



/* The index handle class.  */

CHOP_DECLARE_RT_CLASS (uuid_index_handle, index_handle,
		       uuid_t uuid;  /* the block key */);

/* The manual for libuuid specifies that `uuid_generate ()' returns 36 bytes
   (ASCII) plus a trailing zero.  */
#define CHOP_UUID_SIZE  37

static int
uih_equalp (const chop_object_t *h1, const chop_object_t *h2)
{
  chop_uuid_index_handle_t *uih1, *uih2;

  uih1 = (chop_uuid_index_handle_t *)h1;
  uih2 = (chop_uuid_index_handle_t *)h2;

  return (!uuid_compare (uih1->uuid, uih2->uuid));
}

static errcode_t
uih_copy (const chop_object_t *s, chop_object_t *d)
{
  chop_uuid_index_handle_t *source, *dest;

  source = (chop_uuid_index_handle_t *)s;
  dest = (chop_uuid_index_handle_t *)d;

  uuid_copy (dest->uuid, source->uuid);

  return 0;
}

static errcode_t
uih_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_uuid_index_handle_t *uuid =
    (chop_uuid_index_handle_t *)object;
  char out[CHOP_UUID_SIZE];


  switch (method)
    {
    case CHOP_SERIAL_BINARY:
      /* FIXME: We should try to implement a binary serialization function. */
    case CHOP_SERIAL_ASCII:
      uuid_unparse (uuid->uuid, out);
      /* assert (strlen (out) + 1 == CHOP_UUID_SIZE); */
      chop_buffer_push (buffer, out, CHOP_UUID_SIZE);
      break;

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

static errcode_t
uih_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;
  chop_uuid_index_handle_t *uuid =
    (chop_uuid_index_handle_t *)object;

  *bytes_read = 0;
  err = chop_object_initialize (object, &chop_uuid_index_handle_class);
  if (err)
    return err;

  switch (method)
    {
    case CHOP_SERIAL_BINARY:
    case CHOP_SERIAL_ASCII:
      if (size < 37)
	return CHOP_DESERIAL_TOO_SHORT;

      if (uuid_parse (buffer, uuid->uuid))
	return CHOP_DESERIAL_CORRUPT_INPUT;

      *bytes_read = 37;
      break;

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

CHOP_DEFINE_RT_CLASS (uuid_index_handle, index_handle,
		      NULL, NULL,
		      uih_copy, uih_equalp,
		      uih_serialize, uih_deserialize);


/* The fetcher class.  */
CHOP_DECLARE_RT_CLASS (uuid_block_fetcher, block_fetcher,
		       /* Nothing, easy.  */
		       chop_log_t log;);

static errcode_t uuid_block_fetch (chop_block_fetcher_t *,
				   const chop_index_handle_t *,
				   chop_block_store_t *,
				   chop_buffer_t *,
				   size_t *);

static errcode_t
ubf_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_uuid_block_fetcher_t *fetcher;

  fetcher = (chop_uuid_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = uuid_block_fetch;
  fetcher->block_fetcher.index_handle_class = &chop_uuid_index_handle_class;

  return chop_log_init ("uuid-block-fetcher", &fetcher->log);
}

static void
ubf_dtor (chop_object_t *object)
{
  chop_uuid_block_fetcher_t *fetcher;

  fetcher = (chop_uuid_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = NULL;
  fetcher->block_fetcher.index_handle_class = NULL;

  chop_object_destroy ((chop_object_t *)&fetcher->log);
}

static errcode_t
ubf_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  /* Stateless.  */
  return 0;
}

static errcode_t
ubf_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;

  err = chop_object_initialize (object, &chop_uuid_block_fetcher_class);

  /* Stateless.  */
  *bytes_read = 0;

  return 0;
}

CHOP_DEFINE_RT_CLASS (uuid_block_fetcher, block_fetcher,
		      ubf_ctor, ubf_dtor,
		      NULL, NULL,
		      ubf_serialize, ubf_deserialize);

chop_log_t *
chop_uuid_block_fetcher_log (chop_block_fetcher_t *fetcher)
{
  chop_uuid_block_fetcher_t *hfetcher;

  if (!chop_object_is_a ((chop_object_t *)fetcher,
			 &chop_uuid_block_fetcher_class))
    return NULL;

  hfetcher = (chop_uuid_block_fetcher_t *)fetcher;
  return (&hfetcher->log);
}

static errcode_t
uuid_block_fetch (chop_block_fetcher_t *block_fetcher,
		  const chop_index_handle_t *index,
		  chop_block_store_t *store,
		  chop_buffer_t *buffer, size_t *size)
{
  errcode_t err;
  chop_uuid_index_handle_t *handle;
  chop_uuid_block_fetcher_t *fetcher;
  char uuid[CHOP_UUID_SIZE];
  chop_block_key_t key;

  fetcher = (chop_uuid_block_fetcher_t *)block_fetcher;
  if (!chop_object_is_a ((chop_object_t *)index,
			 &chop_uuid_index_handle_class))
    return CHOP_INVALID_ARG;

  handle = (chop_uuid_index_handle_t *)index;
  uuid_unparse (handle->uuid, uuid);
  chop_block_key_init (&key, uuid, CHOP_UUID_SIZE, NULL, NULL);

  err = chop_store_read_block (store, &key, buffer, size);
#if 0
  if (!err)
    {
      /* Did we get as much data as expected?  */
      if (*size != handle->block_size)
	{
	  char *hex;

	  hex = alloca (handle->key_size * 2 + 1);
	  chop_buffer_to_hex_string (handle->content, handle->key_size, hex);

	  chop_log_printf (&fetcher->log, "block %s: "
			   "got %u bytes instead of %u",
			   hex, *size, handle->block_size);

	  *size = 0;
	  return CHOP_BLOCK_INDEXER_ERROR;
	}
    }
#endif

  return err;
}


/* The indexer class.  */
CHOP_DECLARE_RT_CLASS (uuid_block_indexer, block_indexer,
		       /* Stateless.  */);

static errcode_t
uuid_indexer_init_fetcher (const chop_block_indexer_t *block_indexer,
			   chop_block_fetcher_t *fetcher)
{
  /* Our fetchers are stateless so there is nothing special to initialize
     here.  */
  return chop_object_initialize ((chop_object_t *)fetcher,
				 &chop_uuid_block_fetcher_class);
}

static errcode_t
uuid_block_index (chop_block_indexer_t *indexer,
		  chop_block_store_t *store,
		  const char *buffer,
		  size_t size,
		  chop_index_handle_t *handle);

static errcode_t
ubi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_uuid_block_indexer_t *indexer;

  indexer = (chop_uuid_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = &chop_uuid_index_handle_class;
  indexer->block_indexer.block_fetcher_class = &chop_uuid_block_fetcher_class;
  indexer->block_indexer.index_block = uuid_block_index;
  indexer->block_indexer.init_fetcher = uuid_indexer_init_fetcher;

  return 0;
}

static void
ubi_dtor (chop_object_t *object)
{
  chop_uuid_block_indexer_t *indexer;

  indexer = (chop_uuid_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = NULL;
  indexer->block_indexer.block_fetcher_class = NULL;
}

static errcode_t
ubi_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  /* Stateless.  */
  return 0;
}

static errcode_t
ubi_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;

  *bytes_read = 0;

  err = chop_object_initialize (object, &chop_uuid_block_indexer_class);
  if (err)
    return err;

  /* Stateless.  */

  return 0;
}

CHOP_DEFINE_RT_CLASS (uuid_block_indexer, block_indexer,
		      ubi_ctor, ubi_dtor,
		      NULL, NULL,
		      ubi_serialize, ubi_deserialize);


static errcode_t
uuid_block_index (chop_block_indexer_t *indexer,
		  chop_block_store_t *store,
		  const char *buffer,
		  size_t size,
		  chop_index_handle_t *handle)
{
  errcode_t err;
  chop_uuid_block_indexer_t *uuid_indexer;
  chop_uuid_index_handle_t *uuid_handle;
  char uuid[CHOP_UUID_SIZE];
  chop_block_key_t key;

  uuid_indexer = (chop_uuid_block_indexer_t *)indexer;
  err = chop_object_initialize ((chop_object_t *)handle,
				&chop_uuid_index_handle_class);
  if (err)
    return err;

  uuid_handle = (chop_uuid_index_handle_t *)handle;

  /* Compute an identifier for BUFFER.  */
  uuid_generate (uuid_handle->uuid);
  uuid_unparse (uuid_handle->uuid, uuid);

  chop_block_key_init (&key, uuid, CHOP_UUID_SIZE, NULL, NULL);

  /* Write BUFFER to the backing store using this identifier.  */
  err = chop_store_write_block (store, &key, buffer, size);
  if (err)
    chop_object_destroy ((chop_object_t *)handle);
  else
    uuid_handle->index_handle.size = CHOP_UUID_SIZE;

  return err;
}

errcode_t
chop_uuid_block_indexer_open (chop_block_indexer_t *indexer)
{
  return chop_object_initialize ((chop_object_t *)indexer,
				 &chop_uuid_block_indexer_class);
}


/* The following GNU Arch identifier was actually computed by `uuidgen', from
   libuuid.  */

/* arch-tag: 19ac0cb7-596d-40b7-b93e-50e922bd9f78
 */
