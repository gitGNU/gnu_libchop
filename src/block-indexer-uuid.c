/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2011, 2012, 2013  Ludovic Courtès <ludo@gnu.org>
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

/* A block indexer that generates DCE compatible Universally Unique
   Identifiers using libuuid.  A lot of code is borrowed from
   `block-indexer-hash.c'.

   This block indexer, unlike the `hash' and `chk' block indexers, does not
   have the single-instance storage property since every block automatically
   gets assigned a new ID.  */

#include <chop/chop-config.h>

#include <alloca.h>

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

static chop_error_t
uih_copy (const chop_object_t *s, chop_object_t *d)
{
  chop_uuid_index_handle_t *source, *dest;

  source = (chop_uuid_index_handle_t *)s;
  dest = (chop_uuid_index_handle_t *)d;

  uuid_copy (dest->uuid, source->uuid);
  dest->index_handle.size = CHOP_UUID_SIZE;

  return 0;
}

static chop_error_t
uih_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_error_t err;
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
      err = chop_buffer_push (buffer, out, CHOP_UUID_SIZE);
      break;

    default:
      err = CHOP_ERR_NOT_IMPL;
    }

  return err;
}

static chop_error_t
uih_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;
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
	{
	  chop_object_destroy (object);
	  return CHOP_DESERIAL_TOO_SHORT;
	}

      if (uuid_parse (buffer, uuid->uuid))
	{
	  chop_object_destroy (object);
	  return CHOP_DESERIAL_CORRUPT_INPUT;
	}

      uuid->index_handle.size = CHOP_UUID_SIZE;
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

static chop_error_t uuid_block_fetch (chop_block_fetcher_t *,
				      const chop_index_handle_t *,
				      chop_block_store_t *,
				      chop_buffer_t *,
				      size_t *);
static chop_error_t uuid_blocks_exist (chop_block_fetcher_t *,
				       size_t n,
				       const chop_index_handle_t *h[n],
				       chop_block_store_t *,
				       bool e[n]);

static chop_error_t
ubf_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_uuid_block_fetcher_t *fetcher;

  fetcher = (chop_uuid_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = uuid_block_fetch;
  fetcher->block_fetcher.blocks_exist = uuid_blocks_exist;
  fetcher->block_fetcher.index_handle_class = &chop_uuid_index_handle_class;

  return chop_log_init ("uuid-block-fetcher", &fetcher->log);
}

static void
ubf_dtor (chop_object_t *object)
{
  chop_uuid_block_fetcher_t *fetcher;

  fetcher = (chop_uuid_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = NULL;
  fetcher->block_fetcher.blocks_exist = NULL;
  fetcher->block_fetcher.index_handle_class = NULL;

  chop_object_destroy ((chop_object_t *)&fetcher->log);
}

static chop_error_t
ubf_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  /* Stateless.  */
  return 0;
}

static chop_error_t
ubf_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;

  err = chop_object_initialize (object, &chop_uuid_block_fetcher_class);

  /* Stateless.  */
  *bytes_read = 0;

  return err;
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

static chop_error_t
uuid_blocks_exist (chop_block_fetcher_t *block_fetcher,
		   size_t n,
		   const chop_index_handle_t *indices[n],
		   chop_block_store_t *store,
		   bool exists[n])
{
  size_t i;
  chop_block_key_t keys[n];

  for (i = 0; i < n; i++)
    {
      if (!chop_object_is_a ((chop_object_t *) indices[i],
			     &chop_uuid_index_handle_class))
	return CHOP_INVALID_ARG;

      chop_uuid_index_handle_t *handle;
      handle = (chop_uuid_index_handle_t *) indices[i];

      char uuid[CHOP_UUID_SIZE];
      uuid_unparse (handle->uuid, uuid);

      chop_block_key_init (&keys[i], uuid, sizeof uuid, NULL, NULL);
    }

  return chop_store_blocks_exist (store, n, keys, exists);
}

static chop_error_t
uuid_block_fetch (chop_block_fetcher_t *block_fetcher,
		  const chop_index_handle_t *index,
		  chop_block_store_t *store,
		  chop_buffer_t *buffer, size_t *size)
{
  chop_error_t err;
  chop_uuid_index_handle_t *handle;
  char uuid[CHOP_UUID_SIZE];
  chop_block_key_t key;

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

static chop_error_t
uuid_indexer_init_fetcher (const chop_block_indexer_t *block_indexer,
			   chop_block_fetcher_t *fetcher)
{
  /* Our fetchers are stateless so there is nothing special to initialize
     here.  */
  return chop_object_initialize ((chop_object_t *)fetcher,
				 &chop_uuid_block_fetcher_class);
}

static chop_error_t
uuid_block_index (chop_block_indexer_t *indexer,
		  chop_block_store_t *store,
		  const char *buffer,
		  size_t size,
		  chop_index_handle_t *handle);

static chop_error_t
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

static chop_error_t
ubi_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  /* Stateless.  */
  return 0;
}

static chop_error_t
ubi_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  /* Stateless.  */
  *bytes_read = 0;

  return (chop_object_initialize (object, &chop_uuid_block_indexer_class));
}

CHOP_DEFINE_RT_CLASS (uuid_block_indexer, block_indexer,
		      ubi_ctor, ubi_dtor,
		      NULL, NULL,
		      ubi_serialize, ubi_deserialize);


static chop_error_t
uuid_block_index (chop_block_indexer_t *indexer,
		  chop_block_store_t *store,
		  const char *buffer,
		  size_t size,
		  chop_index_handle_t *handle)
{
  chop_error_t err;
  chop_uuid_index_handle_t *uuid_handle;
  char uuid[CHOP_UUID_SIZE];
  chop_block_key_t key;

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

chop_error_t
chop_uuid_block_indexer_open (chop_block_indexer_t *indexer)
{
  return chop_object_initialize ((chop_object_t *)indexer,
				 &chop_uuid_block_indexer_class);
}


/* The following GNU Arch identifier was actually computed by `uuidgen', from
   libuuid.  */

/* arch-tag: 19ac0cb7-596d-40b7-b93e-50e922bd9f78
 */

