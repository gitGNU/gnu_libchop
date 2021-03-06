/* libchop -- a utility library for distributed storage and data backup
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

/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <errno.h>
#include <assert.h>

#ifdef DEBUG
# include <stdio.h>
#endif



/* Constructors.  */

static inline chop_error_t
chop_block_indexer_make_fetcher_alloc (chop_block_indexer_t *indexer,
				       chop_block_fetcher_t **fetcher)
{
  chop_error_t err;
  const chop_class_t *fetcher_class;

  fetcher_class = chop_block_indexer_fetcher_class (indexer);
  *fetcher =
    (chop_block_fetcher_t *)gwrap_chop_malloc (fetcher_class);

  err = chop_block_indexer_initialize_fetcher (indexer, *fetcher);
  if (err)
    gwrap_chop_free_uninitialized ((chop_object_t *) *fetcher,
				   fetcher_class);

  return err;
}

static inline chop_error_t
chop_hash_block_indexer_open_alloc (chop_hash_method_t hash_method,
				    chop_block_indexer_t **bi)
{
  chop_error_t err;

  *bi = gwrap_chop_malloc (&chop_hash_block_indexer_class);
  err = chop_hash_block_indexer_open (hash_method, *bi);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *bi,
				     &chop_hash_block_indexer_class);
      *bi = NULL;
    }

  return err;
}

static inline chop_error_t
chop_chk_block_indexer_open_alloc (chop_cipher_handle_t cipher_handle,
				   chop_hash_method_t key_hash_method,
				   chop_hash_method_t block_id_hash_method,
				   chop_block_indexer_t **bi)
{
  chop_error_t err;

  *bi = gwrap_chop_malloc (&chop_chk_block_indexer_class);
  err = chop_chk_block_indexer_open (cipher_handle,
				     0, /* let the GC free it when needed */
				     key_hash_method,
				     block_id_hash_method,
				     *bi);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *bi,
				     &chop_chk_block_indexer_class);
      *bi = NULL;
    }

  return err;
}

static inline chop_error_t
chop_uuid_block_indexer_open_alloc (chop_block_indexer_t **bi)
{
#ifdef HAVE_LIBUUID

  chop_error_t err;

  *bi = gwrap_chop_malloc (&chop_uuid_block_indexer_class);
  err = chop_uuid_block_indexer_open (*bi);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *bi,
				     &chop_uuid_block_indexer_class);
      *bi = NULL;
    }

  return err;

#else /* !HAVE_LIBUUID */

  *bi = NULL;

  return CHOP_ERR_NOT_IMPL;

#endif /* !HAVE_LIBUUID */
}

static inline chop_error_t
chop_integer_block_indexer_open_alloc (unsigned long start,
				       chop_block_indexer_t **bi)
{
  chop_error_t err;

  *bi = gwrap_chop_malloc (&chop_integer_block_indexer_class);
  err = chop_integer_block_indexer_open (start, *bi);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *bi,
				     &chop_integer_block_indexer_class);
      *bi = NULL;
    }

  return err;
}


/* Methods.  */

static inline chop_error_t
chop_block_indexer_index_alloc (chop_block_indexer_t *block_indexer,
				chop_block_store_t *store,
				const char *buffer, size_t size,
				chop_index_handle_t **handle)
{
  chop_error_t err;
  const chop_class_t *handle_class;

  handle_class = chop_block_indexer_index_handle_class (block_indexer);
  *handle = gwrap_chop_malloc (handle_class);
  err = chop_block_indexer_index (block_indexer, store,
				  buffer, size, *handle);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *handle,
				     handle_class);
      *handle = NULL;
    }

  return err;
}

static inline chop_error_t
chop_block_fetcher_fetch_alloc_u8vector (chop_block_fetcher_t *block_fetcher,
					 const chop_index_handle_t *index,
					 chop_block_store_t *store,
					 SCM *vector)
{
  chop_error_t err;
  size_t size;
  chop_buffer_t buffer;

  err = chop_buffer_init (&buffer, 0);
  if (err)
    return err;

  err = chop_block_fetcher_fetch (block_fetcher, index, store,
				  &buffer, &size);
  if (err)
    {
      chop_buffer_return (&buffer);
      *vector = SCM_BOOL_F;

      return err;
    }

  assert (size == chop_buffer_size (&buffer));
  if (!size)
    *vector = SCM_BOOL_F;
  else
    {
      scm_t_uint8 *block = (scm_t_uint8 *)scm_malloc (size);

      memcpy (block, chop_buffer_content (&buffer), size);
      *vector = scm_take_u8vector (block, size);
    }

  chop_buffer_return (&buffer);

  return err;
}


/* Convenience functions.  */

static inline chop_error_t
chop_index_handle_ascii_serialize (const chop_index_handle_t *handle,
				   char **serialization)
{
  chop_error_t err;
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

static inline chop_error_t
chop_index_handle_ascii_deserialize (const chop_class_t *handle_class,
				     const char *ascii_handle,
				     chop_index_handle_t **handle)
{
  chop_error_t err;
  size_t bytes_read;

  *handle = gwrap_chop_malloc (handle_class);

#ifdef DEBUG
  fprintf (stderr, "%s: deserializing index handle, class `%s', size %u\n",
	   __FUNCTION__,
	   chop_class_name (handle_class),
	   chop_class_instance_size (handle_class));
#endif

  err = chop_object_deserialize ((chop_object_t *)*handle, handle_class,
				 CHOP_SERIAL_ASCII,
				 ascii_handle, strlen (ascii_handle),
				 &bytes_read);
  if (CHOP_EXPECT_FALSE (err))
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *handle,
				     handle_class);
      *handle = NULL;
    }

  return err;
}

/* arch-tag: ee8a3378-553e-4f27-8b74-1d14ad29edba
 */
