/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2011  Ludovic Courtès <ludo@gnu.org>
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

/* Content-based addressing, also sometimes referred to as
   ``compare-by-hash''.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/block-indexers.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>



/* The index handle class.  */

CHOP_DECLARE_RT_CLASS (hash_index_handle, index_handle,
		       size_t block_size; /* size of the indexed block */
		       size_t key_size;   /* size of the block key */
		       char content[1024];/* the block key */);

static int
hih_equalp (const chop_object_t *h1, const chop_object_t *h2)
{
  chop_hash_index_handle_t *hih1, *hih2;

  hih1 = (chop_hash_index_handle_t *)h1;
  hih2 = (chop_hash_index_handle_t *)h2;

  return ((hih1->block_size == hih2->block_size)
	  && (hih1->key_size == hih2->key_size)
	  && (!memcmp (hih1->content, hih2->content, hih1->key_size)));
}

/* Define this to enable the use of magic bytes in the header of serialized
   `hash_index_handle' objects.  This may help find out the cause of
   consistency problems.  */
#undef BINARY_SERIALIZATION_USES_MAGIC_BYTES


#ifndef BINARY_SERIALIZATION_USES_MAGIC_BYTES
# define BINARY_SERIALIZATION_HEADER_SIZE  (8)
#else
# define BINARY_SERIALIZATION_MAGIC        "HiH"
# define BINARY_SERIALIZATION_MAGIC_SIZE   4 /* include trailing \0 */
# define BINARY_SERIALIZATION_HEADER_SIZE  (12)
#endif


static chop_error_t
hih_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_hash_index_handle_t *handle =
    (chop_hash_index_handle_t *)object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	/* Return something like "n57k2jlivui6pufs6qijfqsfbyoqryy6/44e".  */
	char *serial;

	serial = alloca ((handle->key_size * 2) + 1);
	chop_buffer_to_base32_string (handle->content, handle->key_size,
				      serial);
	chop_buffer_push (buffer, serial, strlen (serial) /* strip trailing \0 */);

	/* Append a slash and the indexed block size.  */
	serial[0] = '/';
	sprintf (serial + 1, "%zx", handle->block_size);
	chop_buffer_append (buffer, serial, strlen (serial) + 1);

	return 0;
      }

    case CHOP_SERIAL_BINARY:
      {
	chop_error_t err;
	size_t orig_size;
	unsigned char size[8];

#ifdef BINARY_SERIALIZATION_USES_MAGIC_BYTES
	err = chop_buffer_push (buffer, BINARY_SERIALIZATION_MAGIC,
				BINARY_SERIALIZATION_MAGIC_SIZE);
	if (err)
	  return err;
#endif

	orig_size = handle->block_size;
	size[0] = orig_size & 0xff;  orig_size >>= 8;
	size[1] = orig_size & 0xff;  orig_size >>= 8;
	size[2] = orig_size & 0xff;  orig_size >>= 8;
	size[3] = orig_size & 0xff;  orig_size >>= 8;
	assert (!orig_size);

	orig_size = handle->key_size;
	size[4] = orig_size & 0xff;  orig_size >>= 8;
	size[5] = orig_size & 0xff;  orig_size >>= 8;
	size[6] = orig_size & 0xff;  orig_size >>= 8;
	size[7] = orig_size & 0xff;  orig_size >>= 8;
	assert (!orig_size);

#ifndef BINARY_SERIALIZATION_USES_MAGIC_BYTES
	chop_buffer_push (buffer, (char *)size, sizeof (size));
#else
	chop_buffer_append (buffer, (char *)size, sizeof (size));
#endif

	err = chop_buffer_append (buffer, handle->content, handle->key_size);

	return err;
      }

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return CHOP_ERR_NOT_IMPL;
}

static chop_error_t
hih_deserialize (const char *s_buffer, size_t size,
		 chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;
  const unsigned char *buffer = (unsigned char *)s_buffer;
  chop_hash_index_handle_t *handle =
    (chop_hash_index_handle_t *)object;

  *bytes_read = 0;
  err = chop_object_initialize (object, &chop_hash_index_handle_class);
  if (err)
    return err;

  switch (method)
    {
      case CHOP_SERIAL_ASCII:
	{
	  /* Avoid a stack overflow with the INPUT array below.  */
	  size = size > 1023 ? 1023 : size;

	  char input[size + 1];
	  char *slash;
	  const char *end;

	  /* Copy BUFFER locally and add a trailing zero to make sure we
	     don't read past the end.  */
	  memcpy (input, buffer, size);
	  input[size] = '\0';

	  slash = strchr ((char *) input, '/');
	  if (!slash)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  /* Read the block ID.  */
	  assert (slash - input <= sizeof (handle->content));
	  handle->key_size =
	    chop_base32_string_to_buffer (input, slash - input,
					  handle->content, &end);
	  if (end != slash)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  end++; /* skip the slash */

	  {
	    /* Read the block size.  */
	    unsigned long int block_size;
	    const char *start;

	    start = end;

	    block_size = strtoul ((char *)start, (char **)&end, 16);
	    if (end == start)
	      err = CHOP_DESERIAL_CORRUPT_INPUT;
	    else if (block_size < 0)
	      err = CHOP_DESERIAL_CORRUPT_INPUT;
	    /* FIXME:  Check for possible overflow.  */
	    else
	      handle->block_size = block_size;
	  }

	  if (!err)
	    *bytes_read = end - input;

	  break;
	}

    case CHOP_SERIAL_BINARY:
      {
	size_t block_size, index_size;

	/* The serialized thing has to contain at least 4 bytes representing
	   the size of the index itself and 4 bytes representing the size of
	   the addressed block.  */
	if (size < BINARY_SERIALIZATION_HEADER_SIZE)
	  return CHOP_DESERIAL_CORRUPT_INPUT;

#ifdef BINARY_SERIALIZATION_USES_MAGIC_BYTES
	if (memcmp (buffer, BINARY_SERIALIZATION_MAGIC,
		    BINARY_SERIALIZATION_MAGIC_SIZE))
	  {
	    printf ("didn't find hih magic bytes\n");
	    return CHOP_DESERIAL_CORRUPT_INPUT;
	  }

	buffer += BINARY_SERIALIZATION_MAGIC_SIZE;
#endif

	block_size  = buffer[3]; block_size <<= 8;
	block_size |= buffer[2]; block_size <<= 8;
	block_size |= buffer[1]; block_size <<= 8;
	block_size |= buffer[0];
	handle->block_size = block_size;

	index_size  = buffer[7]; index_size <<= 8;
	index_size |= buffer[6]; index_size <<= 8;
	index_size |= buffer[5]; index_size <<= 8;
	index_size |= buffer[4];
	handle->key_size = index_size;

	if (size - BINARY_SERIALIZATION_HEADER_SIZE < index_size)
	  {
	    printf ("expecting block size %zu and "
		    "index size %zu but only %zu bytes left",
		    block_size, index_size,
		    size - BINARY_SERIALIZATION_HEADER_SIZE);
	    err = CHOP_DESERIAL_TOO_SHORT;
	  }
	else
	  {
	    memcpy (handle->content, buffer + 8, index_size);
	    *bytes_read = index_size + 8;
#ifdef BINARY_SERIALIZATION_USES_MAGIC_BYTES
	    *bytes_read += BINARY_SERIALIZATION_MAGIC_SIZE;
#endif
	  }

	break;
      }

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  if (!err)
    /* The size of the binary representation of that handle: this includes
       the size of the `block_size' and `key_size' fields, currently 8
       bytes.  */
    handle->index_handle.size = handle->key_size
      + BINARY_SERIALIZATION_HEADER_SIZE;

  return err;
}

/* Note: the default copy constructor will work fine.  */

CHOP_DEFINE_RT_CLASS (hash_index_handle, index_handle,
		      NULL, NULL,
		      NULL, hih_equalp,
		      hih_serialize, hih_deserialize);


/* The fetcher class.  */
CHOP_DECLARE_RT_CLASS (hash_block_fetcher, block_fetcher,
		       chop_hash_method_t hash_method;
		       chop_log_t log;);

static chop_error_t hash_block_fetch (chop_block_fetcher_t *,
				      const chop_index_handle_t *,
				      chop_block_store_t *,
				      chop_buffer_t *,
				      size_t *);

static chop_error_t
hbf_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_hash_block_fetcher_t *fetcher;

  fetcher = (chop_hash_block_fetcher_t *) object;
  fetcher->block_fetcher.fetch_block = hash_block_fetch;
  fetcher->block_fetcher.block_exists = NULL;
  fetcher->block_fetcher.index_handle_class = &chop_hash_index_handle_class;

  fetcher->hash_method = CHOP_HASH_NONE;

  return chop_log_init ("hash-block-fetcher", &fetcher->log);
}

static void
hbf_dtor (chop_object_t *object)
{
  chop_hash_block_fetcher_t *fetcher;

  fetcher = (chop_hash_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = NULL;
  fetcher->block_fetcher.block_exists = NULL;
  fetcher->block_fetcher.index_handle_class = NULL;

  chop_object_destroy ((chop_object_t *)&fetcher->log);
}

static chop_error_t
hbf_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_error_t err;
  chop_hash_block_fetcher_t *fetcher;

  fetcher = (chop_hash_block_fetcher_t *) object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      if (fetcher->hash_method != CHOP_HASH_NONE)
	{
	  char *name, *p;

	  name = strdupa (chop_hash_method_name (fetcher->hash_method));
	  for (p = name; *p; p++)
	    *p = tolower (*p);

	  err = chop_buffer_push (buffer, name, strlen (name) + 1);
	}
      else
	err = 0;
      break;

    default:
      err = CHOP_ERR_NOT_IMPL;
    }

  return err;
}

static chop_error_t
hbf_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;
  chop_hash_block_fetcher_t *fetcher;

  fetcher = (chop_hash_block_fetcher_t *) object;

  err = chop_object_initialize (object, &chop_hash_block_fetcher_class);

  if (err == 0)
    switch (method)
      {
      case CHOP_SERIAL_ASCII:
	{
	  const char *bound;

	  /* Get a non-graph token.  */
	  for (bound = buffer;
	       *bound && (!ispunct (*bound)) && (!isspace (*bound));
	       bound++);

	  if (bound != buffer)
	    {
	      /* Hash method information is available.  */
	      char name[bound - buffer + 1];
	      chop_hash_method_t hash_method;

	      strncpy (name, buffer, bound - buffer);
	      name[bound - buffer] = '\0';

	      err = chop_hash_method_lookup (name, &hash_method);
	      if (err)
		err = CHOP_DESERIAL_CORRUPT_INPUT;
	      else
		{
		  fetcher->hash_method = hash_method;
		  *bytes_read = bound - buffer;
		}
	    }
	  else
	    /* Hash method is unknown, so integrity checks won't be
	       performed.  */
	    *bytes_read = 0;

	  break;
	}

      default:
	err = CHOP_ERR_NOT_IMPL;
      }

  return err;
}

CHOP_DEFINE_RT_CLASS (hash_block_fetcher, block_fetcher,
		      hbf_ctor, hbf_dtor,
		      NULL, NULL,
		      hbf_serialize, hbf_deserialize);

chop_log_t *
chop_hash_block_fetcher_log (chop_block_fetcher_t *fetcher)
{
  chop_hash_block_fetcher_t *hfetcher;

  if (!chop_object_is_a ((chop_object_t *)fetcher,
			 &chop_hash_block_fetcher_class))
    return NULL;

  hfetcher = (chop_hash_block_fetcher_t *)fetcher;
  return (&hfetcher->log);
}

static chop_error_t
hash_block_fetch (chop_block_fetcher_t *block_fetcher,
		  const chop_index_handle_t *index,
		  chop_block_store_t *store,
		  chop_buffer_t *buffer, size_t *size)
{
  chop_error_t err;
  chop_hash_index_handle_t *handle;
  chop_hash_block_fetcher_t *fetcher;
  chop_block_key_t key;

  fetcher = (chop_hash_block_fetcher_t *)block_fetcher;
  if (!chop_object_is_a ((chop_object_t *)index,
			 &chop_hash_index_handle_class))
    return CHOP_INVALID_ARG;

  handle = (chop_hash_index_handle_t *)index;
  chop_block_key_init (&key, handle->content, handle->key_size,
		       NULL, NULL);

  err = chop_store_read_block (store, &key, buffer, size);
  if (!err)
    {
      char key_hex[handle->key_size * 2 + 1];
      chop_buffer_to_hex_string (handle->content, handle->key_size, key_hex);


      /* Did we get as much data as expected?  */
      if (*size != handle->block_size)
	{
	  chop_log_printf (&fetcher->log, "block %s: "
			   "got %zu bytes instead of %zu",
			   key_hex, *size, handle->block_size);

	  *size = 0;
	  err = CHOP_BLOCK_INDEXER_ERROR;
	}
      /* Has the block been tampered with?  */
      else if (fetcher->hash_method != CHOP_HASH_NONE)
	{
	  if (handle->key_size == chop_hash_size (fetcher->hash_method))
	    {
	      char hash[chop_hash_size (fetcher->hash_method)];

	      chop_hash_buffer (fetcher->hash_method,
				chop_buffer_content (buffer),
				chop_buffer_size (buffer),
				hash);
	      if (memcmp (hash, handle->content,
			  chop_hash_size (fetcher->hash_method)))
		{
		  chop_log_printf (&fetcher->log, "block %s: "
				   "wrong `%s' hash", key_hex,
				   chop_hash_method_name (fetcher->hash_method));

		  *size = 0;
		  err = CHOP_BLOCK_INDEXER_ERROR;
		}
	    }
	  else
	    {
	      chop_log_printf (&fetcher->log, "block %s: "
			       "wrong `%s' key size (got %zi; expected %zi)",
			       key_hex,
			       chop_hash_method_name (fetcher->hash_method),
			       handle->key_size,
			       chop_hash_size (fetcher->hash_method));
	      err = CHOP_BLOCK_INDEXER_ERROR;
	    }
	}
      else
	chop_log_printf (&fetcher->log, "block %s: hash method unknown, "
			 "cannot check integrity", key_hex);
    }

  return err;
}


/* The indexer class.  */
CHOP_DECLARE_RT_CLASS (hash_block_indexer, block_indexer,
		       chop_hash_method_t hash_method;);

static chop_error_t
hash_indexer_init_fetcher (const chop_block_indexer_t *block_indexer,
			   chop_block_fetcher_t *fetcher)
{
  chop_error_t err;
  chop_hash_block_indexer_t *hash_indexer =
    (chop_hash_block_indexer_t *) block_indexer;
  chop_hash_block_fetcher_t *hash_fetcher =
    (chop_hash_block_fetcher_t *) fetcher;

  err = chop_object_initialize ((chop_object_t *) fetcher,
				&chop_hash_block_fetcher_class);

  /* Tell FETCHER which hash method is used so it can check block integrity
     upon retrieval.  */
  hash_fetcher->hash_method = hash_indexer->hash_method;

  return err;
}

static chop_error_t
hash_block_index (chop_block_indexer_t *indexer,
		  chop_block_store_t *store,
		  const char *buffer,
		  size_t size,
		  chop_index_handle_t *handle);

static chop_error_t
hbi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_hash_block_indexer_t *indexer;

  indexer = (chop_hash_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = &chop_hash_index_handle_class;
  indexer->block_indexer.block_fetcher_class = &chop_hash_block_fetcher_class;
  indexer->block_indexer.index_block = hash_block_index;
  indexer->block_indexer.init_fetcher = hash_indexer_init_fetcher;

  indexer->hash_method = CHOP_HASH_NONE;

  return 0;
}

static void
hbi_dtor (chop_object_t *object)
{
  chop_hash_block_indexer_t *indexer;

  indexer = (chop_hash_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = NULL;
  indexer->block_indexer.block_fetcher_class = NULL;

  indexer->hash_method = CHOP_HASH_NONE;
}

static chop_error_t
hbi_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_hash_block_indexer_t *indexer;

  indexer = (chop_hash_block_indexer_t *)object;
  switch (method)
    {
      case CHOP_SERIAL_ASCII:
	{
	  const char *hash_method;

	  hash_method = chop_hash_method_name (indexer->hash_method);
	  if (!hash_method)
	    return CHOP_INVALID_ARG;

	  chop_buffer_push (buffer, hash_method, strlen (hash_method) + 1);
	  break;
	}

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

static chop_error_t
hbi_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;
  chop_hash_block_indexer_t *indexer;

  indexer = (chop_hash_block_indexer_t *)object;
  err = chop_object_initialize (object, &chop_hash_block_indexer_class);
  if (err)
    return err;

  switch (method)
    {
      case CHOP_SERIAL_ASCII:
	{
	  char name[512];
	  size_t name_len = 0;
	  const char *end = buffer;

	  while (isalnum (*end))
	    {
	      if (name_len >= sizeof (name))
		return CHOP_DESERIAL_CORRUPT_INPUT;

	      name[name_len++] = *end;
	      end++;
	    }

	  *bytes_read = name_len;
	  name[name_len] = '\0';

	  if (chop_hash_method_lookup (name, &indexer->hash_method))
	    return CHOP_INVALID_ARG;

	  break;
	}

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

CHOP_DEFINE_RT_CLASS (hash_block_indexer, block_indexer,
		      hbi_ctor, hbi_dtor,
		      NULL, NULL,
		      hbi_serialize, hbi_deserialize);


static chop_error_t
hash_block_index (chop_block_indexer_t *indexer,
		  chop_block_store_t *store,
		  const char *buffer,
		  size_t size,
		  chop_index_handle_t *handle)
{
  chop_error_t err;
  chop_hash_block_indexer_t *hash_indexer;
  chop_hash_index_handle_t *hash_handle;
  chop_block_key_t key;
  size_t hash_size;

  hash_indexer = (chop_hash_block_indexer_t *)indexer;
  err = chop_object_initialize ((chop_object_t *)handle,
				&chop_hash_index_handle_class);
  if (err)
    return err;

  hash_handle = (chop_hash_index_handle_t *)handle;

  hash_size = chop_hash_size (hash_indexer->hash_method);
  if ((!hash_size) || (hash_size > sizeof (hash_handle->content)))
    return CHOP_INVALID_ARG;

  /* Compute an identifier for BUFFER using the user-specified hash
     method.  */
  chop_hash_buffer (hash_indexer->hash_method, buffer, size,
		    hash_handle->content);
  hash_handle->key_size = hash_size;
  chop_block_key_init (&key, hash_handle->content,
		       hash_handle->key_size, NULL, NULL);

  /* Write BUFFER to the backing store using this identifier.  */
  err = chop_store_write_block (store, &key, buffer, size);
  if (err)
    chop_object_destroy ((chop_object_t *)handle);
  else
    {
      hash_handle->block_size = size;

      /* Again, the binary representation uses 8 bytes to store the
	 `block_size' field.  */
      hash_handle->index_handle.size =
	hash_size + BINARY_SERIALIZATION_HEADER_SIZE;
    }

  return err;
}

chop_error_t
chop_hash_block_indexer_open (chop_hash_method_t hash_method,
			      chop_block_indexer_t *indexer)
{
  chop_hash_block_indexer_t *hash_indexer;

  chop_object_initialize ((chop_object_t *)indexer,
			  &chop_hash_block_indexer_class);

  hash_indexer = (chop_hash_block_indexer_t *)indexer;
  hash_indexer->hash_method = hash_method;

  return 0;
}

/* arch-tag: 03d6d9af-7db4-488c-aea9-d5914240861c
 */
