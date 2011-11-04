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

/* Convergent encryption.  The resulting index is also referred to as a
   ``content hash key'' in GNUnet/FreeNet terms.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/block-indexers.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#ifdef HAVE_VALGRIND_MEMCHECK_H
# include <valgrind/memcheck.h>
#endif


/* The index handle class.  */

CHOP_DECLARE_RT_CLASS (chk_index_handle, index_handle,
		       size_t block_size; /* size of the indexed block */
		       size_t key_size;
		       char   key[1024];
		       size_t block_id_size;
		       char   block_id[1024];);

static int
chk_equalp (const chop_object_t *h1, const chop_object_t *h2)
{
  chop_chk_index_handle_t *chk1, *chk2;

  chk1 = (chop_chk_index_handle_t *)h1;
  chk2 = (chop_chk_index_handle_t *)h2;

  return ((chk1->block_size == chk2->block_size)
	  && (chk1->key_size == chk2->key_size)
	  && (chk1->block_id_size == chk2->block_id_size)
	  && (!memcmp (chk1->key, chk2->key, chk1->key_size))
	  && (!memcmp (chk1->block_id, chk2->block_id, chk1->block_id_size)));
}

#define BINARY_SERIALIZATION_HEADER_SIZE  12

static chop_error_t
chk_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_error_t err;
  chop_chk_index_handle_t *handle =
    (chop_chk_index_handle_t *)object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	/* Return something like
	   "772daoefadfptnzpfnawob5yke======,oq564axk7sz36sym4yj26vzetfbym3iv/75a".  */
	char *b32_key, *b32_block_id;

	b32_key = alloca ((handle->key_size * 2) + 1);
	b32_block_id = alloca ((handle->block_id_size * 2) + 1);
	chop_buffer_to_base32_string (handle->key, handle->key_size, b32_key);
	chop_buffer_to_base32_string (handle->block_id, handle->block_id_size,
				      b32_block_id);

	chop_buffer_push (buffer, b32_key,
			  strlen (b32_key) /* strip trailing \0 */);
	chop_buffer_append (buffer, ",", 1);
	chop_buffer_append (buffer, b32_block_id, strlen (b32_block_id));

	/* Append a slash and the indexed block size.  */
	b32_key[0] = '/';
	sprintf (b32_key + 1, "%zx", handle->block_size);
	err = chop_buffer_append (buffer, b32_key, strlen (b32_key) + 1);

	return err;
      }

    case CHOP_SERIAL_BINARY:
      {
	chop_error_t err;
	size_t orig_size;
	unsigned char size[12];

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

	orig_size = handle->block_id_size;
	size[8]  = orig_size & 0xff;  orig_size >>= 8;
	size[9]  = orig_size & 0xff;  orig_size >>= 8;
	size[10] = orig_size & 0xff;  orig_size >>= 8;
	size[11] = orig_size & 0xff;  orig_size >>= 8;
	assert (!orig_size);

	chop_buffer_push (buffer, (char *)size, sizeof (size));

	err = chop_buffer_append (buffer, handle->key, handle->key_size);
	if (err)
	  return err;

	err = chop_buffer_append (buffer, handle->block_id,
				  handle->block_id_size);

	return err;
      }

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return CHOP_ERR_NOT_IMPL;
}

static chop_error_t
chk_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;
  chop_chk_index_handle_t *handle =
    (chop_chk_index_handle_t *)object;

  *bytes_read = 0;
  err = chop_object_initialize (object, &chop_chk_index_handle_class);
  if (err)
    return err;

  /* Help Valgrind keep cool.  */
  memset (handle->block_id, 0, sizeof (handle->block_id));
  memset (handle->key, 0, sizeof (handle->key));

  switch (method)
    {
      case CHOP_SERIAL_ASCII:
	{
	  /* Avoid a stack overflow with the INPUT array below.  */
	  size = size > 1023 ? 1023 : size;

	  char input[size + 1];
	  char *comma, *slash;
	  const char *end;

	  /* Copy BUFFER locally and add a trailing zero to make sure we
	     don't read past the end.  */
	  memcpy (input, buffer, size);
	  input[size] = '\0';

	  comma = strchr (input, ',');
	  if (!comma)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  /* Read the key.  */
	  assert (comma - input <= sizeof (handle->key));
	  handle->key_size =
	    chop_base32_string_to_buffer (input, comma - input,
					  handle->key, &end);
	  if (end != comma)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  end++; /* skip the comma */

	  /* Read the block ID.  */
	  slash = strchr (comma, '/');
	  if (!slash)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  assert (slash - comma + 1 <= sizeof (handle->block_id));
	  handle->block_id_size =
	    chop_base32_string_to_buffer (end, slash - end,
					  handle->block_id, &end);
	  if (end != slash)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  end++; /* skip the slash */

	  {
	    /* Read the block size.  */
	    unsigned long int block_size;
	    const char *start;

	    start = end;

	    block_size = strtoul (start, (char **) &end, 16);
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
	const unsigned char *u_buffer = (unsigned char *)buffer;
	size_t block_size, key_size, block_id_size;

	/* The serialized thing has to contain at least 4 bytes representing
	   the size of the index itself, 4 bytes representing the size of
	   the addressed block, and 4 more bytes for the cipher key.  */
	if (size < BINARY_SERIALIZATION_HEADER_SIZE)
	  return CHOP_DESERIAL_CORRUPT_INPUT;

	block_size  = u_buffer[3]; block_size <<= 8;
	block_size |= u_buffer[2]; block_size <<= 8;
	block_size |= u_buffer[1]; block_size <<= 8;
	block_size |= u_buffer[0];
	handle->block_size = block_size;

	key_size  = u_buffer[7]; key_size <<= 8;
	key_size |= u_buffer[6]; key_size <<= 8;
	key_size |= u_buffer[5]; key_size <<= 8;
	key_size |= u_buffer[4];
	handle->key_size = key_size;

	block_id_size  = u_buffer[11]; block_id_size <<= 8;
	block_id_size |= u_buffer[10]; block_id_size <<= 8;
	block_id_size |= u_buffer[9];  block_id_size <<= 8;
	block_id_size |= u_buffer[8];
	handle->block_id_size = block_id_size;

	if (size - BINARY_SERIALIZATION_HEADER_SIZE < key_size + block_id_size)
	  err = CHOP_DESERIAL_TOO_SHORT;
	else
	  {
	    memcpy (handle->key, buffer + BINARY_SERIALIZATION_HEADER_SIZE,
		    key_size);
	    memcpy (handle->block_id,
		    buffer + BINARY_SERIALIZATION_HEADER_SIZE + key_size,
		    block_id_size);
	    *bytes_read = key_size +
	      block_id_size + BINARY_SERIALIZATION_HEADER_SIZE;
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

CHOP_DEFINE_RT_CLASS (chk_index_handle, index_handle,
		      NULL, NULL,
		      NULL, chk_equalp,
		      chk_serialize, chk_deserialize);



/* The fetcher class.  */

CHOP_DECLARE_RT_CLASS (chk_block_fetcher, block_fetcher,
		       chop_cipher_handle_t cipher_handle;
		       int owns_cipher_handle;
		       chop_hash_method_t block_id_hash_method;
		       chop_log_t log;);

static chop_error_t chk_block_fetch (chop_block_fetcher_t *,
				     const chop_index_handle_t *,
				     chop_block_store_t *,
				     chop_buffer_t *,
				     size_t *);

static chop_error_t
cbf_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_chk_block_fetcher_t *fetcher;

  fetcher = (chop_chk_block_fetcher_t *) object;
  fetcher->block_fetcher.fetch_block = chk_block_fetch;
  fetcher->block_fetcher.block_exists = NULL;
  fetcher->block_fetcher.index_handle_class = &chop_chk_index_handle_class;

  fetcher->cipher_handle = CHOP_CIPHER_HANDLE_NIL;
  fetcher->owns_cipher_handle = 0;
  fetcher->block_id_hash_method = CHOP_HASH_NONE;

  return chop_log_init ("chk-block-fetcher", &fetcher->log);
}

static void
cbf_dtor (chop_object_t *object)
{
  chop_chk_block_fetcher_t *fetcher;

  fetcher = (chop_chk_block_fetcher_t *)object;

  if (fetcher->owns_cipher_handle)
    chop_cipher_close (fetcher->cipher_handle);

  fetcher->cipher_handle = CHOP_CIPHER_HANDLE_NIL;
  fetcher->owns_cipher_handle = 0;

  chop_object_destroy ((chop_object_t *)&fetcher->log);
}

static chop_error_t
cbf_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_error_t err;
  chop_chk_block_fetcher_t *fetcher;
  chop_cipher_algo_t algo;
  chop_cipher_mode_t mode;
  char *algo_name, *mode_name;

  fetcher = (chop_chk_block_fetcher_t *) object;
  switch (method)
    {
      case CHOP_SERIAL_ASCII:
	{
	  char *p;

	  algo = chop_cipher_algorithm (fetcher->cipher_handle);
	  mode = chop_cipher_mode (fetcher->cipher_handle);

	  algo_name = strdupa (chop_cipher_algo_name (algo));
	  mode_name = strdupa (chop_cipher_mode_name (mode));

	  for (p = algo_name; *p; p++)
	    *p = tolower (*p);
	  for (p = mode_name; *p; p++)
	    *p = tolower (*p);

	  err = chop_buffer_push (buffer, algo_name, strlen (algo_name));
	  if (!err)
	    err = chop_buffer_append (buffer, ",", 1);
	  if (!err)
	    err = chop_buffer_append (buffer, mode_name,
				      strlen (mode_name));

	  if (fetcher->block_id_hash_method != CHOP_HASH_NONE)
	    {
	      char *block_id_hash;

	      block_id_hash =
		strdupa (chop_hash_method_name (fetcher->block_id_hash_method));
	      for (p = block_id_hash; *p; p++)
		*p = tolower (*p);

	      if (!err)
		err = chop_buffer_append (buffer, ",", 1);
	      if (!err)
		err = chop_buffer_append (buffer, block_id_hash,
					  strlen (block_id_hash));
	    }

	  err = chop_buffer_append (buffer, "\0", 1);

	  break;
	}

    default:
      err = CHOP_ERR_NOT_IMPL;
    }

  return err;
}

static chop_error_t
cbf_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;
  chop_chk_block_fetcher_t *fetcher;
  const char *comma, *punct;
  chop_cipher_algo_t algo;
  chop_cipher_mode_t mode;

  fetcher = (chop_chk_block_fetcher_t *) object;

  err = chop_object_initialize (object, &chop_chk_block_fetcher_class);
  if (err == 0)
    {
      switch (method)
	{
	case CHOP_SERIAL_ASCII:
	  {
	    char *name;

	    comma = (const char *)memchr (buffer, ',', size);
	    if (!comma)
	      return CHOP_DESERIAL_CORRUPT_INPUT;

	    name = alloca (comma - buffer + 1);
	    strncpy (name, buffer, comma - buffer);
	    name[comma - buffer] = '\0';

	    err = chop_cipher_algo_lookup (name, &algo);
	    if (err)
	      return CHOP_DESERIAL_CORRUPT_INPUT;

	    /* Get to the next non-graph.  */
	    for (punct = comma + 1;
		 *punct && (!ispunct (*punct)) && (!isspace (*punct));
		 punct++);

	    name = alloca (punct - comma);
	    strncpy (name, comma + 1, punct - comma - 1);
	    name[punct - comma - 1] = '\0';

	    err = chop_cipher_mode_lookup (name, &mode);
	    if (err)
	      return CHOP_DESERIAL_CORRUPT_INPUT;

	    /* Get to the next non-graph, if any.  */
	    for (comma = punct = punct + 1;
		 *punct && (!ispunct (*punct)) && (!isspace (*punct));
		 punct++);

	    if (punct != comma)
	      {
		/* Block ID hash method info is available.  */
		char name[punct - comma + 1];
		chop_hash_method_t hash_method;

		strncpy (name, comma, punct - comma);
		name[punct - comma] = '\0';

		err = chop_hash_method_lookup (name, &hash_method);
		if (err)
		  err = CHOP_DESERIAL_CORRUPT_INPUT;
		else
		  fetcher->block_id_hash_method = hash_method;
	      }

	    if (err == 0)
	      {
		fetcher->cipher_handle = chop_cipher_open (algo, mode);
		fetcher->owns_cipher_handle = 1;
		*bytes_read = punct - buffer;
	      }
	    else
	      *bytes_read = 0;

	    break;
	  }

	default:
	  err = CHOP_ERR_NOT_IMPL;
	}
    }

  return err;
}

CHOP_DEFINE_RT_CLASS (chk_block_fetcher, block_fetcher,
		      cbf_ctor, cbf_dtor,
		      NULL, NULL,
		      cbf_serialize, cbf_deserialize);

chop_log_t *
chop_chk_block_fetcher_log (chop_block_fetcher_t *fetcher)
{
  chop_chk_block_fetcher_t *cbf;

  if (!chop_object_is_a ((chop_object_t *) fetcher,
			 &chop_chk_block_fetcher_class))
    return NULL;

  cbf = (chop_chk_block_fetcher_t *) fetcher;
  return (&cbf->log);
}

static chop_error_t
chk_block_fetch (chop_block_fetcher_t *block_fetcher,
		 const chop_index_handle_t *index,
		 chop_block_store_t *store,
		 chop_buffer_t *buffer, size_t *size)
{
  chop_error_t err;
  chop_chk_index_handle_t *handle;
  chop_chk_block_fetcher_t *fetcher;
  chop_buffer_t ciphertext;
  chop_block_key_t key;
  char *key_hex;

  fetcher = (chop_chk_block_fetcher_t *)block_fetcher;
  if (!chop_object_is_a ((chop_object_t *)index,
			 &chop_chk_index_handle_class))
    return CHOP_INVALID_ARG;

  handle = (chop_chk_index_handle_t *) index;
  chop_block_key_init (&key, handle->block_id, handle->block_id_size,
		       NULL, NULL);

  key_hex = alloca (handle->block_id_size * 2 + 1);
  chop_buffer_to_hex_string (handle->block_id, handle->block_id_size,
			     key_hex);

  err = chop_buffer_init (&ciphertext, 0);
  if (err)
    return err;

  err = chop_store_read_block (store, &key, &ciphertext, size);
  if (!err)
    {
      chop_cipher_algo_t cipher_algo;

      /* Did we get as much data as expected?  Note that indexing may pad the
	 original input block with zeros (so that the input is suitable to
	 the ciphering algorithm) so *SIZE may be slightly larger than
	 HANDLE->BLOCK_SIZE.  */
      if (*size < handle->block_size)
	{
	  chop_log_printf (&fetcher->log, "block %s: "
			   "got %zu bytes instead of %zu",
			   key_hex, *size, handle->block_size);

	  *size = 0;
	  err = CHOP_BLOCK_FETCHER_ERROR;

	  goto finish;
	}
      /* Has the block been tampered with?  */
      else if (fetcher->block_id_hash_method != CHOP_HASH_NONE)
	{
	  size_t hash_size;
	  const char *hash_name;

	  hash_size = chop_hash_size (fetcher->block_id_hash_method);
	  hash_name = chop_hash_method_name (fetcher->block_id_hash_method);

	  if (handle->block_id_size == hash_size)
	    {
	      /* Check data integrity.  */
	      char hash[hash_size];

	      chop_hash_buffer (fetcher->block_id_hash_method,
				chop_buffer_content (&ciphertext),
				chop_buffer_size (&ciphertext),
				hash);
	      if (memcmp (hash, handle->block_id, hash_size))
		{
		  chop_log_printf (&fetcher->log, "block %s: "
				   "wrong `%s' hash", key_hex, hash_name);

		  *size = 0;
		  err = CHOP_BLOCK_FETCHER_ERROR;
		  goto finish;
		}
	    }
	  else
	    {
	      chop_log_printf (&fetcher->log, "block %s: "
			       "wrong `%s' key size (got %zi; "
			       "expected %zi)",
			       key_hex, hash_name,
			       handle->block_id_size, hash_size);
	      *size = 0;
	      err = CHOP_BLOCK_FETCHER_ERROR;
	      goto finish;
	    }
	}
      else
	chop_log_printf (&fetcher->log, "block %s: hash method unknown, "
			 "cannot check integrity", key_hex);

      cipher_algo = chop_cipher_algorithm (fetcher->cipher_handle);

      /* We round hash keys so that their size is equal to the key size
	 requested by the algorithm (see `cipher_make_suitable_key ()').
	 Therefore, at this point, this should still be true.  */
      if (chop_cipher_algo_key_size (cipher_algo) != handle->key_size)
	{
	  chop_log_printf (&fetcher->log, "index' hash key size "
			   "is %zu instead of %zu", handle->key_size,
			   chop_cipher_algo_key_size (cipher_algo));
	  return CHOP_BLOCK_FETCHER_ERROR;
	}

      chop_cipher_reset (fetcher->cipher_handle);

      /* Provide exactly the right key size.  */
      err = chop_cipher_set_key (fetcher->cipher_handle,
				 handle->key, handle->key_size);


      if (!err)
	{
	  char *cleartext = alloca (*size);

	  err = chop_cipher_decrypt (fetcher->cipher_handle,
				     cleartext, *size,
				     chop_buffer_content (&ciphertext),
				     *size);
	  if (!err)
	    {
	      /* Push only HANDLE->BLOCK_SIZE bytes (the payload).  */
	      err = chop_buffer_push (buffer, cleartext,
				      handle->block_size);
	      if (!err)
		*size = handle->block_size;
	    }
	}

      if (err)
	chop_log_printf (&fetcher->log, "block `%s': decryption error: %s",
			 key_hex, chop_error_message (err));
    }

 finish:
  chop_buffer_return (&ciphertext);

  return err;
}



/* The block indexer class.  */

CHOP_DECLARE_RT_CLASS (chk_block_indexer, block_indexer,
		       chop_cipher_handle_t cipher_handle;
		       int owns_cipher_handle;
		       chop_hash_method_t key_hash_method;
		       chop_hash_method_t block_id_hash_method;
		       chop_log_t log;);

static chop_error_t
chk_indexer_init_fetcher (const chop_block_indexer_t *block_indexer,
			  chop_block_fetcher_t *block_fetcher)
{
  chop_error_t err;
  chop_chk_block_indexer_t *indexer;
  chop_chk_block_fetcher_t *fetcher;

  err = chop_object_initialize ((chop_object_t *)block_fetcher,
				&chop_chk_block_fetcher_class);
  if (err)
    return err;

  indexer = (chop_chk_block_indexer_t *)block_indexer;
  fetcher = (chop_chk_block_fetcher_t *)block_fetcher;

  fetcher->cipher_handle = chop_cipher_copy (indexer->cipher_handle);
  fetcher->owns_cipher_handle = 1;

  /* Tell FETCHER which hash method is used so it can check block integrity
     upon retrieval.  */
  fetcher->block_id_hash_method = indexer->block_id_hash_method;

  return err;
}

static chop_error_t
chk_index_block (chop_block_indexer_t *indexer,
		 chop_block_store_t *store,
		 const char *buffer,
		 size_t size,
		 chop_index_handle_t *handle);

static chop_error_t
cbi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_chk_block_indexer_t *indexer;

  indexer = (chop_chk_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = &chop_chk_index_handle_class;
  indexer->block_indexer.block_fetcher_class = &chop_chk_block_fetcher_class;
  indexer->block_indexer.index_block = chk_index_block;
  indexer->block_indexer.init_fetcher = chk_indexer_init_fetcher;

  indexer->key_hash_method = CHOP_HASH_NONE;
  indexer->cipher_handle = CHOP_CIPHER_HANDLE_NIL;
  indexer->owns_cipher_handle = 0;

  return chop_log_init ("chk-block-indexer", &indexer->log);
}

static void
cbi_dtor (chop_object_t *object)
{
  chop_chk_block_indexer_t *indexer;

  indexer = (chop_chk_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = NULL;
  indexer->block_indexer.block_fetcher_class = NULL;

  indexer->key_hash_method = CHOP_HASH_NONE;

  if (indexer->owns_cipher_handle)
    chop_cipher_close (indexer->cipher_handle);
  indexer->owns_cipher_handle = 0;
  indexer->cipher_handle = CHOP_CIPHER_HANDLE_NIL;

  chop_object_destroy ((chop_object_t *) &indexer->log);
}

static chop_error_t
cbi_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  return CHOP_ERR_NOT_IMPL;
}

static chop_error_t
cbi_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  chop_error_t err;
  chop_chk_block_indexer_t *indexer;

  err = chop_object_initialize (object, &chop_chk_block_indexer_class);
  if (err)
    return err;

  *bytes_read = 0;
  indexer = (chop_chk_block_indexer_t *)object;
  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	/* The user-visible serialization format is as follows:
	   CIPHER,CIPHER-MODE,KEY-HASH,BLOCK-ID-HASH.  So, for instance, the
	   following is a valid combination: "blowfish,cbc,sha1,sha1".  */
	const char *end;
	char *comma;
	char *algo_name, *mode_name, *key_hash_name, *block_id_hash_name;
	chop_cipher_algo_t algo;
	chop_cipher_mode_t mode;
	chop_hash_method_t key_hash_method, block_id_hash_method;

#define FETCH_NAME(_name)				\
    comma = memchr (buffer, ',', size - *bytes_read);	\
    if (!comma)						\
      return CHOP_DESERIAL_CORRUPT_INPUT;		\
							\
    (_name) = alloca (comma - buffer + 1);		\
    strncpy ((_name), buffer, comma - buffer);		\
    (_name)[comma - buffer] = '\0';			\
    *bytes_read += comma - buffer + 1;			\
    buffer = comma + 1;

        FETCH_NAME (algo_name);
	FETCH_NAME (mode_name);
	FETCH_NAME (key_hash_name);

#undef FETCH_NAME

	/* The last one needs to be treated specially.  */
	for (end = comma + 1;
	     *end && isalnum (*end);
	     end++);
	block_id_hash_name = alloca (end - comma);
	strncpy (block_id_hash_name, comma + 1, end - comma - 1);
	block_id_hash_name[end - comma - 1] = '\0';
	*bytes_read += end - comma - 1;

	if (chop_cipher_algo_lookup (algo_name, &algo))
	  return CHOP_DESERIAL_CORRUPT_INPUT;
	if (chop_cipher_mode_lookup (mode_name, &mode))
	  return CHOP_DESERIAL_CORRUPT_INPUT;
	if (chop_hash_method_lookup (key_hash_name, &key_hash_method))
	  return CHOP_DESERIAL_CORRUPT_INPUT;
	if (chop_hash_method_lookup (block_id_hash_name, &block_id_hash_method))
	  return CHOP_DESERIAL_CORRUPT_INPUT;

	indexer->cipher_handle = chop_cipher_open (algo, mode);
	if (indexer->cipher_handle == CHOP_CIPHER_HANDLE_NIL)
	  return CHOP_DESERIAL_CORRUPT_INPUT;
	indexer->owns_cipher_handle = 1;

	indexer->key_hash_method = key_hash_method;
	indexer->block_id_hash_method = block_id_hash_method;
	break;
      }

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return err;
}

CHOP_DEFINE_RT_CLASS (chk_block_indexer, block_indexer,
		      cbi_ctor, cbi_dtor,
		      NULL, NULL,
		      cbi_serialize, cbi_deserialize);

/* Make KEY point to a ciphering key of at most KEY_SIZE bytes.  Fill KEY
   with data from SOURCE, a SOURCE_SIZE-byte buffer, cycling if SOURCE is
   smaller than ALGO's key size and truncating SOURCE if SOURCE_SIZE is
   larger than ALGO's key size.  */
static inline void
cipher_make_suitable_key (char *key, size_t key_size,
			  const char *source, size_t source_size)
{
#define MIN(a, b)  (((a) < (b)) ? (a) : (b))
  if (source_size >= key_size)
    /* Keep only KEY_SIZE bytes from SOURCE.  */
    memcpy (key, source, key_size);
  else
    {
      size_t source_offset = 0, key_offset = 0;

      while (key_size > key_offset)
	{
	  size_t amount;

	  amount = MIN (source_size - source_offset,
			key_size - key_offset);

	  memcpy (key + key_offset,
		  source + source_offset,
		  amount);

	  source_offset = (source_offset + amount) % source_size;
	  key_offset += amount;
	}
    }
#undef MIN
}

/* The threshold above which memory allocations are made on the heap rather
   than on the stack, to avoid stack overflows.  */
#define ALLOCA_THRESHOLD 4096

static chop_error_t
chk_index_block (chop_block_indexer_t *indexer,
		 chop_block_store_t *store,
		 const char *buffer,
		 size_t size,
		 chop_index_handle_t *handle)
{
  chop_error_t err;
  chop_cipher_handle_t cipher_handle;
  chop_chk_block_indexer_t *chk_indexer;
  chop_chk_index_handle_t *chk_handle;
  chop_block_key_t key;
  size_t hash_key_size, cipher_key_size, block_id_size;
  size_t block_size, padding_size, total_size;
  chop_cipher_algo_t algo;
  char *hash_key, *block_content;

  /* Getting ready.  */
  chk_indexer = (chop_chk_block_indexer_t *)indexer;
  cipher_handle = chk_indexer->cipher_handle;
  err = chop_object_initialize ((chop_object_t *)handle,
				&chop_chk_index_handle_class);
  if (err)
    return err;

  chk_handle = (chop_chk_index_handle_t *)handle;
  memset (chk_handle->block_id, 0, sizeof (chk_handle->block_id));
  memset (chk_handle->key, 0, sizeof (chk_handle->key));

  algo = chop_cipher_algorithm (cipher_handle);

  /* Encrypt the block using its hash as a key.  */
  block_size = chop_cipher_algo_block_size (algo);
  cipher_key_size = chop_cipher_algo_key_size (algo);
  assert (block_size > 0);
  assert (cipher_key_size > 0);

  hash_key_size = chop_hash_size (chk_indexer->key_hash_method);
  hash_key = alloca (hash_key_size);
  chop_hash_buffer (chk_indexer->key_hash_method, buffer, size, hash_key);

  /* Most ciphering algorithms need the input size to be a multiple of
     their ciphering block size.  */
  padding_size = (size % block_size) ? (block_size - (size % block_size)) : 0;
  total_size = size + padding_size;

  if (size < total_size)
    {
      /* Provide the ciphering routines with an input buffer of the right
	 size.  */
      char *new_buffer;

      new_buffer = total_size > ALLOCA_THRESHOLD
	? chop_malloc (total_size, &chop_chk_block_indexer_class)
	: alloca (total_size);
      memcpy (new_buffer, buffer, size);
      memset (new_buffer + size, 0, padding_size);
      buffer = new_buffer;
    }

  block_content = total_size > ALLOCA_THRESHOLD
    ? chop_malloc (total_size, &chop_chk_block_indexer_class)
    : alloca (total_size);
  if (padding_size)
    /* Pad with zeros.  The actual size of the block will be stored in
       its index anyway.  */
    memset (block_content + size, 0, padding_size);

  /* Make INDEX->HASH_KEY a key of size CIPHER_KEY_SIZE from HASH_KEY
     which is HASH_KEY_SIZE-byte long.  */
  cipher_make_suitable_key (chk_handle->key, cipher_key_size,
			    hash_key, hash_key_size);

  chop_cipher_reset (cipher_handle);

  /* Provide exactly the right key size.  */
  err = chop_cipher_set_key (cipher_handle, chk_handle->key,
			     cipher_key_size);

  while (CHOP_EXPECT_FALSE (err == CHOP_CIPHER_WEAK_KEY))
    {
      /* The key we wanted to use was considered weak.  Hence, we use a
	 random one, hoping that it won't be considered weak...  */
      chop_randomize (chk_handle->key, cipher_key_size);
      err = chop_cipher_set_key (cipher_handle, chk_handle->key,
				 cipher_key_size);
    }

  if (CHOP_EXPECT_TRUE (err == 0))
    {
#ifdef HAVE_VALGRIND_MEMCHECK_H
      VALGRIND_CHECK_MEM_IS_DEFINED (buffer, size);
#endif
      err = chop_cipher_encrypt (cipher_handle,
				 block_content, total_size,
				 buffer, total_size);
      if (err)
	chop_log_printf (&chk_indexer->log, "cipher failed: %s",
			 chop_error_message (err));
    }
  else
    chop_log_printf (&chk_indexer->log, "failed to set encryption key: %s",
		     chop_error_message (err));

  if (CHOP_EXPECT_TRUE (err == 0))
    {
      /* Compute the block key.  */
      block_id_size = chop_hash_size (chk_indexer->block_id_hash_method);
      chop_hash_buffer (chk_indexer->block_id_hash_method,
			block_content, total_size,
			chk_handle->block_id);
      chk_handle->block_id_size = block_id_size;
      chk_handle->key_size = cipher_key_size;


      /* Write the ciphered block to backing store.  */
      chop_block_key_init (&key, chk_handle->block_id, block_id_size,
			   NULL, NULL);

      err = chop_store_write_block (store, &key, block_content, total_size);
      chk_handle->block_size = size;

      /* Again, the binary representation uses 8 bytes to store the
	 `block_size' field.  */
      chk_handle->index_handle.size =
	chk_handle->key_size + chk_handle->block_id_size
	+ BINARY_SERIALIZATION_HEADER_SIZE;
    }

  if (CHOP_EXPECT_FALSE (err != 0))
    chop_object_destroy ((chop_object_t *) handle);

  if (total_size > ALLOCA_THRESHOLD)
    {
      if (total_size > size)
	chop_free ((char *) buffer, &chop_chk_block_indexer_class);
      chop_free (block_content, &chop_chk_block_indexer_class);
    }

  return err;
}

chop_error_t
chop_chk_block_indexer_open (chop_cipher_handle_t cipher_handle,
			     int owns_cipher_handle,
			     chop_hash_method_t key_hash_method,
			     chop_hash_method_t block_id_hash_method,
			     chop_block_indexer_t *block_indexer)
{
  chop_error_t err;
  chop_chk_block_indexer_t *indexer;

  if ((key_hash_method == CHOP_HASH_NONE) ||
      (block_id_hash_method == CHOP_HASH_NONE) ||
      (cipher_handle == CHOP_CIPHER_HANDLE_NIL))
    return CHOP_INVALID_ARG;

  indexer = (chop_chk_block_indexer_t *)block_indexer;
  err = chop_object_initialize ((chop_object_t *)indexer,
				&chop_chk_block_indexer_class);
  if (err)
    return err;

  indexer->owns_cipher_handle = owns_cipher_handle;
  indexer->cipher_handle = cipher_handle;
  indexer->key_hash_method = key_hash_method;
  indexer->block_id_hash_method = block_id_hash_method;

  return err;
}


/* arch-tag: e90a9c2b-ae67-4082-a518-6278e2224c8e
 */
