/* Content-based addressing, also sometimes referred to as
   ``compare-by-hash''.  */

#include <chop/chop.h>
#include <chop/block-indexers.h>

#include <stdio.h>
#include <ctype.h>
#include <assert.h>



/* The index handle class.  */

CHOP_DECLARE_RT_CLASS (hash_index_handle, index_handle,
		       size_t block_size; /* size of the indexed block */
		       size_t key_size;   /* size of the block key */
		       char content[1024];/* the block key */);

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


static errcode_t
hih_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  chop_hash_index_handle_t *handle =
    (chop_hash_index_handle_t *)object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	/* Return something like
	   "eabe1ca1f3c7ca148ce2fe5954f52ef9a0f0082a/39a".  */
	char *hex;

	hex = alloca ((handle->key_size * 2) + 1);
	chop_buffer_to_hex_string (handle->content, handle->key_size, hex);
	chop_buffer_push (buffer, hex, strlen (hex) /* strip trailing \0 */);

	/* Append a slash and the indexed block size.  */
	hex[0] = '/';
	sprintf (hex + 1, "%x", handle->block_size);
	chop_buffer_append (buffer, hex, strlen (hex) + 1);

	return 0;
      }

    case CHOP_SERIAL_BINARY:
      {
	errcode_t err;
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
	chop_buffer_push (buffer, size, sizeof (size));
#else
	chop_buffer_append (buffer, size, sizeof (size));
#endif

	err = chop_buffer_append (buffer, handle->content, handle->key_size);

	return err;
      }

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
hih_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;
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
	  char *slash;
	  const char *end;

	  slash = strchr (buffer, '/');
	  if (!slash)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  /* Read the block ID.  */
	  assert (slash - buffer <= sizeof (handle->content));
	  chop_hex_string_to_buffer (buffer, slash - buffer,
				     handle->content, &end);
	  if (end != slash)
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  handle->key_size = (slash - buffer) / 2;
	  end++; /* skip the slash */

	  {
	    /* Read the block size.  */
	    long int block_size;
	    const char *start;

	    start = end;

	    block_size = strtol (start, (char **)&end, 16);
	    if (end == start)
	      err = CHOP_DESERIAL_CORRUPT_INPUT;
	    else if (block_size < 0)
	      err = CHOP_DESERIAL_CORRUPT_INPUT;
	    /* FIXME:  Check for possible overflow.  */
	    else
	      handle->block_size = block_size;
	  }

	  if (!err)
	    *bytes_read = end - buffer;

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
	    printf ("expecting block size %u and "
		    "index size %u but only %u bytes left",
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

CHOP_DEFINE_RT_CLASS (hash_index_handle, index_handle,
		      NULL, NULL,
		      hih_serialize, hih_deserialize);


/* The fetcher class.  */
CHOP_DECLARE_RT_CLASS (hash_block_fetcher, block_fetcher,
		       /* Nothing, easy.  */
		       chop_log_t log;);

static errcode_t hash_block_fetch (chop_block_fetcher_t *,
				   const chop_index_handle_t *,
				   chop_block_store_t *,
				   chop_buffer_t *,
				   size_t *);

static errcode_t
hbf_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_hash_block_fetcher_t *fetcher;

  fetcher = (chop_hash_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = hash_block_fetch;
  fetcher->block_fetcher.index_handle_class = &chop_hash_index_handle_class;

  return chop_log_init ("hash-block-fetcher", &fetcher->log);
}

static void
hbf_dtor (chop_object_t *object)
{
  chop_hash_block_fetcher_t *fetcher;

  fetcher = (chop_hash_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = NULL;
  fetcher->block_fetcher.index_handle_class = NULL;

  chop_log_close (&fetcher->log);
}

static errcode_t
hbf_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  /* Stateless.  */
  return 0;
}

static errcode_t
hbf_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;

  err = chop_object_initialize (object, &chop_hash_block_fetcher_class);

  /* Stateless.  */
  *bytes_read = 0;

  return 0;
}

CHOP_DEFINE_RT_CLASS (hash_block_fetcher, block_fetcher,
		      hbf_ctor, hbf_dtor,
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

static errcode_t
hash_block_fetch (chop_block_fetcher_t *block_fetcher,
		  const chop_index_handle_t *index,
		  chop_block_store_t *store,
		  chop_buffer_t *buffer, size_t *size)
{
  errcode_t err;
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

  return err;
}


/* The indexer class.  */
CHOP_DECLARE_RT_CLASS (hash_block_indexer, block_indexer,
		       chop_hash_method_t hash_method;);

static errcode_t
hash_indexer_init_fetcher (const chop_block_indexer_t *block_indexer,
			   chop_block_fetcher_t *fetcher)
{
  /* Our fetchers are stateless so there is nothing special to initialize
     here.  */
  return chop_object_initialize ((chop_object_t *)fetcher,
				 &chop_hash_block_fetcher_class);
}

static errcode_t
hbi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_hash_block_indexer_t *indexer;

  indexer = (chop_hash_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = &chop_hash_index_handle_class;
  indexer->block_indexer.block_fetcher_class = &chop_hash_block_fetcher_class;
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

static errcode_t
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

static errcode_t
hbi_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;
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

	  if (!chop_hash_method_lookup (name, &indexer->hash_method))
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
		      hbi_serialize, hbi_deserialize);


static errcode_t
hash_block_index (chop_block_indexer_t *indexer,
		  chop_block_store_t *store,
		  const char *buffer,
		  size_t size,
		  chop_index_handle_t *handle)
{
  errcode_t err;
  chop_hash_block_indexer_t *hash_indexer;
  chop_hash_index_handle_t *hash_handle;
  chop_block_key_t key;
  size_t hash_size;

  hash_indexer = (chop_hash_block_indexer_t *)indexer;
  chop_object_initialize ((chop_object_t *)handle,
			  &chop_hash_index_handle_class);
  hash_handle = (chop_hash_index_handle_t *)handle;

  hash_size = chop_hash_size (hash_indexer->hash_method);
  if ((!hash_size) || (hash_size > sizeof (hash_handle->content)))
    return CHOP_INVALID_ARG;

  chop_hash_buffer (hash_indexer->hash_method, buffer, size,
		    hash_handle->content);
  hash_handle->key_size = hash_size;
  chop_block_key_init (&key, hash_handle->content,
		       hash_handle->key_size, NULL, NULL);

  err = chop_store_write_block (store, &key, buffer, size);
  if (err)
    chop_object_destroy ((chop_object_t *)handle);

  hash_handle->block_size = size;

  /* Again, the binary representation uses 8 bytes to store the `block_size'
     field.  */
  hash_handle->index_handle.size =
    hash_size + BINARY_SERIALIZATION_HEADER_SIZE;

  return err;
}

errcode_t
chop_hash_block_indexer_open (chop_hash_method_t hash_method,
			      chop_block_indexer_t *indexer)
{
  chop_hash_block_indexer_t *hash_indexer;

  chop_object_initialize ((chop_object_t *)indexer,
			  &chop_hash_block_indexer_class);

  hash_indexer = (chop_hash_block_indexer_t *)indexer;
  hash_indexer->block_indexer.index_block = hash_block_index;
  hash_indexer->hash_method = hash_method;

  return 0;
}

/* arch-tag: 03d6d9af-7db4-488c-aea9-d5914240861c
 */
