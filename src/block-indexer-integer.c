/* A block indexer that generates a unique 32-bit integer for each block it
   is passed.  A lot of code is borrowed from `block-indexer-uuid.c'.

   This block indexer, unlike the `hash' and `chk' block indexers, does not
   have the single-instance storage property since every block automatically
   gets assigned a new ID.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/block-indexers.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <arpa/inet.h>
#include <stdint.h>


/* The index handle class.  */

CHOP_DECLARE_RT_CLASS (integer_index_handle, index_handle,
		       uint32_t id;  /* the current block key */);


static int
iih_equalp (const chop_object_t *h1, const chop_object_t *h2)
{
  chop_integer_index_handle_t *iih1, *iih2;

  iih1 = (chop_integer_index_handle_t *) h1;
  iih2 = (chop_integer_index_handle_t *) h2;

  return (iih1->id == iih2->id);
}

static errcode_t
iih_copy (const chop_object_t *s, chop_object_t *d)
{
  chop_integer_index_handle_t *source, *dest;

  source = (chop_integer_index_handle_t *)s;
  dest = (chop_integer_index_handle_t *)d;

  dest->id = source->id;

  return 0;
}

static errcode_t
iih_serialize (const chop_object_t *object, chop_serial_method_t method,
	      chop_buffer_t *buffer)
{
  chop_integer_index_handle_t *iih =
    (chop_integer_index_handle_t *) object;

  switch (method)
    {
    case CHOP_SERIAL_BINARY:
      {
	uint32_t id;
	id = htonl (iih->id);
	chop_buffer_push (buffer, (char *) &id, sizeof (id));
      }
      break;

    case CHOP_SERIAL_ASCII:
      {
	char out[123];
	sprintf (out, "%08x", iih->id);
	chop_buffer_push (buffer, out, 9);
      }
      break;

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

static errcode_t
iih_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;
  chop_integer_index_handle_t *iih =
    (chop_integer_index_handle_t *) object;

  *bytes_read = 0;
  err = chop_object_initialize (object, &chop_integer_index_handle_class);
  if (err)
    return err;

  switch (method)
    {
    case CHOP_SERIAL_BINARY:
      if (size < sizeof (iih->id))
	return CHOP_DESERIAL_TOO_SHORT;
      memcpy (&iih->id, buffer, sizeof (iih->id));
      iih->id = ntohl (iih->id);

      *bytes_read = sizeof (iih->id);
      break;

    case CHOP_SERIAL_ASCII:
      if (size < 8)
	return CHOP_DESERIAL_TOO_SHORT;

      {
	char *end;
	iih->id = strtoul (buffer, &end, 16);
	if (end == buffer)
	  return CHOP_DESERIAL_CORRUPT_INPUT;
	else
	  *bytes_read = end - buffer;
      }

      break;

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

CHOP_DEFINE_RT_CLASS (integer_index_handle, index_handle,
		      NULL, NULL,
		      iih_copy, iih_equalp,
		      iih_serialize, iih_deserialize);


/* The fetcher class.  */
CHOP_DECLARE_RT_CLASS (integer_block_fetcher, block_fetcher,
		       /* Nothing, easy.  */
		       chop_log_t log;);

static errcode_t integer_block_fetch (chop_block_fetcher_t *,
				      const chop_index_handle_t *,
				      chop_block_store_t *,
				      chop_buffer_t *,
				      size_t *);

static errcode_t
ibf_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_integer_block_fetcher_t *fetcher;

  fetcher = (chop_integer_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = integer_block_fetch;
  fetcher->block_fetcher.index_handle_class = &chop_integer_index_handle_class;

  return chop_log_init ("integer-block-fetcher", &fetcher->log);
}

static void
ibf_dtor (chop_object_t *object)
{
  chop_integer_block_fetcher_t *fetcher;

  fetcher = (chop_integer_block_fetcher_t *)object;
  fetcher->block_fetcher.fetch_block = NULL;
  fetcher->block_fetcher.index_handle_class = NULL;

  chop_object_destroy ((chop_object_t *)&fetcher->log);
}

static errcode_t
ibf_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  /* Stateless.  */
  return 0;
}

static errcode_t
ibf_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;

  err = chop_object_initialize (object, &chop_integer_block_fetcher_class);

  /* Stateless.  */
  *bytes_read = 0;

  return 0;
}

CHOP_DEFINE_RT_CLASS (integer_block_fetcher, block_fetcher,
		      ibf_ctor, ibf_dtor,
		      NULL, NULL,
		      ibf_serialize, ibf_deserialize);

chop_log_t *
chop_integer_block_fetcher_log (chop_block_fetcher_t *fetcher)
{
  chop_integer_block_fetcher_t *hfetcher;

  if (!chop_object_is_a ((chop_object_t *)fetcher,
			 &chop_integer_block_fetcher_class))
    return NULL;

  hfetcher = (chop_integer_block_fetcher_t *)fetcher;
  return (&hfetcher->log);
}

static errcode_t
integer_block_fetch (chop_block_fetcher_t *block_fetcher,
		     const chop_index_handle_t *index,
		     chop_block_store_t *store,
		     chop_buffer_t *buffer, size_t *size)
{
  errcode_t err;
  chop_integer_index_handle_t *iih;
  chop_integer_block_fetcher_t *fetcher;
  uint32_t id;
  chop_block_key_t key;

  fetcher = (chop_integer_block_fetcher_t *)block_fetcher;
  if (!chop_object_is_a ((chop_object_t *)index,
			 &chop_integer_index_handle_class))
    return CHOP_INVALID_ARG;

  iih = (chop_integer_index_handle_t *) index;
  id = htonl (iih->id);

  chop_block_key_init (&key, (char *) &id, sizeof (id), NULL, NULL);

  err = chop_store_read_block (store, &key, buffer, size);

  return err;
}


/* The indexer class.  */
CHOP_DECLARE_RT_CLASS (integer_block_indexer, block_indexer,
		       uint32_t id; /* the current block ID */);

static errcode_t
integer_indexer_init_fetcher (const chop_block_indexer_t *block_indexer,
			      chop_block_fetcher_t *fetcher)
{
  /* Our fetchers are stateless so there is nothing special to initialize
     here.  */
  return chop_object_initialize ((chop_object_t *)fetcher,
				 &chop_integer_block_fetcher_class);
}

static errcode_t
integer_block_index (chop_block_indexer_t *indexer,
		     chop_block_store_t *store,
		     const char *buffer,
		     size_t size,
		     chop_index_handle_t *handle);

static errcode_t
ibi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_integer_block_indexer_t *indexer;

  indexer = (chop_integer_block_indexer_t *)object;
  indexer->block_indexer.index_handle_class = &chop_integer_index_handle_class;
  indexer->block_indexer.block_fetcher_class = &chop_integer_block_fetcher_class;
  indexer->block_indexer.index_block = integer_block_index;
  indexer->block_indexer.init_fetcher = integer_indexer_init_fetcher;
  indexer->id = 0;

  return 0;
}

static void
ibi_dtor (chop_object_t *object)
{
  chop_integer_block_indexer_t *indexer;

  indexer = (chop_integer_block_indexer_t *) object;
  indexer->block_indexer.index_handle_class = NULL;
  indexer->block_indexer.block_fetcher_class = NULL;
}

static errcode_t
ibi_serialize (const chop_object_t *object, chop_serial_method_t method,
	       chop_buffer_t *buffer)
{
  /* Stateless.  */
  return 0;
}

static errcode_t
ibi_deserialize (const char *buffer, size_t size, chop_serial_method_t method,
		 chop_object_t *object, size_t *bytes_read)
{
  errcode_t err;
  chop_integer_block_indexer_t *ibi =
    (chop_integer_block_indexer_t *) object;

  *bytes_read = 0;
  err = chop_object_initialize (object, &chop_integer_block_indexer_class);
  if (err)
    return err;

  switch (method)
    {
    case CHOP_SERIAL_BINARY:
      if (size < sizeof (ibi->id))
	return CHOP_DESERIAL_TOO_SHORT;
      memcpy (&ibi->id, buffer, sizeof (ibi->id));
      ibi->id = ntohl (ibi->id);

      *bytes_read = sizeof (ibi->id);
      break;

    case CHOP_SERIAL_ASCII:
      /* Allow for empty serials, in which case ID is set to zero.  */
      if (size > 0)
	{
	  char *end;
	  ibi->id = strtoul (buffer, &end, 16);
	  if (end > buffer)
	    *bytes_read = end - buffer;
	  else
	    ibi->id = 0;
	}
      break;

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return 0;
}

CHOP_DEFINE_RT_CLASS (integer_block_indexer, block_indexer,
		      ibi_ctor, ibi_dtor,
		      NULL, NULL,
		      ibi_serialize, ibi_deserialize);


static errcode_t
integer_block_index (chop_block_indexer_t *indexer,
		     chop_block_store_t *store,
		     const char *buffer,
		     size_t size,
		     chop_index_handle_t *handle)
{
  errcode_t err;
  chop_integer_block_indexer_t *ibi;
  chop_integer_index_handle_t *iih;
  chop_block_key_t key;
  uint32_t id;

  ibi = (chop_integer_block_indexer_t *) indexer;
  err = chop_object_initialize ((chop_object_t *)handle,
				&chop_integer_index_handle_class);
  if (err)
    return err;

  iih = (chop_integer_index_handle_t *) handle;

  /* Compute an identifier for BUFFER, i.e., take the current integer and
     increment it.  */
  iih->id = ibi->id++;
  id = htonl (iih->id);

  chop_block_key_init (&key, (char *) &id, sizeof (iih->id), NULL, NULL);

  /* Write BUFFER to the backing store using this identifier.  */
  err = chop_store_write_block (store, &key, buffer, size);
  if (err)
    chop_object_destroy ((chop_object_t *)handle);
  else
    iih->index_handle.size = sizeof (iih->id);

  return err;
}

errcode_t
chop_integer_block_indexer_open (unsigned long start,
				 chop_block_indexer_t *indexer)
{
  errcode_t err;

  err = chop_object_initialize ((chop_object_t *)indexer,
				&chop_integer_block_indexer_class);
  if (!err)
    {
      chop_integer_block_indexer_t *ibi;

      ibi = (chop_integer_block_indexer_t *) indexer;
      ibi->id = start;
    }

  return err;
}


/* arch-tag: d522f5aa-2b89-444a-aba4-c6c83f859ab1
 */

