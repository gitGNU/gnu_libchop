/* A dummy block store for debugging purposes.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>


/* Class definition (this has to be somewhere).  */

CHOP_DEFINE_RT_CLASS (block_store, object,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);

CHOP_DECLARE_RT_CLASS (dummy_block_store, block_store,
		       chop_block_store_t *backend;);

CHOP_DEFINE_RT_CLASS (dummy_block_store, block_store,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);



static errcode_t
chop_dummy_block_store_read_block (chop_block_store_t *store,
				   const chop_block_key_t *key,
				   chop_buffer_t *buffer,
				   size_t *size)
{
  errcode_t err;
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;
  char hex_key[1024];

  chop_block_key_to_hex_string (key, hex_key);
  fprintf (stdout,
	   "dummy: read_block (%s@%p, 0x%s,\n"
	   "                   %p, %p)\n",
	   store->name, store, hex_key, buffer, size);
  *size = 0;

  if (!dummy->backend)
    return CHOP_ERR_NOT_IMPL;

  err = chop_store_read_block (dummy->backend, key, buffer, size);

  if (err)
    fprintf (stdout,
	     "dummy: read_block: underlying store returned \"%s\"\n",
	     error_message (err));

  return err;
}

static errcode_t
chop_dummy_block_store_write_block (chop_block_store_t *store,
				    const chop_block_key_t *key,
				    const char *block, size_t size)
{
  errcode_t err;
  char hex_key[1024];
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  chop_block_key_to_hex_string (key, hex_key);
  fprintf (stdout,
	   "dummy: write_block (%s@%p, 0x%s,\n"
	   "                    %p, %u)\n",
	   store->name, store, hex_key, block, size);

  if (!dummy->backend)
    return 0;

  err = chop_store_write_block (dummy->backend, key, block, size);
  if (err)
    fprintf (stdout,
	     "dummy: read_block: underlying store returned \"%s\"\n",
	     error_message (err));

  return err;
}

static errcode_t
chop_dummy_block_store_sync (chop_block_store_t *store)
{
  errcode_t err;
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  fprintf (stdout, "dummy: sync (%s@%p)\n", store->name, store);
  if (!dummy->backend)
    return 0;

  err = chop_store_sync (dummy->backend);
  if (err)
    fprintf (stdout,
	     "dummy: sync: underlying store returned \"%s\"\n",
	     error_message (err));

  return err;
}

static errcode_t
chop_dummy_block_store_close (chop_block_store_t *store)
{
  fprintf (stdout, "dummy: close (%s@%p)\n", store->name, store);
  free (store->name);
  store->name = NULL;
  return 0;
}

void
chop_dummy_block_store_open (const char *name,
			     chop_block_store_t *store)
{
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  store->name = strdup (name);
  store->read_block = chop_dummy_block_store_read_block;
  store->write_block = chop_dummy_block_store_write_block;
  store->close = chop_dummy_block_store_close;
  store->sync = chop_dummy_block_store_sync;

  dummy->backend = NULL;
}

void
chop_dummy_proxy_block_store_open (const char *name,
				   chop_block_store_t *backend,
				   chop_block_store_t *store)
{
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  chop_dummy_block_store_open (name, store);

  dummy->backend = backend;
}
