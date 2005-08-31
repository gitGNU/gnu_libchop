/* A `smart' block store that only writes a block if it does not already
   exist on the proxied store.  This is typically useful as a proxy to remote
   block stores.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>


/* Class definition.  */

CHOP_DECLARE_RT_CLASS (smart_block_store, block_store,
		       chop_log_t log;
		       chop_block_store_t *backend;);

CHOP_DEFINE_RT_CLASS (smart_block_store, block_store,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);



static errcode_t
chop_smart_block_store_block_exists (chop_block_store_t *store,
				     const chop_block_key_t *key,
				     int *exists)
{
  errcode_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;


  err = chop_store_block_exists (smart->backend, key, exists);

  return err;
}

static errcode_t
chop_smart_block_store_read_block (chop_block_store_t *store,
				   const chop_block_key_t *key,
				   chop_buffer_t *buffer,
				   size_t *size)
{
  errcode_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  *size = 0;

  err = chop_store_read_block (smart->backend, key, buffer, size);

  return err;
}

static errcode_t
chop_smart_block_store_write_block (chop_block_store_t *store,
				    const chop_block_key_t *key,
				    const char *block, size_t size)
{
  errcode_t err;
  int exists = 0;
  char hex_key[1024];
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  chop_block_key_to_hex_string (key, hex_key);
  chop_log_printf (&smart->log,
		   "smart: write_block (%s@%p, 0x%s,\n"
		   "                    %p, %u)\n",
		   store->name, store, hex_key, block, size);

  err = chop_store_block_exists (smart->backend, key, &exists);
  if (err)
    return err;

  if (!exists)
    err = chop_store_write_block (smart->backend, key, block, size);
  else
    chop_log_printf (&smart->log, "smart: block not actually written");

  return err;
}

static errcode_t
chop_smart_block_store_delete_block (chop_block_store_t *store,
				     const chop_block_key_t *key)
{
  errcode_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  err = chop_store_delete_block (smart->backend, key);

  return err;
}

static errcode_t
chop_smart_block_store_first_key (chop_block_store_t *store,
				  chop_block_key_t *key)
{
  errcode_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  err = chop_store_first_key (smart->backend, key);

  return err;
}

static errcode_t
chop_smart_block_store_next_key (chop_block_store_t *store,
				 const chop_block_key_t *key,
				 chop_block_key_t *next)
{
  errcode_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  err = chop_store_next_key (smart->backend, key, next);

  return err;
}

static errcode_t
chop_smart_block_store_sync (chop_block_store_t *store)
{
  errcode_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  err = chop_store_sync (smart->backend);

  return err;
}

static errcode_t
chop_smart_block_store_close (chop_block_store_t *store)
{

  return 0;
}


errcode_t
chop_smart_block_store_open (chop_block_store_t *backend,
			     chop_block_store_t *store)
{
  errcode_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  if (!backend)
    return CHOP_INVALID_ARG;

  err = chop_log_init ("smart-block-store", &smart->log);
  if (err)
    return err;

  store->block_exists = chop_smart_block_store_block_exists;
  store->read_block = chop_smart_block_store_read_block;
  store->write_block = chop_smart_block_store_write_block;
  store->delete_block = chop_smart_block_store_delete_block;
  store->first_key = chop_smart_block_store_first_key;
  store->next_key = chop_smart_block_store_next_key;
  store->close = chop_smart_block_store_close;
  store->sync = chop_smart_block_store_sync;

  smart->backend = backend;

  return 0;
}

chop_log_t *
chop_smart_block_store_log (chop_block_store_t *store)
{
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  return (&smart->log);
}
