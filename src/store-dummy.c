/* A dummy block store for debugging purposes.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static errcode_t
chop_dummy_block_store_read_block (chop_block_store_t *store,
				   const chop_block_key_t *key,
				   chop_buffer_t *buffer,
				   size_t *size)
{
  char hex_key[1024];
  chop_block_key_to_hex_string (key, hex_key);
  fprintf (stdout,
	   "dummy: read_block (%s-%p, 0x%s,\n"
	   "                   %p, %p)\n",
	   store->name, store, hex_key, buffer, size);
  *size = 0;
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_dummy_block_store_write_block (chop_block_store_t *store,
				    const chop_block_key_t *key,
				    const char *block, size_t size)
{
  char hex_key[1024];
  chop_block_key_to_hex_string (key, hex_key);
  fprintf (stdout,
	   "dummy: write_block (%s-%p, 0x%s,\n"
	   "                    %p, %u)\n",
	   store->name, store, hex_key, block, size);
  return 0;
}

static errcode_t
chop_dummy_block_store_sync (chop_block_store_t *store)
{
  fprintf (stdout, "dummy: sync (%s-%p)\n", store->name, store);
  return 0;
}

static errcode_t
chop_dummy_block_store_close (chop_block_store_t *store)
{
  fprintf (stdout, "dummy: close (%s-%p)\n", store->name, store);
  free (store->name);
  store->name = NULL;
  return 0;
}

void
chop_dummy_block_store_open (const char *name,
			     chop_dummy_block_store_t *dummy)
{
  chop_block_store_t *store = (chop_block_store_t *)dummy;

  store->name = strdup (name);
  store->read_block = chop_dummy_block_store_read_block;
  store->write_block = chop_dummy_block_store_write_block;
  store->close = chop_dummy_block_store_close;
  store->sync = chop_dummy_block_store_sync;
}

