/* A dummy block store for debugging purposes.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>


/* Class definitions.  */

CHOP_DECLARE_RT_CLASS (dummy_block_store, block_store,
		       chop_log_t log;
		       chop_block_store_t *backend;);

static void
dbs_dtor (chop_object_t *object)
{
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)object;

  if (dummy->block_store.name)
    free (dummy->block_store.name);
  dummy->block_store.name = NULL;

  chop_object_destroy ((chop_object_t *)&dummy->log);
}

CHOP_DEFINE_RT_CLASS (dummy_block_store, block_store,
		      NULL, dbs_dtor,
		      NULL, NULL  /* No serializer/deserializer */);




static errcode_t
chop_dummy_block_store_block_exists (chop_block_store_t *store,
				     const chop_block_key_t *key,
				     int *exists)
{
  errcode_t err;
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;
  char hex_key[1024];

  chop_block_key_to_hex_string (key, hex_key);
  chop_log_printf (&dummy->log,
		   "dummy: block_exists (%s@%p, 0x%s)",
		   store->name, store, hex_key);
  *exists = 0;

  if (!dummy->backend)
    return 0; /* CHOP_ERR_NOT_IMPL ? */

  err = chop_store_block_exists (dummy->backend, key, exists);

  if (err)
    chop_log_printf (&dummy->log,
		     "dummy: block_exists: underlying store returned \"%s\"\n",
		     error_message (err));
  else
    chop_log_printf (&dummy->log,
		     "dummy: block 0x%s does %sexist",
		     hex_key, (*exists ? "" : "NOT "));


  return err;
}

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
  chop_log_printf (&dummy->log,
		   "dummy: read_block (%s@%p, 0x%s,\n"
		   "                   %p, %p)\n",
		   store->name, store, hex_key, buffer, size);
  *size = 0;

  if (!dummy->backend)
    return CHOP_ERR_NOT_IMPL;

  err = chop_store_read_block (dummy->backend, key, buffer, size);

  if (err)
    chop_log_printf (&dummy->log,
		     "dummy: read_block: underlying store returned \"%s\"\n",
		     error_message (err));

  if ((!err) && (chop_buffer_size (buffer) != *size))
    chop_log_printf (&dummy->log,
		     "dummy: read_block: warning: buffer size is %u while "
		     "reported size is %u\n",
		     chop_buffer_size (buffer), *size);

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
  chop_log_printf (&dummy->log,
		   "dummy: write_block (%s@%p, 0x%s,\n"
		   "                    %p, %u)\n",
		   store->name, store, hex_key, block, size);

  if (!dummy->backend)
    return 0;

  err = chop_store_write_block (dummy->backend, key, block, size);
  if (err)
    chop_log_printf (&dummy->log,
		     "dummy: write_block: underlying store returned \"%s\"\n",
		     error_message (err));

  return err;
}

static errcode_t
chop_dummy_block_store_delete_block (chop_block_store_t *store,
				     const chop_block_key_t *key)
{
  errcode_t err;
  char hex_key[1024];
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  chop_block_key_to_hex_string (key, hex_key);
  chop_log_printf (&dummy->log,
		   "dummy: delete_block (%s@%p, 0x%s,\n",
		   store->name, store, hex_key);

  if (!dummy->backend)
    return 0;

  err = chop_store_delete_block (dummy->backend, key);
  if (err)
    chop_log_printf (&dummy->log,
		     "dummy: delete_block: underlying store returned \"%s\"\n",
		     error_message (err));

  return err;
}

static errcode_t
chop_dummy_block_store_first_block (chop_block_store_t *store,
				    chop_block_iterator_t *it)
{
  errcode_t err;
  char hex_key[1024];
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  if ((!dummy->backend) || (!chop_store_iterator_class (store)))
    return CHOP_ERR_NOT_IMPL;

  err = chop_store_first_block (dummy->backend, it);
  if (err)
    chop_log_printf (&dummy->log,
		     "dummy: first_block: underlying store returned \"%s\"\n",
		     error_message (err));
  else
    {
      const chop_block_key_t *key;

      key = chop_block_iterator_key (it);
      chop_block_key_to_hex_string (key, hex_key);
      chop_log_printf (&dummy->log,
		       "dummy: first_block (%s@%p) => 0x%s",
		       store->name, store, hex_key);
    }

  return err;
}


static errcode_t
chop_dummy_block_store_sync (chop_block_store_t *store)
{
  errcode_t err;
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  chop_log_printf (&dummy->log,
		   "dummy: sync (%s@%p)\n", store->name, store);
  if (!dummy->backend)
    return 0;

  err = chop_store_sync (dummy->backend);
  if (err)
    chop_log_printf (&dummy->log,
		     "dummy: sync: underlying store returned \"%s\"\n",
		     error_message (err));

  return err;
}

static errcode_t
chop_dummy_block_store_close (chop_block_store_t *store)
{
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  chop_log_printf (&dummy->log,
		   "dummy: close (%s@%p)\n", store->name, store);

  if (dummy->backend)
    {
      chop_store_close (dummy->backend);
      dummy->backend = NULL;
    }

  return 0;
}

void
chop_dummy_block_store_open (const char *name,
			     chop_block_store_t *store)
{
  errcode_t err;
  char *log_name;
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  log_name = alloca (strlen (name) + 6 + 1);
  strcpy (log_name, "dummy/");
  strcpy (log_name + 6, name);

  err = chop_object_initialize ((chop_object_t *)store,
				&chop_dummy_block_store_class);
  if (err)
    return;  /* FIXME */

  err = chop_log_init (log_name, &dummy->log);
  if (err)
    {
      chop_object_destroy ((chop_object_t *)store);
      return; /* FIXME */
    }

  /* By default, dump to stderr */
/*   chop_log_attach (&dummy->log, 2, 0); */

  store->name = strdup (name);
  store->iterator_class = NULL; /* not supported */
  store->block_exists = chop_dummy_block_store_block_exists;
  store->read_block = chop_dummy_block_store_read_block;
  store->write_block = chop_dummy_block_store_write_block;
  store->delete_block = chop_dummy_block_store_delete_block;
  store->first_block = chop_dummy_block_store_first_block;
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
  if (backend)
    /* Note: since we do not implement a proxy iterator class here, we are
       not able to track calls to `chop_block_iterator_next ()'.  */
    store->iterator_class = chop_store_iterator_class (backend);
}

chop_log_t *
chop_dummy_block_store_log (chop_block_store_t *store)
{
  chop_dummy_block_store_t *dummy =
    (chop_dummy_block_store_t *)store;

  return (&dummy->log);
}
