/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <stdlib.h>
#include <errno.h>

static __inline__ chop_block_store_t *
chop_dummy_block_store_open_alloc (const char *name)
{
  chop_block_store_t *store;

  store = malloc (chop_class_instance_size (&chop_dummy_block_store_class));
  if (!store)
    return NULL;

  chop_dummy_block_store_open (name, store);

  return store;
}

static __inline__ chop_block_store_t *
chop_dummy_proxy_block_store_open_alloc (const char *name,
					 chop_block_store_t *backend)
{
  chop_block_store_t *store;

  store = malloc (chop_class_instance_size (&chop_dummy_block_store_class));
  if (!store)
    return NULL;

  chop_dummy_proxy_block_store_open (name, backend, store);

  return store;
}

static __inline__ errcode_t
chop_gdbm_block_store_open_alloc (const char *name, size_t block_size,
				  int mode, chop_block_store_t **store)
{
  errcode_t err;

  *store = malloc (chop_class_instance_size (&chop_gdbm_block_store_class));
  if (!*store)
    return ENOMEM;

  err = chop_gdbm_store_open (name, block_size, mode, NULL,
			      *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_remote_block_store_open_alloc (const char *host, const char *protocol,
				    chop_block_store_t **store)
{
  errcode_t err;

  *store = malloc (chop_class_instance_size (&chop_remote_block_store_class));
  if (!*store)
    return ENOMEM;

  err = chop_remote_block_store_open (host, protocol, *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static void
chop_store_close_dealloc (chop_block_store_t *store)
{
  if (store)
    {
      chop_store_close (store);
      free (store);
    }
}

