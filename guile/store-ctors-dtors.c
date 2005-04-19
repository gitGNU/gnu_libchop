/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <stdlib.h>
#include <errno.h>
#include <assert.h>

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
				  int open_flags, mode_t mode,
				  chop_block_store_t **store)
{
  errcode_t err;

  *store = malloc
    (chop_class_instance_size ((chop_class_t *)&chop_gdbm_block_store_class));
  if (!*store)
    return ENOMEM;

  err = chop_gdbm_store_open (name, block_size, open_flags, mode, NULL,
			      *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_tdb_block_store_open_alloc (const char *name, int hash_size,
				 int open_flags, mode_t mode,
				 chop_block_store_t **store)
{
  errcode_t err;

  *store = malloc
    (chop_class_instance_size ((chop_class_t *)&chop_tdb_block_store_class));
  if (!*store)
    return ENOMEM;

  err = chop_tdb_store_open (name, hash_size, 0, open_flags, mode,
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

static __inline__ errcode_t
chop_store_read_block_alloc_u8vector (chop_block_store_t *store,
				      const chop_block_key_t *key,
				      SCM *result)
{
  errcode_t err;
  size_t size;
  chop_buffer_t buffer;

  chop_buffer_init (&buffer, 0);

  err = chop_store_read_block (store, key, &buffer, &size);
  if (err)
    {
      chop_buffer_return (&buffer);
      *result = SCM_BOOL_F;

      /* When a block wasn't found in the underlying store, simply return
	 #f.  */
      return ((err == CHOP_STORE_BLOCK_UNAVAIL) ? 0 : err);
    }

  assert (size == chop_buffer_size (&buffer));
  if (!size)
    *result = SCM_BOOL_F;
  else
    {
      char *block = malloc (size);
      if (block)
	{
	  memcpy (block, chop_buffer_content (&buffer), size);
	  *result = scm_take_u8vector (block, size);
	}
      else
	{
	  err = ENOMEM;
	  *result = SCM_BOOL_F;
	}
    }

  return err;
}
