/* The GDBM store */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>

#include <stdlib.h>
#include <gdbm.h>
#include <errno.h>

/* `chop_gdbm_block_store_t' inherits from `chop_block_store_t'.  */
CHOP_DECLARE_RT_CLASS (gdbm_block_store, block_store,
		       GDBM_FILE db;);

CHOP_DEFINE_RT_CLASS (gdbm_block_store, block_store,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);




static errcode_t chop_gdbm_read_block (chop_block_store_t *,
				       const chop_block_key_t *,
				       chop_buffer_t *,
				       size_t *);

static errcode_t chop_gdbm_write_block (chop_block_store_t *,
					const chop_block_key_t *,
					const char *,
					size_t);

static errcode_t chop_gdbm_sync (chop_block_store_t *);

static errcode_t chop_gdbm_close (chop_block_store_t *);


errcode_t
chop_gdbm_store_open (const char *name, size_t block_size,
		      int mode, void (* fatal_func) (const char *),
		      chop_block_store_t *s)
{
  chop_gdbm_block_store_t *store = (chop_gdbm_block_store_t *)s;
  store->db = gdbm_open ((char *)name, block_size, GDBM_WRCREAT,
			 mode, fatal_func);
  if (!store->db)
    {
      if (gdbm_errno == GDBM_FILE_OPEN_ERROR)
	return ENOENT;
      else
	return (gdbm_errno ? gdbm_errno : errno);
    }

  store->block_store.read_block = chop_gdbm_read_block;
  store->block_store.write_block = chop_gdbm_write_block;
  store->block_store.sync = chop_gdbm_sync;
  store->block_store.close = chop_gdbm_close;

  return 0;
}

/* Convert `chop_block_key_t' object CK into GDBM key GDBMK.  */
#define CHOP_KEY_TO_GDBM(_gdbmk, _ck)			\
{							\
  (_gdbmk)->dptr = (char *)chop_block_key_buffer (_ck);	\
  (_gdbmk)->dsize = chop_block_key_size (_ck);		\
}

static errcode_t
chop_gdbm_read_block (chop_block_store_t *store,
		      const chop_block_key_t *key, chop_buffer_t *buffer,
		      size_t *size)
{
  errcode_t err;
  datum gdbm_key, gdbm_content;
  chop_gdbm_block_store_t *gdbm = (chop_gdbm_block_store_t *)store;

  CHOP_KEY_TO_GDBM (&gdbm_key, key);

  gdbm_content = gdbm_fetch (gdbm->db, gdbm_key);
  if (!gdbm_content.dptr)
    {
      *size = 0;
      return CHOP_STORE_BLOCK_UNAVAIL;
    }

  err = chop_buffer_push (buffer, gdbm_content.dptr, gdbm_content.dsize);
  *size = gdbm_content.dsize;

  free (gdbm_content.dptr);

  return err;
}

static errcode_t
chop_gdbm_write_block (chop_block_store_t *store,
		       const chop_block_key_t *key,
		       const char *buffer, size_t size)
{
  int err;
  chop_gdbm_block_store_t *gdbm = (chop_gdbm_block_store_t *)store;
  datum gdbm_key, gdbm_content;

  CHOP_KEY_TO_GDBM (&gdbm_key, key);
  gdbm_content.dptr = (char *)buffer;
  gdbm_content.dsize = size;

  err = gdbm_store (gdbm->db,
		    gdbm_key, gdbm_content,
		    GDBM_REPLACE /* FIXME: Really? */);
  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

static errcode_t
chop_gdbm_sync (chop_block_store_t *store)
{
  chop_gdbm_block_store_t *gdbm = (chop_gdbm_block_store_t *)store;

  gdbm_sync (gdbm->db);

  return 0;
}

static errcode_t
chop_gdbm_close (chop_block_store_t *store)
{
  chop_gdbm_block_store_t *gdbm = (chop_gdbm_block_store_t *)store;

  /* `gdbm_close ()' calls `gdbm_sync ()' */
  gdbm_close (gdbm->db);

  return 0;
}
