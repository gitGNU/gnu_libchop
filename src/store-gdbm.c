/* The GDBM store */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>

#include <gdbm.h>
#include <errno.h>

struct chop_gdbm_block_store
{
  chop_block_store_t store;

  GDBM_FILE db;
};


static errcode_t chop_gdbm_read_block (chop_block_store_t *,
				       const chop_block_key_t *,
				       chop_buffer_t *,
				       size_t *);

static errcode_t chop_gdbm_write_block (chop_block_store_t *,
					const chop_block_key_t *,
					const char *,
					size_t);

errcode_t
chop_gdbm_store_open (const char *name, size_t block_size,
		      int mode, void (* fatal_func) (const char *),
		      chop_gdbm_block_store_t *store)
{
  store->db = gdbm_open ((char *)name, block_size, GDBM_WRCREAT,
			 mode, fatal_func);
  if (!store->db)
    return (gdbm_errno ? gdbm_errno : errno);

  store->store.read_block = chop_gdbm_read_block;
  store->store.write_block = chop_gdbm_write_block;

  return 0;
}

static errcode_t
chop_gdbm_read_block (chop_block_store_t *store,
		      const chop_block_key_t *key, chop_buffer_t *buffer,
		      size_t *size)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_gdbm_write_block (chop_block_store_t *store,
		       const chop_block_key_t *key,
		       const char *buffer, size_t size)
{
  int err;
  chop_gdbm_block_store_t *gdbm = (chop_gdbm_block_store_t *)store;
  datum gdbm_key, gdbm_content;

  gdbm_key.dptr = (char *)chop_block_key_buffer (key);
  gdbm_key.dsize = chop_block_key_size (key);
  gdbm_content.dptr = (char *)buffer;
  gdbm_content.dsize = size;

  err = gdbm_store (gdbm->db,
		    gdbm_key, gdbm_content,
		    GDBM_REPLACE /* FIXME: Really? */);
  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

