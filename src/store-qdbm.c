/* The QDBM store.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>

#include <stdlib.h>
#include <errno.h>

/* We QDBM's GDBM-compatibile API, calle `hovel'.  So this file is almost
   unchanged compared to `store-gdbm.c'.  I don't see the point of using one
   of the incompatible APIs of QDBM since they remain pretty close to
   GDBM's.  */

/* FIXME:  Actually, it's probably not a good idea since at run-time, we
   don't know whether the symbols from libgdbm or those from libqdbm will be
   used.  */
#include <hovel.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>


/* `chop_qdbm_block_store_t' inherits from `chop_block_store_t' and its
   metaclass is `chop_file_based_store_class_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (qdbm_block_store, block_store,
				      file_based_store_class,
				      GDBM_FILE db;);

/* A generic open method, common to all file-based block stores.  */
static errcode_t
chop_qdbm_generic_open (const chop_class_t *class,
			const char *file, int open_flags, mode_t mode,
			chop_block_store_t *store)
{
  if ((chop_file_based_store_class_t *)class != &chop_qdbm_block_store_class)
    return CHOP_INVALID_ARG;

  return (chop_qdbm_store_open (file, 0, open_flags, mode, store));
}


static void
qdbm_dtor (chop_object_t *object)
{
  chop_qdbm_block_store_t *store;

  store = (chop_qdbm_block_store_t *)object;
  chop_store_close ((chop_block_store_t *)store);

  store->db = NULL;
  store->block_store.read_block = NULL;
  store->block_store.write_block = NULL;
  store->block_store.block_exists = NULL;
}


CHOP_DEFINE_RT_CLASS_WITH_METACLASS (qdbm_block_store, block_store,
				     file_based_store_class,

				     /* metaclass inits */
				     .generic_open = chop_qdbm_generic_open,

				     NULL, qdbm_dtor,
				     NULL, NULL  /* No serial/deserial */);




static errcode_t chop_qdbm_block_exists (chop_block_store_t *,
					 const chop_block_key_t *,
					 int *);

static errcode_t chop_qdbm_read_block (chop_block_store_t *,
				       const chop_block_key_t *,
				       chop_buffer_t *,
				       size_t *);

static errcode_t chop_qdbm_write_block (chop_block_store_t *,
					const chop_block_key_t *,
					const char *,
					size_t);

static errcode_t chop_qdbm_delete_block (chop_block_store_t *,
					 const chop_block_key_t *);

static errcode_t chop_qdbm_first_key (chop_block_store_t *,
				      chop_block_key_t *);

static errcode_t chop_qdbm_next_key (chop_block_store_t *,
				     const chop_block_key_t *,
				     chop_block_key_t *);

static errcode_t chop_qdbm_sync (chop_block_store_t *);

static errcode_t chop_qdbm_close (chop_block_store_t *);


errcode_t
chop_qdbm_store_open (const char *name, size_t block_size,
		      int open_flags, mode_t mode,
		      chop_block_store_t *s)
{
  GDBM_FILE db;
  chop_qdbm_block_store_t *store = (chop_qdbm_block_store_t *)s;
  int gdbm_flags = 0;

  /* See `FIXME' above...  */
  fprintf (stderr, "%s: warning: the QDBM store is still experimental\n",
	   __FUNCTION__);

  if (open_flags & O_CREAT)
    gdbm_flags |= GDBM_WRCREAT;
  if (open_flags & O_RDONLY)
    gdbm_flags |= GDBM_READER;
  if (open_flags & O_WRONLY)
    gdbm_flags |= GDBM_WRITER;

  db = gdbm_open ((char *)name, block_size, gdbm_flags,
		  mode, NULL /* FIXME:  Incompatibility */);
  if (!db)
    {
      if (gdbm_errno == GDBM_FILE_OPEN_ERROR)
	return ENOENT;
      else
	return (gdbm_errno ? gdbm_errno : errno);
    }

  chop_object_initialize ((chop_object_t *)store,
			  (chop_class_t *)&chop_qdbm_block_store_class);
  store->db = db;

  store->block_store.block_exists = chop_qdbm_block_exists;
  store->block_store.read_block = chop_qdbm_read_block;
  store->block_store.write_block = chop_qdbm_write_block;
  store->block_store.delete_block = chop_qdbm_delete_block;
  store->block_store.first_key = chop_qdbm_first_key;
  store->block_store.next_key = chop_qdbm_next_key;
  store->block_store.sync = chop_qdbm_sync;
  store->block_store.close = chop_qdbm_close;

  return 0;
}


/* Define all the macros expected by the generic database-based block store
   implementation.  */

#define DB_TYPE       qdbm
#define DB_DATA_TYPE  datum
#define DB_EXISTS(_db, _key)           gdbm_exists ((_db), (_key))
#define DB_READ(_db, _key, _datap)  (*(_datap)) = gdbm_fetch ((_db), (_key))
#define DB_WRITE(_db, _key, _data, _flags)  \
  gdbm_store ((_db), (_key), (_data), (_flags))
#define DB_WRITE_REPLACE_FLAG  GDBM_REPLACE
#define DB_DELETE(_db, _key)           gdbm_delete ((_db), (_key))
#define DB_FIRST_KEY(_db)              gdbm_firstkey ((_db))
#define DB_NEXT_KEY(_db, _key)         gdbm_nextkey ((_db), (_key))
#define DB_SYNC(_db)                   gdbm_sync ((_db))
#define DB_CLOSE(_db)                  gdbm_close ((_db))

/* Convert `chop_block_key_t' object CK into GDBM key GDBMK.  */
#define CHOP_KEY_TO_DB(_gdbmk, _ck)			\
{							\
  (_gdbmk)->dptr = (char *)chop_block_key_buffer (_ck);	\
  (_gdbmk)->dsize = chop_block_key_size (_ck);		\
}

#include "store-generic-db.c"
