/* The TDB store */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>

#include <stdlib.h>
#include <tdb.h>
#include <errno.h>

/* `chop_tdb_block_store_t' inherits from `chop_block_store_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (tdb_block_store, block_store,
				      file_based_store_class,
				      TDB_CONTEXT *db;);

/* A generic open method, common to all file-based block stores.  */
static errcode_t
chop_tdb_generic_open (const chop_class_t *class,
		       const char *file, int open_flags, mode_t mode,
		       chop_block_store_t *store)
{
  if ((chop_file_based_store_class_t *)class != &chop_tdb_block_store_class)
    return CHOP_INVALID_ARG;

  return (chop_tdb_store_open (file, 0, TDB_DEFAULT,
			       open_flags, mode, store));
}

static void
tdb_dtor (chop_object_t *object)
{
  chop_tdb_block_store_t *store;

  store = (chop_tdb_block_store_t *)object;
  chop_store_close ((chop_block_store_t *)store);

  store->db = NULL;
  store->block_store.read_block = NULL;
  store->block_store.write_block = NULL;
  store->block_store.block_exists = NULL;
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (tdb_block_store, block_store,
				     file_based_store_class,

				     /* metaclass inits */
				     .generic_open = chop_tdb_generic_open,

				     NULL, tdb_dtor,
				     NULL, NULL  /* No serial/deserial */);




static errcode_t chop_tdb_block_exists (chop_block_store_t *,
					const chop_block_key_t *,
					int *);

static errcode_t chop_tdb_read_block (chop_block_store_t *,
				      const chop_block_key_t *,
				      chop_buffer_t *,
				      size_t *);

static errcode_t chop_tdb_write_block (chop_block_store_t *,
				       const chop_block_key_t *,
				       const char *,
				       size_t);

static errcode_t chop_tdb_delete_block (chop_block_store_t *,
					const chop_block_key_t *);

static errcode_t chop_tdb_first_key (chop_block_store_t *,
				     chop_block_key_t *);

static errcode_t chop_tdb_next_key (chop_block_store_t *,
				    const chop_block_key_t *,
				    chop_block_key_t *);

static errcode_t chop_tdb_sync (chop_block_store_t *);

static errcode_t chop_tdb_close (chop_block_store_t *);

/* #define DEBUG 1 */

#ifdef DEBUG
#include <stdio.h>
#include <stdarg.h>

static void
show_message (TDB_CONTEXT *db, int level, const char *message, ...)
{
  va_list args;
  char *fmt;

  fmt = alloca (strlen (message) + 15);
  strcpy (fmt, "tdb: ");
  strcat (fmt, message);
  strcat (fmt, "\n");

  va_start (args, message);
  vfprintf (stderr, fmt, args);
  va_end (args);
}
#endif

errcode_t
chop_tdb_store_open (const char *name,
		     int hash_size, int tdb_flags,
		     int open_flags, mode_t mode,
		     chop_block_store_t *s)
{
  TDB_CONTEXT *db;
  chop_tdb_block_store_t *store = (chop_tdb_block_store_t *)s;

  /* FIXME:  The `TDB_NOLOCK' thing works around what looks like a bug on the
     PowerPC (`tdb_store ()' eventually loops in `spin_lock ()').  */
  db = tdb_open (name, hash_size, TDB_DEFAULT | TDB_NOLOCK,
		 open_flags, mode);
  if (!db)
    return (errno);  /* FIXME:  Is this true?  */

  chop_object_initialize ((chop_object_t *)store,
			  (chop_class_t *)&chop_tdb_block_store_class);
  store->db = db;

  store->block_store.block_exists = chop_tdb_block_exists;
  store->block_store.read_block = chop_tdb_read_block;
  store->block_store.write_block = chop_tdb_write_block;
  store->block_store.delete_block = chop_tdb_delete_block;
  store->block_store.first_key = chop_tdb_first_key;
  store->block_store.next_key = chop_tdb_next_key;
  store->block_store.sync = chop_tdb_sync;
  store->block_store.close = chop_tdb_close;

#ifdef DEBUG
  tdb_logging_function (store->db, show_message);
#endif

  return 0;
}


/* Define all the macros expected by the generic database-based block store
   implementation.  */

#define DB_TYPE       tdb
#define DB_DATA_TYPE  TDB_DATA

#define DB_EXISTS(_db, _key)           tdb_exists ((_db), (_key))
#define DB_READ(_db, _key, _datap)  (*(_datap)) = tdb_fetch ((_db), (_key))
#define DB_WRITE(_db, _key, _data, _flags)  \
  tdb_store ((_db), (_key), (_data), (_flags))
#define DB_WRITE_REPLACE_FLAG  TDB_REPLACE
#define DB_DELETE(_db, _key)           tdb_delete ((_db), (_key))
#define DB_FIRST_KEY(_db)              tdb_firstkey ((_db))
#define DB_NEXT_KEY(_db, _key)         tdb_nextkey ((_db), (_key))
#define DB_SYNC(_db)                   /* No such function */
#define DB_CLOSE(_db)                  tdb_close ((_db))

/* Convert `chop_block_key_t' object CK into TDB key TDBK.  */
#define CHOP_KEY_TO_DB(_tdbk, _ck)			\
{							\
  (_tdbk)->dptr = (char *)chop_block_key_buffer (_ck);	\
  (_tdbk)->dsize = chop_block_key_size (_ck);		\
}

#include "store-generic-db.c"
