/* A store backed by SleepyCat's Berkeley DB.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>

#include <stdlib.h>
#include <db.h>  /* BDB 4.3 */
#include <errno.h>

/* `chop_bdb_block_store_t' inherits from `chop_block_store_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (bdb_block_store, block_store,
				      file_based_store_class,
				      DB_ENV *db_env;
				      DB *db;);

/* A generic open method, common to all file-based block stores.  */
static errcode_t
chop_bdb_generic_open (const chop_class_t *class,
		       const char *file, int open_flags, mode_t mode,
		       chop_block_store_t *store)
{
  if ((chop_file_based_store_class_t *)class != &chop_bdb_block_store_class)
    return CHOP_INVALID_ARG;

  return (chop_bdb_store_open (file, 0, 0 /* FIXME: BDB_DEFAULT */,
			       open_flags, mode, store));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (bdb_block_store, block_store,
				     file_based_store_class,

				     /* metaclass inits */
				     .generic_open = chop_bdb_generic_open,

				     NULL, NULL, /* No ctor/dtor */
				     NULL, NULL  /* No serial/deserial */);




static errcode_t chop_bdb_block_exists (chop_block_store_t *,
					const chop_block_key_t *,
					int *);

static errcode_t chop_bdb_read_block (chop_block_store_t *,
				      const chop_block_key_t *,
				      chop_buffer_t *,
				      size_t *);

static errcode_t chop_bdb_write_block (chop_block_store_t *,
				       const chop_block_key_t *,
				       const char *,
				       size_t);

static errcode_t chop_bdb_delete_block (chop_block_store_t *,
					const chop_block_key_t *);

static errcode_t chop_bdb_first_block (chop_block_store_t *,
				       chop_block_iterator_t *);

static errcode_t chop_bdb_sync (chop_block_store_t *);

static errcode_t chop_bdb_close (chop_block_store_t *);


errcode_t
chop_bdb_store_open (const char *name,
		     int hash_size, int bdb_flags,
		     int open_flags, mode_t mode,
		     chop_block_store_t *s)
{
  int err;
  int bdb_open_flags = 0;
  chop_bdb_block_store_t *store = (chop_bdb_block_store_t *)s;

  if (open_flags & O_CREAT)
    bdb_open_flags |= DB_CREATE;
  if (open_flags & O_RDONLY)
    bdb_open_flags |= DB_RDONLY;
  if (open_flags & O_WRONLY)
    bdb_open_flags |= 0 /* ? */;

#if 0
  if (db_env_create (&store->db_env, 0))
    return CHOP_STORE_ERROR;
#endif

  if (db_create (&store->db, NULL, 0))
    return CHOP_INVALID_ARG;

  err = store->db->open (store->db, NULL, name, NULL /* database */,
			 DB_HASH /* type */, bdb_open_flags, mode);
  switch (err)
    {
    case 0:
      break;

    case ENOENT:
    case EEXIST:
    case EINVAL:
      return err;

    default:
      return CHOP_STORE_ERROR;
    }

  store->block_store.iterator_class = NULL; /* FIXME */
  store->block_store.block_exists = chop_bdb_block_exists;
  store->block_store.read_block = chop_bdb_read_block;
  store->block_store.write_block = chop_bdb_write_block;
  store->block_store.delete_block = chop_bdb_delete_block;
  store->block_store.first_block = chop_bdb_first_block;
  store->block_store.sync = chop_bdb_sync;
  store->block_store.close = chop_bdb_close;

  return 0;
}


/* Method implementations.  */

static errcode_t
chop_bdb_block_exists (chop_block_store_t *store,
		       const chop_block_key_t *key,
		       int *exists)
{
  int err;
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;
  DBT thing, db_key;

  memset (&thing, 0, sizeof (thing));
  thing.flags = DB_DBT_USERMEM;

  memset (&db_key, 0, sizeof (db_key));
  db_key.data = (char *)chop_block_key_buffer (key);
  db_key.ulen = db_key.size = chop_block_key_size (key);
  db_key.flags = DB_DBT_USERMEM;

  err = bdb->db->get (bdb->db, NULL, &db_key, &thing, 0);
  if (err == DB_BUFFER_SMALL)
    {
      *exists = 1;
      return 0;
    }

  if (err == DB_NOTFOUND)
    {
      *exists = 0;
      return 0;
    }

  *exists  = 0;
  return CHOP_STORE_ERROR;
}

static errcode_t
chop_bdb_read_block (chop_block_store_t *store,
		     const chop_block_key_t *key, chop_buffer_t *buffer,
		     size_t *size)
{
  int err;
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;
  DBT thing, db_key;

  memset (&thing, 0, sizeof (thing));
  thing.flags = DB_DBT_MALLOC;

  memset (&db_key, 0, sizeof (db_key));
  db_key.data = (char *)chop_block_key_buffer (key);
  db_key.ulen = db_key.size = chop_block_key_size (key);
  db_key.flags = DB_DBT_USERMEM;

  err = bdb->db->get (bdb->db, NULL, &db_key, &thing, 0);
  if ((err == DB_NOTFOUND) || (!thing.data) || (!thing.size))
    {
      *size = 0;
      return CHOP_STORE_BLOCK_UNAVAIL;
    }

  err = chop_buffer_push (buffer, thing.data, thing.size);
  if (err)
    *size = 0;
  else
    *size = thing.size;

  free (thing.data);

  return err;
}

static errcode_t
chop_bdb_write_block (chop_block_store_t *store,
		      const chop_block_key_t *key,
		      const char *buffer, size_t size)
{
  int err;
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;
  DBT thing, db_key;

  memset (&thing, 0, sizeof (thing));
  thing.data = (char *)buffer;
  thing.ulen = thing.size = size;
  thing.flags = DB_DBT_USERMEM;

  memset (&db_key, 0, sizeof (db_key));
  db_key.data = (char *)chop_block_key_buffer (key);
  db_key.ulen = db_key.size = chop_block_key_size (key);
  db_key.flags = DB_DBT_USERMEM;

  err = bdb->db->put (bdb->db, NULL, &db_key, &thing,
		      0 /* replace */);
  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

static errcode_t
chop_bdb_delete_block (chop_block_store_t *store,
		       const chop_block_key_t *key)
{
  int err;
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;
  DBT db_key;

  memset (&db_key, 0, sizeof (db_key));
  db_key.data = (char *)chop_block_key_buffer (key);
  db_key.ulen = db_key.size = chop_block_key_size (key);
  db_key.flags = DB_DBT_USERMEM;

  bdb->db->del (bdb->db, NULL, &db_key, 0);
  if (err == EINVAL)
    return CHOP_STORE_BLOCK_UNAVAIL;

  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

static errcode_t
chop_bdb_first_block (chop_block_store_t *store,
		      chop_block_iterator_t *it)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_bdb_sync (chop_block_store_t *store)
{
  int err;
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;

  err = bdb->db->sync (bdb->db, 0);
  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

static errcode_t
chop_bdb_close (chop_block_store_t *store)
{
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;

  if (bdb->db)
    {
      int err;

      err = bdb->db->close (bdb->db, 0);

      /* The handler may no longer be accessed, regardless of the return
	 value.  */
      bdb->db = NULL;

#if 0
      err |= bdb->db_env->close (bdb->db_env, 0);
      bdb->db_env = NULL;
#endif

      if (err)
	return CHOP_STORE_ERROR;
    }

  return 0;
}


#if 0 /* Experimental use of `store-generic-db.c'.  */
/* Define all the macros expected by the generic database-based block store
   implementation.  */

#define DB_TYPE       bdb
#define DB_DATA_TYPE  DBT

#define DB_EXISTS(_db, _key)          (0) /* FIXME */
#define DB_READ(_db, _key, _datap)		\
  (_db)->get ((_db), NULL, (_key), (_datap), 0)
#define DB_WRITE(_db, _key, _datap, _flags)	\
  (_db)->put ((_db), NULL, (_key), (_datap), (_flags))
#define DB_WRITE_REPLACE_FLAG          (0)
#define DB_DELETE(_db, _key)			\
  (_db)->del ((_db), NULL, (_key), 0)

#define DB_SYNC(_db)				\
  (_db)->sync (_db)
#define DB_CLOSE(_db)				\
  (_db)->close (_db)

/* Convert `chop_block_key_t' object CK into BDB key BDBK.  */
#define CHOP_KEY_TO_DB(_bdbk, _ck)			\
{							\
  memset ((_bdbk), 0, sizeof (_bdbk));			\
  (_bdbk)->data = (char *)chop_block_key_buffer (_ck);	\
  (_bdbk)->size = chop_block_key_size (_ck);		\
  (_bdbk)->ulen = chop_block_key_size (_ck);		\
  (_bdbk)->flags = DB_DBT_USERMEM;			\
}

/* Note: since `DB_FIRST_KEY' and `DB_NEXT_KEY' are not defined, this will
   not generate the corresponding methods.  */
#include "store-generic-db.c"

#endif


/* arch-tag: cfe9dad0-ea76-4ac2-bc70-4d6d59404332
 */

