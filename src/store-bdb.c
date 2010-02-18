/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

/* A store backed by SleepyCat's Berkeley DB.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>

#include <stdlib.h>
#include <db.h>  /* BDB 4.3/4.4 */
#include <errno.h>

/* `chop_bdb_block_store_t' inherits from `chop_block_store_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (bdb_block_store, block_store,
				      file_based_store_class,
				      DB *db;);

/* A generic open method, common to all file-based block stores.  */
static chop_error_t
chop_bdb_generic_open (const chop_class_t *class,
		       const char *file, int open_flags, mode_t mode,
		       chop_block_store_t *store)
{
  if ((chop_file_based_store_class_t *)class != &chop_bdb_block_store_class)
    return CHOP_INVALID_ARG;

  return (chop_bdb_store_open (file, DB_HASH,
			       open_flags, mode, store));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (bdb_block_store, block_store,
				     file_based_store_class,

				     /* metaclass inits */
				     .generic_open = chop_bdb_generic_open,

				     NULL, NULL, /* No ctor/dtor */
				     NULL, NULL, /* No copy/equalp */
				     NULL, NULL  /* No serial/deserial */);




/* Iterators.  */

CHOP_DECLARE_RT_CLASS (bdb_block_iterator, block_iterator,
		       DBC *cursor;);

static chop_error_t chop_bdb_it_next (chop_block_iterator_t *);

static chop_error_t
bbi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_bdb_block_iterator_t *it = (chop_bdb_block_iterator_t *)object;

  it->block_iterator.next = chop_bdb_it_next;
  it->cursor = NULL;

  return 0;
}

static void
bbi_dtor (chop_object_t *object)
{
  chop_bdb_block_iterator_t *it = (chop_bdb_block_iterator_t *)object;

  it->block_iterator.next = NULL;
  if (it->cursor)
    {
      it->cursor->c_close (it->cursor);
      it->cursor = NULL;
    }
}

CHOP_DEFINE_RT_CLASS (bdb_block_iterator, block_iterator,
		      bbi_ctor, bbi_dtor,
		      NULL, NULL,
		      NULL, NULL);


static void
do_free (char *ptr, void *thing)
{
  free (ptr);
}

static __inline__ chop_error_t
bdb_cursor_nextify (DBC *cursor, chop_block_key_t *key)
{
  int err;
  char buffer[2048]; /* maybe this could be parameterized */
  DBT db_key, db_data;

  memset (&db_key, 0, sizeof (db_key));
  db_key.flags = DB_DBT_MALLOC;

  /* Problem: to actually get a cursor to the next thing, we must fetch the
     data attached to the current pair, even if we don't want to.  So, in
     order to minimize use of the heap, we'll first try to get the data in
     our buffer on the stack.  If that doesn't work, then okay, we'll go for
     dynamic allocation.  */
  memset (&db_data, 0, sizeof (db_data));
  db_data.data = buffer;
  db_data.ulen = sizeof (buffer);
  db_data.flags = DB_DBT_USERMEM;

 doit:
  /* The first time, `DB_NEXT_NODUP' is equivalent to `DB_FIRST'.  */
  err = cursor->c_get (cursor, &db_key, &db_data, DB_NEXT_NODUP);

  switch (err)
    {
    case 0:
      break;

    case DB_BUFFER_SMALL:
      if (db_data.flags == DB_DBT_USERMEM)
	{
	  /* Crap.  We got the key, but not the data associated to it.  So if
	     we really want the next call to this function to succeed and
	     actually return the next element, we need to get the data for
	     good.  */
	  free (db_key.data);
	  memset (&db_key, 0, sizeof (db_key));
	  db_key.flags = DB_DBT_MALLOC;

	  memset (&db_data, 0, sizeof (db_data));
	  db_data.flags = DB_DBT_MALLOC;

	  goto doit;
	}
      else
	{
	  /* Gosh, we failed.  */
	  if (err == ENOMEM)
	    return err;
	  else
	    return CHOP_STORE_ERROR;
	}
      break;

    case DB_NOTFOUND:
      chop_block_key_free (key);
      return CHOP_STORE_END;

    default:
      cursor->c_close (cursor);
      chop_block_key_free (key);
      return CHOP_STORE_ERROR;
    }

  if ((db_data.flags == DB_DBT_MALLOC) && (db_data.data))
    /* Note: we could actually cache it so that a future call to
       `read_block_at (IT)' would be faster.  */
    free (db_data.data);

  if (!err)
    {
      chop_block_key_free (key);
      chop_block_key_init (key, db_key.data, db_key.size, do_free, NULL);
    }

  return err;
}

static chop_error_t
chop_bdb_first_block (chop_block_store_t *store,
		      chop_block_iterator_t *it)
{
  int err;
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;
  chop_bdb_block_iterator_t *bdb_it = (chop_bdb_block_iterator_t *)it;
  DBC *cursor;

  err = bdb->db->cursor (bdb->db, NULL, &cursor, 0);
  if (err)
    return CHOP_INVALID_ARG;

  err = chop_object_initialize ((chop_object_t *)it,
				&chop_bdb_block_iterator_class);
  if (err)
    {
      cursor->c_close (cursor);
      return err;
    }

  err = bdb_cursor_nextify (cursor, &it->key);
  if (err)
    return err;

  bdb_it->block_iterator.nil = 0;
  bdb_it->cursor = cursor;

  return 0;
}

static chop_error_t
chop_bdb_it_next (chop_block_iterator_t *it)
{
  chop_error_t err;
  chop_bdb_block_iterator_t *bdb_it = (chop_bdb_block_iterator_t *)it;

  if ((chop_block_iterator_is_nil (it)) || (!bdb_it->cursor))
    return CHOP_STORE_END;

  err = bdb_cursor_nextify (bdb_it->cursor, &it->key);
  if ((err) && (err != CHOP_STORE_END))
    bdb_it->cursor = NULL;

  bdb_it->block_iterator.nil = (err == CHOP_STORE_END) ? 1 : 0;

  return err;
}



static chop_error_t chop_bdb_block_exists (chop_block_store_t *,
					   const chop_block_key_t *,
					   int *);

static chop_error_t chop_bdb_read_block (chop_block_store_t *,
					 const chop_block_key_t *,
					 chop_buffer_t *,
					 size_t *);

static chop_error_t chop_bdb_write_block (chop_block_store_t *,
					  const chop_block_key_t *,
					  const char *,
					  size_t);

static chop_error_t chop_bdb_delete_block (chop_block_store_t *,
					   const chop_block_key_t *);

static chop_error_t chop_bdb_first_block (chop_block_store_t *,
					  chop_block_iterator_t *);

static chop_error_t chop_bdb_sync (chop_block_store_t *);

static chop_error_t chop_bdb_close (chop_block_store_t *);


/* XXX: We could either make this function further configurable, or provide a
   `chop_bdb_store_db ()' that would return a pointer to the underlying
   database.  */
chop_error_t
chop_bdb_store_open (const char *name, int db_type,
		     int open_flags, mode_t mode,
		     chop_block_store_t *s)
{
  int err;
  int bdb_open_flags = 0;
  DB *db;
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

  if (db_create (&db, NULL, 0))
    return CHOP_INVALID_ARG;

  err = db->open (db, NULL, name, NULL /* database */,
		  db_type /* e.g., `DB_HASH' */, bdb_open_flags, mode);
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

  err = chop_object_initialize ((chop_object_t *)store,
				(chop_class_t *)&chop_bdb_block_store_class);
  if (err)
    {
      db->close (db, 0);
      return err;
    }

  store->db = db;

  store->block_store.iterator_class = &chop_bdb_block_iterator_class;
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

static chop_error_t
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

static chop_error_t
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

static chop_error_t
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

static chop_error_t
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

  err = bdb->db->del (bdb->db, NULL, &db_key, 0);
  if (err == EINVAL)
    return CHOP_STORE_BLOCK_UNAVAIL;

  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

static chop_error_t
chop_bdb_sync (chop_block_store_t *store)
{
  int err;
  chop_bdb_block_store_t *bdb = (chop_bdb_block_store_t *)store;

  err = bdb->db->sync (bdb->db, 0);
  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

static chop_error_t
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


/* arch-tag: cfe9dad0-ea76-4ac2-bc70-4d6d59404332
 */
