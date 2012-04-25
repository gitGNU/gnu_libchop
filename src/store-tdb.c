/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2012  Ludovic Courtès <ludo@gnu.org>
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

/* The TDB store */

#include <alloca.h>

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
static chop_error_t
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

  store->db = NULL;
  store->block_store.read_block = NULL;
  store->block_store.write_block = NULL;
  store->block_store.blocks_exist = NULL;
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (tdb_block_store, block_store,
				     file_based_store_class,

				     /* metaclass inits */
				     .generic_open = chop_tdb_generic_open,

				     NULL, tdb_dtor,
				     NULL, NULL, /* No copy/equalp */
				     NULL, NULL  /* No serial/deserial */);




/* Iterators.  */

CHOP_DECLARE_RT_CLASS (tdb_block_iterator, block_iterator,
		       /* Nothing to add.  */);

CHOP_DEFINE_RT_CLASS (tdb_block_iterator, block_iterator,
		      NULL, NULL,
		      NULL, NULL,
		      NULL, NULL);


static chop_error_t chop_tdb_blocks_exist (chop_block_store_t *,
					   size_t n,
					   const chop_block_key_t k[n],
					   bool e[n]);

static chop_error_t chop_tdb_read_block (chop_block_store_t *,
					 const chop_block_key_t *,
					 chop_buffer_t *,
					 size_t *);

static chop_error_t chop_tdb_write_block (chop_block_store_t *,
					  const chop_block_key_t *,
					  const char *,
					  size_t);

static chop_error_t chop_tdb_delete_block (chop_block_store_t *,
					   const chop_block_key_t *);

static chop_error_t chop_tdb_first_block (chop_block_store_t *,
					  chop_block_iterator_t *);

static chop_error_t chop_tdb_it_next (chop_block_iterator_t *);

static chop_error_t chop_tdb_sync (chop_block_store_t *);

static chop_error_t chop_tdb_close (chop_block_store_t *);

/* #define DEBUG 1 */

#ifdef DEBUG
#include <stdio.h>
#include <stdarg.h>

static void
show_message (TDB_CONTEXT *db, enum tdb_debug_level level, const char *message, ...)
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

chop_error_t
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
    {
      int err = errno;
      return (err ? err : CHOP_INVALID_ARG);
    }

  chop_object_initialize ((chop_object_t *)store,
			  (chop_class_t *)&chop_tdb_block_store_class);
  store->db = db;
  store->block_store.iterator_class = &chop_tdb_block_iterator_class;

  store->block_store.blocks_exist = chop_tdb_blocks_exist;
  store->block_store.read_block = chop_tdb_read_block;
  store->block_store.write_block = chop_tdb_write_block;
  store->block_store.delete_block = chop_tdb_delete_block;
  store->block_store.first_block = chop_tdb_first_block;
  store->block_store.sync = chop_tdb_sync;
  store->block_store.close = chop_tdb_close;

#ifdef DEBUG
  {
    struct tdb_logging_context log;

    log.log_fn = show_message;
    log.log_private = NULL;
    tdb_set_logging_function (store->db, &log);
  }
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
#define CHOP_KEY_TO_DB(_tdbk, _ck)					\
{									\
  (_tdbk)->dptr = (unsigned char *) chop_block_key_buffer (_ck);	\
  (_tdbk)->dsize = chop_block_key_size (_ck);				\
}

#include "store-generic-db.c"
