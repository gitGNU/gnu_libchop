/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2012, 2013  Ludovic Courtès <ludo@gnu.org>
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

/* The GDBM store */

#include <chop/chop-config.h>

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>

#include <stdlib.h>
#include <gdbm.h>
#include <errno.h>

/* `chop_gdbm_block_store_t' inherits from `chop_block_store_t' and its
   metaclass is `chop_file_based_store_class_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (gdbm_block_store, block_store,
				      file_based_store_class,
				      GDBM_FILE db;);

/* A generic open method, common to all file-based block stores.  */
static chop_error_t
chop_gdbm_generic_open (const chop_class_t *class,
			const char *file, int open_flags, mode_t mode,
			chop_block_store_t *store)
{
  if ((chop_file_based_store_class_t *)class != &chop_gdbm_block_store_class)
    return CHOP_INVALID_ARG;

  return (chop_gdbm_store_open (file, 0, open_flags, mode, NULL, store));
}

static void
gdbm_dtor (chop_object_t *object)
{
  chop_gdbm_block_store_t *store;

  store = (chop_gdbm_block_store_t *)object;

  store->db = NULL;
  store->block_store.read_block = NULL;
  store->block_store.write_block = NULL;
  store->block_store.blocks_exist = NULL;
}


CHOP_DEFINE_RT_CLASS_WITH_METACLASS (gdbm_block_store, block_store,
				     file_based_store_class,

				     /* metaclass inits */
				     .generic_open = chop_gdbm_generic_open,

				     NULL, gdbm_dtor,
				     NULL, NULL, /* No copy/equalp */
				     NULL, NULL  /* No serial/deserial */);



/* Iterators.  */

CHOP_DECLARE_RT_CLASS (gdbm_block_iterator, block_iterator,
		       /* Nothing to add.  */);

CHOP_DEFINE_RT_CLASS (gdbm_block_iterator, block_iterator,
		      NULL, NULL,
		      NULL, NULL,
		      NULL, NULL);



static chop_error_t chop_gdbm_blocks_exist (chop_block_store_t *,
					    size_t n,
					    const chop_block_key_t k[n],
					    bool e[n]);

static chop_error_t chop_gdbm_read_block (chop_block_store_t *,
					  const chop_block_key_t *,
					  chop_buffer_t *,
					  size_t *);

static chop_error_t chop_gdbm_write_block (chop_block_store_t *,
					   const chop_block_key_t *,
					   const char *,
					   size_t);

static chop_error_t chop_gdbm_delete_block (chop_block_store_t *,
					    const chop_block_key_t *);

static chop_error_t chop_gdbm_first_block (chop_block_store_t *,
					   chop_block_iterator_t *);

static chop_error_t chop_gdbm_it_next (chop_block_iterator_t *);

static chop_error_t chop_gdbm_sync (chop_block_store_t *);

static chop_error_t chop_gdbm_close (chop_block_store_t *);


chop_error_t
chop_gdbm_store_open (const char *name, size_t block_size,
		      int open_flags, mode_t mode,
		      void (* fatal_func) (const char *),
		      chop_block_store_t *s)
{
  GDBM_FILE db;
  chop_gdbm_block_store_t *store = (chop_gdbm_block_store_t *)s;
  int gdbm_flags = 0;

  if (open_flags & O_CREAT)
    gdbm_flags |= GDBM_WRCREAT;
  if (open_flags & O_RDONLY)
    gdbm_flags |= GDBM_READER;
  if (open_flags & O_WRONLY)
    gdbm_flags |= GDBM_WRITER;

  db = gdbm_open ((char *)name, block_size, gdbm_flags,
		  mode, fatal_func);
  if (!db)
    {
      if (gdbm_errno == GDBM_FILE_OPEN_ERROR)
	return ENOENT;
      else
	return (gdbm_errno ? gdbm_errno : errno);
    }

  chop_object_initialize ((chop_object_t *)store,
			  (chop_class_t *)&chop_gdbm_block_store_class);
  store->db = db;
  store->block_store.iterator_class = &chop_gdbm_block_iterator_class;

  store->block_store.blocks_exist = chop_gdbm_blocks_exist;
  store->block_store.read_block = chop_gdbm_read_block;
  store->block_store.write_block = chop_gdbm_write_block;
  store->block_store.delete_block = chop_gdbm_delete_block;
  store->block_store.first_block = chop_gdbm_first_block;
  store->block_store.sync = chop_gdbm_sync;
  store->block_store.close = chop_gdbm_close;

  return 0;
}


/* Define all the macros expected by the generic database-based block store
   implementation.  */

#define DB_TYPE       gdbm
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
