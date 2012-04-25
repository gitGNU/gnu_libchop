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

#include <chop/chop.h>
#include <chop/stores.h>


static chop_error_t
store_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_block_store_t *store =
    (chop_block_store_t *)object;

  /* Initialize the block store fields so that method pointers are guaranteed
     to either be NULL or point to actual methods.  */
  store->name = NULL;
  store->blocks_exist = NULL;
  store->read_block = NULL;
  store->write_block = NULL;
  store->delete_block = NULL;
  store->iterator_class = NULL;
  store->first_block = NULL;
  store->close = NULL;
  store->sync = NULL;

  return 0;
}

static void
store_dtor (chop_object_t *object)
{
  chop_block_store_t *store =
    (chop_block_store_t *)object;

  /* This guarantees that all stores are closed when they are destroyed.  */
  chop_store_close (store);

  if (store->name != NULL)
    chop_free (store->name, chop_object_get_class (object));

  store->name = NULL;
  store->blocks_exist = NULL;
  store->read_block = NULL;
  store->write_block = NULL;
  store->delete_block = NULL;
  store->iterator_class = NULL;
  store->first_block = NULL;
  store->close = NULL;
  store->sync = NULL;
}

CHOP_DEFINE_RT_CLASS (block_store, object,
		      store_ctor, store_dtor,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);

/* The meta-class for all file-based stores, i.e. GDBM, TDB, etc.  */
CHOP_DEFINE_RT_CLASS (file_based_store_class, class,
		      NULL, NULL,
		      NULL, NULL,
		      NULL, NULL);


chop_error_t
chop_file_based_store_open (const chop_file_based_store_class_t *db_class,
			    const char *file, int open_flags, mode_t mode,
			    chop_block_store_t *store)
{
  if (!db_class->generic_open)
    return CHOP_ERR_NOT_IMPL;

  return (db_class->generic_open ((chop_class_t *)db_class, file,
				  open_flags, mode, store));
}


/* Block iterators.  */

static chop_error_t
bi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_block_iterator_t *bi = (chop_block_iterator_t *)object;

  bi->nil = 1;
  bi->store = NULL;
  bi->next = NULL;
  chop_block_key_init (&bi->key, NULL, 0, NULL, NULL);

  return 0;
}

static void
bi_dtor (chop_object_t *object)
{
  chop_block_iterator_t *bi = (chop_block_iterator_t *)object;

  bi->nil = 1;
  bi->store = NULL;
  bi->next = NULL;
  chop_block_key_free (&bi->key);
}

CHOP_DEFINE_RT_CLASS (block_iterator, object,
		      bi_ctor, bi_dtor,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL /* No serializer/deserializer */);


