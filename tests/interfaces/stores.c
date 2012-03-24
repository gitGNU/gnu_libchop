/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2011  Ludovic Courtès <ludo@gnu.org>
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

/* Simple test of various block store classes.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/stores.h>

#include <testsuite.h>

#include <stdio.h>
#include <stdbool.h>

/* Write the SIZE bytes pointed to by BYTES under KEY in STORE.  */
static void
test_block (chop_block_store_t *store, const chop_block_key_t *key,
	    const char *bytes, size_t size)
{
  chop_error_t err;
  chop_buffer_t buffer;
  int exists = 0;
  size_t amount = 0;

  chop_buffer_init (&buffer, size);

  err = chop_store_block_exists (store, key, &exists);
  test_check_errcode (err, "calling `block_exists'");
  test_assert (!exists);

  err = chop_store_read_block (store, key, &buffer, &amount);
  test_assert (err == CHOP_STORE_BLOCK_UNAVAIL);
  test_assert (amount == 0 && chop_buffer_size (&buffer) == 0);

  /* Write */
  err = chop_store_write_block (store, key,
				bytes, size);
  if (err)
    {
      chop_error (err, "while writing to a `%s' store",
		  chop_class_name (chop_object_get_class
				   ((chop_object_t *) store)));
      exit (2);
    }

  /* Exists? */
  err = chop_store_block_exists (store, key, &exists);
  if (err)
    {
      chop_error (err, "while querying a `%s' store",
		  chop_class_name (chop_object_get_class
				   ((chop_object_t *) store)));
      exit (3);
    }

  /* Read */
  err = chop_store_read_block (store, key, &buffer, &amount);
  if (err)
    {
      chop_error (err, "while reading from a `%s' store",
		  chop_class_name (chop_object_get_class
				   ((chop_object_t *) store)));
      exit (4);
    }
  if (amount < size)
    {
      chop_error (0,
		  "read only %zu bytes instead of %zu from a `%s' store",
		  amount, size,
		  chop_class_name (chop_object_get_class
				   ((chop_object_t *) store)));
      exit (5);
    }

  chop_buffer_return (&buffer);
}

/* Return the index of K within KEYS, or abort if not found.  */
static size_t
key_index (size_t size, const chop_block_key_t keys[size],
	   const chop_block_key_t *k)
{
  size_t index;

  for (index = 0; index < size; index++)
    if (chop_block_key_equal (k, &keys[index]))
      return index;

  abort ();
}


int
main (int argc, char *argv[])
{
#define BLOCK_COUNT 12

  static const chop_file_based_store_class_t *classes[] =
    {
      &chop_gdbm_block_store_class,
      &chop_fs_block_store_class,
#ifdef HAVE_TDB
      &chop_tdb_block_store_class,
#endif
#ifdef HAVE_BDB
      &chop_bdb_block_store_class,
#endif
#ifdef HAVE_QDBM
      &chop_qdbm_block_store_class,
#endif
      NULL
    };
  static const char db_file[] = ",,t-stores.db";

  chop_error_t err;
  const chop_file_based_store_class_t **class;
  char random_bytes[64][BLOCK_COUNT];
  chop_block_key_t keys[BLOCK_COUNT];
  size_t i;

  test_init (argv[0]);
  test_init_random_seed ();

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  for (i = 0; i < BLOCK_COUNT; i++)
    {
      test_randomize_input (random_bytes[i], sizeof random_bytes[i]);
      chop_block_key_init (&keys[i], random_bytes[i], sizeof random_bytes[i],
			   NULL, NULL);
    }

  for (class = classes;
       *class != NULL;
       class++)
    {
      chop_error_t err;
      chop_block_store_t *store;
      chop_block_iterator_t *it;
      int exists = 0;

      test_stage ("the `%s' class",
		  chop_class_name ((chop_class_t *)*class));

      /* Open */
      store = (chop_block_store_t *)
	chop_class_alloca_instance ((chop_class_t *)*class);

      remove (db_file);
      err = chop_file_based_store_open (*class, db_file, O_RDWR | O_CREAT,
					S_IRUSR | S_IWUSR, store);
      if (err)
	{
	  chop_error (err, "while opening `%s' store",
		      chop_class_name ((chop_class_t *) *class));
	  exit (1);
	}

      /* Write BLOCK_COUNT blocks and make sure STORE is in a consistent
	 state.  */
      for (i = 0; i < BLOCK_COUNT; i++)
	test_block (store, &keys[i],
		    random_bytes[i], sizeof random_bytes[i]);

      /* Iterating over blocks (optional).  */
      if (chop_store_iterator_class (store))
	{
	  bool block_found[BLOCK_COUNT];
	  const chop_block_key_t *key;
	  const chop_class_t *it_class = chop_store_iterator_class (store);
	  it = chop_class_alloca_instance (it_class);

	  memset (block_found, 0, sizeof block_found);

	  err = chop_store_first_block (store, it);
	  test_check_errcode (err, "getting an iterator to the first block");
	  test_assert (!chop_block_iterator_is_nil (it));

	  key = chop_block_iterator_key (it);
	  test_assert (key != NULL);

	  block_found[key_index (BLOCK_COUNT, keys, key)] = true;

	  for (i = 1; i < BLOCK_COUNT; i++)
	    {
	      err = chop_block_iterator_next (it);
	      test_assert (!err);

	      key = chop_block_iterator_key (it);
	      test_assert (key != NULL);

	      test_assert (!block_found[key_index (BLOCK_COUNT, keys, key)]);
	      block_found[key_index (BLOCK_COUNT, keys, key)] = true;
	    }

	  err = chop_block_iterator_next (it);
	  test_assert (err == CHOP_STORE_END);
	  test_assert (chop_block_iterator_is_nil (it));

	  /* Make sure everything was found.  */
	  for (i = 0; i < BLOCK_COUNT; i++)
	    test_assert (block_found[i]);

	  chop_object_destroy ((chop_object_t *) it);
	}
      else
	test_stage_intermediate ("(no block iterators)");

      /* Delete.  */
      for (i = 0; i < BLOCK_COUNT; i++)
	{
	  err = chop_store_delete_block (store, &keys[i]);
	  test_assert (!err);

	  err = chop_store_block_exists (store, &keys[i], &exists);
	  test_assert (!err);
	  test_assert (!exists);
	}

      /* Close.  */
      err = chop_store_close (store);
      if (err)
	{
	  chop_error (err, "while closing a `%s' store",
		      chop_class_name ((chop_class_t *) *class));
	  exit (6);
	}

      chop_object_destroy ((chop_object_t *) store);

      test_stage_result (1);
    }

  unlink (db_file);

  return 0;
}
