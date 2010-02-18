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

/* Simple test of various block store classes.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/stores.h>

#include <testsuite.h>

#include <stdio.h>



int
main (int argc, char *argv[])
{
  static const chop_file_based_store_class_t *classes[] =
    {
      &chop_gdbm_block_store_class,
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
  char random_bytes[256];
  chop_block_key_t random_key;
  chop_buffer_t buffer;

  test_init (argv[0]);
  test_init_random_seed ();

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  test_randomize_input (random_bytes, sizeof (random_bytes));
  chop_block_key_init (&random_key, random_bytes, sizeof (random_bytes),
		       NULL, NULL);
  chop_buffer_init (&buffer, sizeof (random_bytes));

  for (class = classes;
       *class != NULL;
       class++)
    {
      chop_error_t err;
      int exists = 0;
      size_t amount = 0;
      chop_block_store_t *store;
      chop_block_iterator_t *it;

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

      /* Write */
      err = chop_store_write_block (store, &random_key,
				    random_bytes, sizeof (random_bytes));
      if (err)
	{
	  chop_error (err, "while writing to a `%s' store",
		      chop_class_name ((chop_class_t *) *class));
	  exit (2);
	}

      /* Exists? */
      err = chop_store_block_exists (store, &random_key, &exists);
      if (err)
	{
	  chop_error (err, "while querying a `%s' store",
		      chop_class_name ((chop_class_t *) *class));
	  exit (3);
	}

      /* Read */
      err = chop_store_read_block (store, &random_key, &buffer, &amount);
      if (err)
	{
	  chop_error (err, "while reading from a `%s' store",
		      chop_class_name ((chop_class_t *) *class));
	  exit (4);
	}
      if (amount < sizeof (random_bytes))
	{
	  chop_error (0,
		      "read only %zu bytes instead of %zu from a `%s' store",
		      amount, sizeof (random_bytes),
		      chop_class_name ((chop_class_t *) *class));
	  exit (5);
	}

      /* Iterating over blocks (optional).  */
      if (chop_store_iterator_class (store))
	{
	  const chop_block_key_t *key;
	  const chop_class_t *it_class = chop_store_iterator_class (store);
	  it = chop_class_alloca_instance (it_class);

	  err = chop_store_first_block (store, it);
	  test_check_errcode (err, "getting an iterator to the first block");
	  test_assert (!chop_block_iterator_is_nil (it));

	  key = chop_block_iterator_key (it);
	  test_assert (key != NULL);

	  test_assert (chop_block_key_equal (key, &random_key));

	  err = chop_block_iterator_next (it);
	  test_assert (err == CHOP_STORE_END);
	  test_assert (chop_block_iterator_is_nil (it));

	  chop_object_destroy ((chop_object_t *)it);
	}
      else
	test_stage_intermediate ("(no block iterators)");

      /* Delete.  */
      err = chop_store_delete_block (store, &random_key);
      test_assert (!err);

      err = chop_store_block_exists (store, &random_key, &exists);
      test_assert (!err);
      test_assert (!exists);

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

  return 0;
}

