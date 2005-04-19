/* Simple test of various block store classes.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/chop-config.h>

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
      NULL
    };
  static const char db_file[] = "test.db";
  const chop_file_based_store_class_t **class;
  char random_bytes[256];
  chop_block_key_t random_key;
  chop_buffer_t buffer;

  chop_block_key_init (&random_key, random_bytes, sizeof (random_bytes),
		       NULL, NULL);
  chop_buffer_init (&buffer, sizeof (random_bytes));

  for (class = classes;
       *class != NULL;
       class++)
    {
      errcode_t err;
      size_t amount = 0;
      chop_block_store_t *store;

      printf ("** Testing the `%s' class...\n",
	      chop_class_name ((chop_class_t *)*class));

      /* Open */
      store = (chop_block_store_t *)
	chop_class_alloca_instance ((chop_class_t *)*class);

      remove (db_file);
      err = chop_file_based_store_open (*class, db_file, O_RDWR | O_CREAT,
					S_IRUSR | S_IWUSR, store);
      if (err)
	{
	  com_err (argv[0], err, "while opening `%s' store",
		   chop_class_name ((chop_class_t *)*class));
	  exit (1);
	}

      /* Write */
      err = chop_store_write_block (store, &random_key,
				    random_bytes, sizeof (random_bytes));
      if (err)
	{
	  com_err (argv[0], err, "while writing to a `%s' store",
		   chop_class_name ((chop_class_t *)*class));
	  exit (2);
	}


      /* Read */
      err = chop_store_read_block (store, &random_key, &buffer, &amount);
      if (err)
	{
	  com_err (argv[0], err, "while reading from a `%s' store",
		   chop_class_name ((chop_class_t *)*class));
	  exit (4);
	}
      if (amount < sizeof (random_bytes))
	{
	  com_err (argv[0], 0,
		   "read only %u bytes instead of %u from a `%s' store",
		   amount, sizeof (random_bytes),
		   chop_class_name ((chop_class_t *)*class));
	  exit (5);
	}

      err = chop_store_close (store);
      if (err)
	{
	  com_err (argv[0], err, "while closing a `%s' store",
		   chop_class_name ((chop_class_t *)*class));
	  exit (6);
	}
    }

  return 0;
}

