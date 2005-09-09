/* This is a high-level test that runs several indexers and, for each of
   these, tests whether (i) it is able to index a whole stream and (ii) it is
   able to fetch the newly indexed stream and provide consistent data.  */

#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/indexers.h>

#include <testsuite.h>

#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <sys/time.h>
#include <assert.h>


/* Convenience macros.  */

#define STORE_FILE_NAME  ",,t-indexers-store.db"

#define INIT_STORES()							\
do									\
{									\
  unlink (STORE_FILE_NAME);						\
  err = chop_file_based_store_open ((chop_file_based_store_class_t *)	\
				    store_class,			\
				    STORE_FILE_NAME,			\
				    O_RDWR | O_CREAT,			\
				    S_IRUSR | S_IWUSR,			\
				    store);				\
  if (err)								\
    {									\
      com_err (argv[0], err, "while opening `%s' store `%s'",		\
	       chop_class_name (store_class),				\
	       STORE_FILE_NAME);					\
      exit (2);								\
    }									\
									\
  metastore = store;							\
}									\
while (0)

#define INIT_CHOPPER()						\
do								\
{								\
  block_size = (random () % 1000) + 3500;			\
  err = chop_fixed_size_chopper_init (stream,			\
				      block_size,		\
				      0 /* Don't pad blocks */,	\
				      chopper);			\
  if (err)							\
    {								\
      com_err (argv[0], err, "while initializing chopper");	\
      exit (2);							\
    }								\
}								\
while (0)



int
main (int argc, char *argv[])
{
  static char mem_stream_contents[1000007];

  errcode_t err;
  chop_stream_t *stream;
  chop_class_t *store_class;
  chop_block_store_t *store, *metastore;
  chop_chopper_t *chopper;
  chop_indexer_t *indexers[5];
  chop_indexer_t **current_indexer;
  chop_index_handle_t *handle;
  char buffer[4001];
  size_t block_size;
  char *mem;
  struct timeval tv;

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  /* Initialize two block stores: one for data, and one for meta-data.  */
#ifdef HAVE_TDB
  store_class = (chop_class_t *)&chop_tdb_block_store_class;
#else
  store_class = (chop_class_t *)&chop_gdbm_block_store_class;
#endif

  store = chop_class_alloca_instance (store_class);
  metastore = store;

  /* Randomize the input stream.  */
  gettimeofday (&tv, NULL);
  srandom (tv.tv_sec);
  for (mem = mem_stream_contents;
       mem - mem_stream_contents < sizeof (mem_stream_contents);
       mem++)
    {
      *mem = random () % 255;
    }

  /* Allocate room for a chopper and a stream (initialized later).  */
  stream = chop_class_alloca_instance ((chop_class_t *)
				       &chop_mem_stream_class);
  chopper = chop_class_alloca_instance ((chop_class_t *)
					&chop_fixed_size_chopper_class);

  /* Initialize of series of indexers to be tested.  */
  indexers[0] = chop_class_alloca_instance (&chop_hash_tree_indexer_class);
  err = chop_hash_tree_indexer_open (CHOP_HASH_NONE, CHOP_HASH_SHA1,
				     CHOP_CIPHER_HANDLE_NIL,
				     12,
				     indexers[0]);
  if (err)
    goto indexer_error;

  indexers[1] = chop_class_alloca_instance (&chop_hash_tree_indexer_class);
  err = chop_hash_tree_indexer_open (CHOP_HASH_NONE, CHOP_HASH_SHA256,
				     CHOP_CIPHER_HANDLE_NIL,
				     47,
				     indexers[1]);
  if (err)
    goto indexer_error;

  indexers[2] = chop_class_alloca_instance (&chop_hash_tree_indexer_class);
  err = chop_hash_tree_indexer_open (CHOP_HASH_SHA1, CHOP_HASH_SHA1,
				     chop_cipher_open (CHOP_CIPHER_BLOWFISH,
						       CHOP_CIPHER_MODE_ECB),
				     40,
				     indexers[2]);
  if (err)
    goto indexer_error;

  indexers[3] = NULL;

 indexer_error:
  test_check_errcode (err, "opening tree hash indexer");


  /* Go ahead! */
  for (current_indexer = indexers;
       *current_indexer;
       current_indexer++)
    {
      size_t bytes_fetched = 0;
      chop_stream_t *fetched_stream;

      test_stage ("indexer #%u", current_indexer - indexers + 1);
      chop_mem_stream_open (mem_stream_contents, sizeof (mem_stream_contents),
			    NULL, stream);
      INIT_CHOPPER ();
      INIT_STORES ();

      handle = chop_indexer_alloca_index_handle (*current_indexer);

      /* Index STREAM.  */
      test_stage_intermediate ("indexing");
      err = chop_indexer_index_blocks (*current_indexer, chopper,
				       store, metastore, handle);
      if ((err) && (err != CHOP_STREAM_END))
	{
	  com_err (argv[0], err, "while indexing blocks");
	  exit (7);
	}

      /* Fetch the stream.  */
      test_stage_intermediate ("fetching");
      fetched_stream =
	chop_class_alloca_instance
	(chop_indexer_index_handle_class (*current_indexer));
      err = chop_indexer_fetch_stream (*current_indexer, handle,
				       store, metastore, fetched_stream);

      test_check_errcode (err, "fetching indexed stream");

      test_stage_intermediate ("reading");
      for (mem = mem_stream_contents;
	   ((mem - mem_stream_contents < sizeof (mem_stream_contents))
	    && (!err));
	   mem++)
	{
	  size_t amount;

	  err = chop_stream_read (fetched_stream, buffer, sizeof (buffer),
				  &amount);
	  if (!err)
	    {
	      assert (bytes_fetched + amount <= sizeof (mem_stream_contents));
	      assert (!memcmp (mem_stream_contents + bytes_fetched,
			       buffer, amount));
	      bytes_fetched += amount;
	    }
	}

      if (err != CHOP_STREAM_END)
	{
	  com_err (argv[0], err, "while reading from fetched stream");
	  exit (10);
	}

      test_assert (bytes_fetched == sizeof (mem_stream_contents));

      chop_stream_close (stream);

      /* Close the stores.  */
      err = chop_store_close (store);
      if ((!err) && (metastore != store))
	err = chop_store_close (metastore);

      test_check_errcode (err, "closing stores");

      chop_object_destroy ((chop_object_t *)handle);
      test_stage_result (1);
    }

  return 0;
}

