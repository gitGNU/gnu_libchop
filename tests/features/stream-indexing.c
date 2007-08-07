/* A high-level test that seeks to perform a complete stream
   indexing/fetching test with different configurations.

   We just very naively test every possible indexer/chopper/block-indexer
   configuration (there aren't so many of them).  The test also exercises the
   deserialization API a bit.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>
#include <chop/block-indexers.h>
#include <chop/indexers.h>
#include <chop/stores.h>

#include <testsuite.h>

#include <stdlib.h>
#include <string.h>


#define STORE_FILE_NAME  ",,t-stream-indexing-store.db"



/* The stream indexers.  */
static const chop_class_t *indexer_classes[] =
  {
    &chop_tree_indexer_class,
    NULL
  };
static const char *indexer_serials[] =
  {
    "12",
    NULL,
  };

/* The block indexers.  */
static const chop_class_t *block_indexer_classes[] =
  {
    &chop_hash_block_indexer_class,
    &chop_chk_block_indexer_class,
    &chop_integer_block_indexer_class,
#ifdef HAVE_LIBUUID
    &chop_uuid_block_indexer_class,
#endif
    NULL
  };
static const char *block_indexer_serials[] =
  {
    "rmd160",
    "blowfish,cbc,sha1,sha1",
    "123",
#ifdef HAVE_LIBUUID
    "",
#endif
    NULL
  };


/* The choppers.  */
static const chop_chopper_class_t *chopper_classes[] =
  {
    &chop_fixed_size_chopper_class,
    &chop_anchor_based_chopper_class,
    &chop_whole_stream_chopper_class,
    NULL
  };


/* Initialize the on-dist block store and set STORE accordingly.  */
static int
initialize_store (const chop_file_based_store_class_t *store_class,
		  chop_block_store_t *store)
{
  errcode_t err;

  unlink (STORE_FILE_NAME);
  err = chop_file_based_store_open (store_class, STORE_FILE_NAME,
				    O_RDWR | O_CREAT,
				    S_IRUSR | S_IWUSR,
				    store);
  test_check_errcode (err, "opening block store");

  return (err ? -1 : 0);
}

/* Test whole-stream indexing and fetching with the given configuration.  */
static int
test_configuration (chop_indexer_t *indexer, chop_block_indexer_t *bi,
		    chop_chopper_t *chopper,
		    const char *ref_buffer, size_t ref_len)
{
#define CLASS_NAME(_o)							\
  (chop_class_name (chop_object_get_class ((chop_object_t *)(_o))))

  errcode_t err;
  const chop_file_based_store_class_t *store_class;
  chop_block_store_t *store;
  chop_index_handle_t *index;
  chop_block_fetcher_t *fetcher;
  chop_stream_t *stream;

  test_stage ("`%s', `%s', `%s'",
	      CLASS_NAME (indexer), CLASS_NAME (bi), CLASS_NAME (chopper));

  /* Get the store.  */
#ifdef HAVE_TDB
  store_class = &chop_tdb_block_store_class;
#else
  store_class = &chop_gdbm_block_store_class;
#endif

  store = chop_class_alloca_instance ((chop_class_t *)store_class);
  if (initialize_store (store_class, store))
    return -1;

  if (test_debug_mode ())
    {
      chop_block_store_t *debugger;

      debugger = chop_class_alloca_instance (&chop_dummy_block_store_class);
      chop_dummy_proxy_block_store_open ("store", store, debugger);
      chop_log_attach (chop_dummy_block_store_log (debugger), 2, 0);
      store = debugger;
    }

  test_stage_intermediate ("indexing");
  index = chop_block_indexer_alloca_index_handle (bi);
  err = chop_indexer_index_blocks (indexer, chopper, bi,
				   store, store,
				   index);
  test_check_errcode (err, "indexing stream");

  test_stage_intermediate ("fetching");
  fetcher = chop_block_indexer_alloca_fetcher (bi);
  err = chop_block_indexer_initialize_fetcher (bi, fetcher);
  test_check_errcode (err, "initializing block fetcher");

  stream = chop_indexer_alloca_stream (indexer);
  err = chop_indexer_fetch_stream (indexer, index, fetcher, store, store,
				   stream);
  test_check_errcode (err, "fetching stream");

  {
    char buffer[1777];
    size_t total_read = 0, read = 0;
    const char *ref_pos = ref_buffer;

    while (total_read < ref_len)
      {
	err = chop_stream_read (stream, buffer, sizeof (buffer), &read);
	if (err == CHOP_STREAM_END)
	  break;

	test_check_errcode (err, "reading from stream");
	test_assert (read <= sizeof (buffer));
	test_assert (!memcmp (ref_pos, buffer, read));

	ref_pos += read;
	total_read += read;
      }

    test_assert (total_read == ref_len);
  }

  chop_object_destroy ((chop_object_t *)store);
  chop_object_destroy ((chop_object_t *)stream);
  chop_object_destroy ((chop_object_t *)fetcher);
  chop_object_destroy ((chop_object_t *)index);

  test_stage_result (1);

  return 0;
#undef CLASS_NAME
}


int
main (int argc, char *argv[])
{
  static long int random_data[789987];

  errcode_t err;
  unsigned chopper_it, indexer_it, bi_it;
  size_t bytes_read = 0;
  chop_stream_t *stream;
  long int *p;

  test_init (argv[0]);
  test_init_random_seed ();

  for (p = random_data;
       p < random_data + (sizeof (random_data) / sizeof (random_data[0]));
       p++)
    *p = random ();

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  stream = chop_class_alloca_instance (&chop_mem_stream_class);

  for (indexer_it = 0;
       indexer_classes[indexer_it];
       indexer_it++)
    {
      const chop_class_t *indexer_class = indexer_classes[indexer_it];
      const char *indexer_serial = indexer_serials[indexer_it];
      chop_indexer_t *indexer = chop_class_alloca_instance (indexer_class);

      test_debug ("deserializing indexer `%s'",
		  chop_class_name (indexer_class));
      err = chop_object_deserialize ((chop_object_t *)indexer,
				     indexer_class, CHOP_SERIAL_ASCII,
				     indexer_serial,
				     strlen (indexer_serial),
				     &bytes_read);
      test_check_errcode (err, "deserializing indexer");
      test_assert (bytes_read == strlen (indexer_serial));
      test_assert (chop_object_is_a ((chop_object_t *)indexer, indexer_class));

      for (chopper_it = 0;
	   chopper_classes[chopper_it];
	   chopper_it++)
	{
	  const chop_chopper_class_t *chopper_class;
	  chop_chopper_t *chopper;

	  chopper_class = chopper_classes[chopper_it];
	  chopper = chop_class_alloca_instance ((chop_class_t *)chopper_class);

	  for (bi_it = 0;
	       block_indexer_classes[bi_it];
	       bi_it++)
	    {
	      chop_block_indexer_t *bi;
	      const chop_class_t *bi_class = block_indexer_classes[bi_it];
	      const char *bi_serial = block_indexer_serials[bi_it];

	      chop_mem_stream_open ((char *)random_data,
				    sizeof (random_data),
				    NULL, stream);

	      test_debug ("instantiating chopper `%s'",
			  chop_class_name ((chop_class_t *)chopper_class));
	      err = chop_chopper_generic_open (chopper_class,
					       stream, 0, chopper);
	      test_check_errcode (err, "initializing chopper");

	      test_debug ("deserializing BI `%s'",
			  chop_class_name (bi_class));
	      bi = chop_class_alloca_instance (bi_class);
	      err = chop_object_deserialize ((chop_object_t *)bi,
					     bi_class, CHOP_SERIAL_ASCII,
					     bi_serial, strlen (bi_serial),
					     &bytes_read);
	      test_check_errcode (err, "deserializing block indexer");
	      test_assert (bytes_read == strlen (bi_serial));

	      if (test_configuration (indexer, bi, chopper,
				      (char *)random_data,
				      sizeof (random_data)))
		return -1;

	      chop_object_destroy ((chop_object_t *)chopper);
	      chop_object_destroy ((chop_object_t *)stream);
	      chop_object_destroy ((chop_object_t *)bi);
	    }
	}

      chop_object_destroy ((chop_object_t *)indexer);
    }

  return 0;
}

/* arch-tag: bbeb7863-f72b-45b1-bca7-5a4e9cbe298c
 */
