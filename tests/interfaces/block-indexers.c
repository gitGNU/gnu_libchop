/* arch-tag: c2125401-5b69-4d59-980e-5e60d307de2e
 */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/block-indexers.h>
#include <chop/chop-config.h>

#include <testsuite.h>

#include <string.h>


#define STORE_FILE_NAME  ",,t-block-indexers-store.db"

#define BLOCKS_TO_INDEX 10
#define BLOCK_SIZE      7567



int
main (int argc, char *argv[])
{
  errcode_t err;
  const chop_file_based_store_class_t *store_class;
  chop_block_store_t *store;
  chop_block_indexer_t *block_indexers[40];
  size_t block_indexer_count = 0;
  chop_block_indexer_t **bi_it;
  chop_cipher_handle_t cipher_handle;
  chop_buffer_t buffer;

  static long int random_data[BLOCKS_TO_INDEX][BLOCK_SIZE];

  test_init (argv[0]);
  test_init_random_seed ();

  {
    long int *p;
    unsigned i;

    for (i = 0; i < BLOCKS_TO_INDEX; i++)
      for (p = random_data[i];
	   p < &random_data[i][BLOCK_SIZE];
	   p++)
	*p = random ();
  }

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  /* Open a block store.  */

#ifdef HAVE_TDB
  store_class = &chop_tdb_block_store_class;
#else
  store_class = &chop_gdbm_block_store_class;
#endif

  unlink (STORE_FILE_NAME);
  store = chop_class_alloca_instance ((chop_class_t *)store_class);
  err = chop_file_based_store_open (store_class, STORE_FILE_NAME,
				    O_RDWR | O_CREAT,
				    S_IRUSR | S_IWUSR,
				    store);
  test_check_errcode (err, "opening block store");

  /* Initialize a set of block indexers for testing.  */
  block_indexers[block_indexer_count] =
    chop_class_alloca_instance (&chop_hash_block_indexer_class);
  err = chop_hash_block_indexer_open (CHOP_HASH_SHA1,
				      block_indexers[block_indexer_count]);
  test_check_errcode (err, "opening hash block indexer");
  block_indexer_count++;

  block_indexers[block_indexer_count] =
    chop_class_alloca_instance (&chop_chk_block_indexer_class);
  cipher_handle = chop_cipher_open (CHOP_CIPHER_BLOWFISH,
				    CHOP_CIPHER_MODE_ECB);
  test_assert (cipher_handle != CHOP_CIPHER_HANDLE_NIL);
  err = chop_chk_block_indexer_open (cipher_handle, 1,
				     CHOP_HASH_SHA1, CHOP_HASH_SHA1,
				     block_indexers[block_indexer_count]);
  test_check_errcode (err, "opening CHK block indexer");
  block_indexer_count++;

#ifdef HAVE_LIBUUID
  block_indexers[block_indexer_count] =
    chop_class_alloca_instance (&chop_uuid_block_indexer_class);
  err = chop_uuid_block_indexer_open (block_indexers[block_indexer_count]);
  test_check_errcode (err, "opening UUID block indexer");
  block_indexer_count++;
#endif

  block_indexers[block_indexer_count] = NULL;

  /* Go! */
  err = chop_buffer_init (&buffer, sizeof (random_data[0]));
  test_check_errcode (err, "initializing buffer");

  for (bi_it = block_indexers;
       *bi_it;
       bi_it++)
    {
      const chop_class_t *class;
      chop_index_handle_t *index[BLOCKS_TO_INDEX];
      chop_block_fetcher_t *fetcher;
      size_t fetched_bytes, i;

      class = chop_object_get_class ((chop_object_t *)*bi_it);
      test_stage ("class `%s'", chop_class_name (class));

      test_stage_intermediate ("indexing");
      for (i = 0; i < BLOCKS_TO_INDEX; i++)
	{
	  index[i] = chop_block_indexer_alloca_index_handle (*bi_it);
	  err = chop_block_indexer_index (*bi_it, store,
					  (char *)random_data[i],
					  sizeof (random_data[i]),
					  index[i]);
	  test_check_errcode (err, "indexing block");
	  test_assert (chop_object_is_a ((chop_object_t *)index[i],
					 &chop_index_handle_class));
	}

      test_stage_intermediate ("fetching");
      fetcher = chop_block_indexer_alloca_fetcher (*bi_it);
      err = chop_block_indexer_initialize_fetcher (*bi_it, fetcher);
      test_check_errcode (err, "initializing block fetcher");

      for (i = 0; i < BLOCKS_TO_INDEX; i++)
	{
	  chop_buffer_clear (&buffer);
	  err = chop_block_fetcher_fetch (fetcher, index[i], store, &buffer,
					  &fetched_bytes);
	  test_check_errcode (err, "fetching block");
	  test_assert (fetched_bytes == sizeof (random_data[i]));
	  test_assert (fetched_bytes == chop_buffer_size (&buffer));
	  test_assert (!memcmp (random_data[i], chop_buffer_content (&buffer),
				sizeof (random_data[i])));

	  chop_object_destroy ((chop_object_t *)index[i]);
	}

      chop_object_destroy ((chop_object_t *)fetcher);
      chop_object_destroy ((chop_object_t *)*bi_it);

      test_stage_result (1);
    }

  chop_object_destroy ((chop_object_t *) store);
  chop_buffer_return (&buffer);

  return 0;
}
