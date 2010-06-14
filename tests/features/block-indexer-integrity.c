/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2010  Ludovic Courtès <ludo@gnu.org>

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

/* Verify the `hash_block_fetcher' and `chk_block_fetcher' integrity checks.
   The test makes sure the fetcher notices altered data blocks upon
   retrieval.  */

#include <chop/chop-config.h>

#include <alloca.h>
#include <string.h>

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/block-indexers.h>

#include <testsuite.h>


static void
test_index_and_fetch (chop_block_store_t *store,
		      chop_block_indexer_t *indexer)
{
  static char data[7777];

  chop_error_t err;
  chop_block_fetcher_t *fetcher;
  chop_index_handle_t *handle;
  chop_block_iterator_t *iterator;
  chop_buffer_t buffer;
  size_t size;
  char *p;

  test_randomize_input (data, sizeof (data));

  /* Prepare.  */
  fetcher = chop_block_indexer_alloca_fetcher (indexer);
  err = chop_block_indexer_initialize_fetcher (indexer, fetcher);
  test_check_errcode (err, "initializing hash block fetcher");

  if (test_debug_mode ())
    {
      chop_log_t *log;

      log = chop_hash_block_fetcher_log (fetcher);
      log = log ?: chop_chk_block_fetcher_log (fetcher);
      if (log != NULL)
	chop_log_attach (log, 2, 0);
    }

  handle = chop_block_indexer_alloca_index_handle (indexer);

  /* Index.  */
  test_stage_intermediate ("indexing");
  err = chop_block_indexer_index (indexer, store, data, sizeof (data),
				  handle);
  test_check_errcode (err, "indexing block");

  /* Fetch.  */
  test_stage_intermediate ("fetching unaltered block");
  err = chop_buffer_init (&buffer, sizeof (data));
  test_check_errcode (err, "initializing buffer");

  err = chop_block_fetcher_fetch (fetcher, handle, store, &buffer, &size);
  test_check_errcode (err, "unaltered block fetch");

  /* Compare.  */
  test_stage_intermediate ("comparing");
  test_assert (size == sizeof (data));
  test_assert (size == chop_buffer_size (&buffer));
  test_assert (!memcmp (data, chop_buffer_content (&buffer), size));

  chop_buffer_clear (&buffer);

  /* Get the first block written to STORE.  */
  iterator = chop_class_alloca_instance (chop_store_iterator_class (store));
  err = chop_store_first_block (store, iterator);
  test_check_errcode (err, "getting iterator to first block");

  err = chop_store_read_block (store, chop_block_iterator_key (iterator),
			       &buffer, &size);
  test_check_errcode (err, "reading block");

  /* Alter it.  */
  test_stage_intermediate ("tampering with block");
  p = (char *) chop_buffer_content (&buffer);
  p[0] = p[0] + 1;
  err = chop_store_write_block (store, chop_block_iterator_key (iterator),
				chop_buffer_content (&buffer),
				chop_buffer_size (&buffer));
  test_check_errcode (err, "writing altered block");

  /* Fetch and make sure the alteration doesn't go unnoticed.  */
  test_stage_intermediate ("fetching altered block");
  err = chop_block_fetcher_fetch (fetcher, handle, store, &buffer, &size);
  test_assert (err != 0);
  test_assert (size == 0);

  chop_buffer_return (&buffer);
  chop_object_destroy ((chop_object_t *) iterator);
  chop_object_destroy ((chop_object_t *) fetcher);
}


int
main (int argc, char *argv[])
{
  /* The block indexers.  */
  static const chop_class_t *block_indexer_classes[] =
    {
      &chop_hash_block_indexer_class,
      &chop_hash_block_indexer_class,
      &chop_hash_block_indexer_class,
      &chop_hash_block_indexer_class,
      &chop_chk_block_indexer_class,
      &chop_chk_block_indexer_class,
      &chop_chk_block_indexer_class,
      &chop_chk_block_indexer_class,
      NULL
    };

  static const char *block_indexer_serials[] =
    {
      "md5",
      "sha1",
      "rmd160",
      "sha256",
      "blowfish,cbc,sha1,sha1",
      "aes256,cbc,sha256,md4",
      "des,cbc,md4,md5",
      "twofish,cbc,sha256,tiger",
      NULL
    };

  static const char db_file[] = ",,block-indexer-hash.db";

  chop_error_t err;
  chop_block_store_t *store;
  const chop_class_t **indexer_class;
  const char **indexer_serial;

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  store =
    chop_class_alloca_instance ((chop_class_t *) &chop_gdbm_block_store_class);

  for (indexer_class = block_indexer_classes,
	 indexer_serial = block_indexer_serials;
       *indexer_class != NULL;
       indexer_class++, indexer_serial++)
    {
      size_t consumed;
      chop_block_indexer_t *indexer;

      test_stage ("`%s', `%s'", chop_class_name (*indexer_class),
		  *indexer_serial);

      remove (db_file);

      /* Open a fresh block store.  */
      err = chop_gdbm_store_open (db_file, 0, O_RDWR | O_CREAT,
				  S_IRUSR | S_IWUSR, NULL, store);
      test_check_errcode (err, "opening block store");

      if (test_debug_mode ())
	{
	  chop_block_store_t *debugger;

	  debugger = chop_class_alloca_instance (&chop_dummy_block_store_class);
	  chop_dummy_proxy_block_store_open ("store", store, debugger);
	  chop_log_attach (chop_dummy_block_store_log (debugger), 2, 0);
	  store = debugger;
	}

      /* Initialize the block indexer.  */
      indexer = chop_class_alloca_instance (*indexer_class);
      err = chop_object_deserialize ((chop_object_t *) indexer,
				     *indexer_class, CHOP_SERIAL_ASCII,
				     *indexer_serial,
				     strlen (*indexer_serial),
				     &consumed);
      test_check_errcode (err, "deserializing block indexer");
      test_assert (consumed == strlen (*indexer_serial));

      test_index_and_fetch (store, indexer);

      chop_object_destroy ((chop_object_t *) indexer);

      chop_store_close (store);
      chop_object_destroy ((chop_object_t *) store);

      test_stage_result (1);
    }

  remove (db_file);

  return EXIT_SUCCESS;
}
