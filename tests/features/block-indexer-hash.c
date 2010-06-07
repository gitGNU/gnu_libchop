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

/* Verify the `hash_block_fetcher' integrity checks.  The test makes sure the
   fetcher notices altered data blocks upon retrieval.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/block-indexers.h>

#include <testsuite.h>


static void
test_index_and_fetch (chop_block_store_t *store)
{
  static char data[7777];

  chop_error_t err;
  chop_block_indexer_t *indexer;
  chop_block_fetcher_t *fetcher;
  chop_index_handle_t *handle;
  char hash[chop_hash_size (CHOP_HASH_MD5)];
  chop_block_key_t key;
  chop_buffer_t buffer;
  size_t size;

  test_randomize_input (data, sizeof (data));

  /* Prepare.  */
  indexer = chop_class_alloca_instance (&chop_hash_block_indexer_class);

  err = chop_hash_block_indexer_open (CHOP_HASH_MD5, indexer);
  test_check_errcode (err, "opening hash block indexer");

  fetcher = chop_block_indexer_alloca_fetcher (indexer);
  err = chop_block_indexer_initialize_fetcher (indexer, fetcher);
  test_check_errcode (err, "initializing hash block fetcher");

  handle = chop_block_indexer_alloca_index_handle (indexer);

  /* Index.  */
  test_stage ("indexing");
  err = chop_block_indexer_index (indexer, store, data, sizeof (data),
				  handle);
  test_check_errcode (err, "indexing block");
  test_stage_result (1);

  /* Fetch.  */
  test_stage ("fetching unaltered block");
  err = chop_buffer_init (&buffer, sizeof (data));
  test_check_errcode (err, "initializing buffer");

  err = chop_block_fetcher_fetch (fetcher, handle, store, &buffer, &size);
  test_check_errcode (err, "unaltered block fetch");

  /* Compare.  */
  test_stage_intermediate ("comparing");
  test_assert (size == sizeof (data));
  test_assert (size == chop_buffer_size (&buffer));
  test_assert (!memcmp (data, chop_buffer_content (&buffer), size));
  test_stage_result (1);

  chop_buffer_clear (&buffer);

  /* Alter.  */
  test_stage ("integrity checks");
  test_stage_intermediate ("tampering with block");
  chop_hash_buffer (CHOP_HASH_MD5, data, sizeof (data), hash);
  chop_block_key_init (&key, hash, chop_hash_size (CHOP_HASH_MD5), NULL, NULL);

  data[0] = data[0] + 1;
  err = chop_store_write_block (store, &key, data, sizeof (data));
  test_check_errcode (err, "writing altered block");

  /* Fetch and make sure the alteration doesn't go unnoticed.  */
  test_stage_intermediate ("fetching altered block");
  err = chop_block_fetcher_fetch (fetcher, handle, store, &buffer, &size);
  test_assert (err != 0);
  test_assert (size == 0);
  test_stage_result (1);

  chop_buffer_return (&buffer);
  chop_object_destroy ((chop_object_t *) fetcher);
  chop_object_destroy ((chop_object_t *) indexer);
}


int
main (int argc, char *argv[])
{
  static const char db_file[] = ",,block-indexer-hash.db";

  chop_error_t err;
  chop_block_store_t *store;

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  remove (db_file);

  store =
    chop_class_alloca_instance ((chop_class_t *) &chop_gdbm_block_store_class);
  err = chop_gdbm_store_open (db_file, 0, O_RDWR | O_CREAT,
			      S_IRUSR | S_IWUSR, NULL, store);
  test_check_errcode (err, "opening block store");

  test_index_and_fetch (store);

  chop_object_destroy ((chop_object_t *) store);

  remove (db_file);

  return EXIT_SUCCESS;
}
