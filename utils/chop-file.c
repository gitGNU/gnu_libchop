#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/indexer-hash-tree.h>

#include <stdio.h>


int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_file_stream_t stream;
  chop_dummy_block_store_t store;
  chop_fixed_size_chopper_t chopper;
  chop_hash_tree_indexer_t indexer;
  chop_buffer_t buffer;
  chop_index_handle_t *handle = NULL;
  size_t block_size;

  if (argc < 2)
    return 1;

  chop_init ();

  err = chop_file_stream_open (argv[1], &stream);
  if (err)
    {
      com_err (argv[0], err, "while opening %s", argv[1]);
      exit (1);
    }

  block_size = chop_stream_preferred_block_size ((chop_stream_t *)&stream);
  err = chop_fixed_size_chopper_init ((chop_stream_t *)&stream,
				      block_size, &chopper);
  if (err)
    {
      com_err (argv[0], err, "while initializing chopper");
      exit (2);
    }

  err = chop_hash_tree_indexer_open (CHOP_HASH_NONE, CHOP_HASH_SHA1,
				     &indexer);
  if (err)
    {
      com_err (argv[0], err, "failed to open tree-hash indexer");
      exit (2);
    }

  chop_dummy_block_store_open (&store);

  handle = chop_indexer_alloca_index_handle (&indexer);
  err = chop_indexer_index_blocks ((chop_indexer_t *)&indexer,
				   (chop_chopper_t *)&chopper,
				   (chop_block_store_t *)&store,
				   handle);

  fprintf (stdout, "chop: done with indexing\n");
  fprintf (stdout, "chop: got a handle of class \"%s\"\n",
	   chop_class_name (chop_object_get_class ((chop_object_t *)handle)));
  chop_object_destroy ((chop_object_t *)handle);

  err = chop_store_close ((chop_block_store_t *)&store);
  if (err)
    {
      com_err (argv[0], err, "while closing output block store");
      exit (7);
    }

  return 0;
}

