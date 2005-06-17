#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/indexers.h>

#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>



int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_stream_t *stream;
  chop_block_store_t *store, *metastore;
  chop_chopper_t *chopper;
  chop_indexer_t *indexer;
  chop_buffer_t buffer;
  chop_index_handle_t *handle = NULL;
  size_t block_size;

  if (argc < 2)
    return 1;

  chop_init ();

  stream = chop_class_alloca_instance (&chop_file_stream_class);
  err = chop_file_stream_open (argv[1], stream);
  if (err)
    {
      com_err (argv[0], err, "while opening %s", argv[1]);
      exit (1);
    }

  block_size = chop_stream_preferred_block_size ((chop_stream_t *)&stream);
  chopper = chop_class_alloca_instance ((chop_class_t *)
					&chop_fixed_size_chopper_class);
  err = chop_fixed_size_chopper_init ((chop_stream_t *)&stream,
				      block_size, 0 /* Don't pad blocks */,
				      chopper);
  if (err)
    {
      com_err (argv[0], err, "while initializing chopper");
      exit (2);
    }

  indexer = chop_class_alloca_instance (&chop_hash_tree_indexer_class);
  err = chop_hash_tree_indexer_open (CHOP_HASH_NONE, CHOP_HASH_SHA1,
				     CHOP_CIPHER_HANDLE_NIL,
				     12,
				     indexer);
  if (err)
    {
      com_err (argv[0], err, "failed to open tree-hash indexer");
      exit (2);
    }

  store = (chop_block_store_t *)
    chop_class_alloca_instance (&chop_dummy_block_store_class);
  metastore = (chop_block_store_t *)
    chop_class_alloca_instance (&chop_dummy_block_store_class);

  chop_dummy_block_store_open ("data", store);
  chop_dummy_block_store_open ("meta", metastore);

  handle = chop_indexer_alloca_index_handle (indexer);
  err = chop_indexer_index_blocks (indexer, chopper,
				   store, metastore, handle);
  if ((err) && (err != CHOP_STREAM_END))
    {
      com_err (argv[0], err, "while indexing blocks");
      exit (7);
    }

  err = chop_store_close (store);
  if (err)
    {
      com_err (argv[0], err, "while closing output block store");
      exit (7);
    }

  err = chop_store_close (metastore);
  if (err)
    {
      com_err (argv[0], err, "while closing output meta-data block store");
      exit (7);
    }

  /* Take a look at the index handle we got */
  fprintf (stdout, "chop: done with indexing\n");
  fprintf (stdout, "chop: got a handle of class \"%s\"\n",
	   chop_class_name (chop_object_get_class ((chop_object_t *)handle)));

  err = chop_buffer_init (&buffer, 400);
  if (err)
    exit (12);

  err = chop_object_serialize ((chop_object_t *)handle, CHOP_SERIAL_ASCII,
			       &buffer);
  if (err)
    {
      com_err (argv[0], err, "while serializing index handle");
      exit (8);
    }

  /* Display the ASCII representation of HANDLE.  We assume that it is
     zero-terminated.  */
  fprintf (stdout, "chop: handle: %s\n", chop_buffer_content (&buffer));

  chop_buffer_return (&buffer);
  chop_object_destroy ((chop_object_t *)handle);

  return 0;
}

