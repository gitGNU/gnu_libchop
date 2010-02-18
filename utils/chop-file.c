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

/* An example of how to use libchop.  Given a number of fixed parameters,
   this programs cuts a file into blocks and stores them into a block
   store (i.e. a block database).  */

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
  chop_error_t err;
  chop_stream_t *stream;
  chop_block_store_t *store, *metastore;
  chop_chopper_t *chopper;
  chop_block_indexer_t *block_indexer;
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

  block_indexer = chop_class_alloca_instance (&chop_hash_block_indexer_class);
  err = chop_hash_block_indexer_open (CHOP_HASH_SHA1, block_indexer);
  if (err)
    {
      com_err (argv[0], err, "failed to open hash block indexer");
      exit (2);
    }

  indexer = chop_class_alloca_instance (&chop_tree_indexer_class);
  err = chop_tree_indexer_open (12 /* indices per block */, indexer);
  if (err)
    {
      com_err (argv[0], err, "failed to open tree indexer");
      exit (2);
    }

  store = (chop_block_store_t *)
    chop_class_alloca_instance (&chop_dummy_block_store_class);
  metastore = (chop_block_store_t *)
    chop_class_alloca_instance (&chop_dummy_block_store_class);

  chop_dummy_block_store_open ("data", store);
  chop_dummy_block_store_open ("meta", metastore);

  handle = chop_block_indexer_alloca_index_handle (block_indexer);
  err = chop_indexer_index_blocks (indexer, chopper, block_indexer,
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

