#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/store-hash-tree.h>

#include <stdio.h>


int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_file_stream_t stream;
  chop_fixed_size_chopper_t chopper;
  chop_hash_tree_block_store_t store;
  chop_buffer_t buffer;
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

  err = chop_hash_tree_block_store_open (NULL, CHOP_HASH_SHA1 /* unused */,
					 &store);
  if (err)
    {
      com_err (argv[0], err, "failed to open tree-hash store");
      exit (2);
    }

  err = chop_buffer_init (&buffer, block_size);
  if (err)
    exit (3);

  while (1)
    {
      char key_buffer[20];
      chop_block_key_t key;
      static size_t block_num = 0;
      size_t amount;
      err = chop_chopper_read_block ((chop_chopper_t *)&chopper,
				     &buffer, &amount);
      if (err == CHOP_STREAM_END)
	{
	  fprintf (stderr, "end of stream: %u blocks (%u B each)\n",
		   block_num, block_size);
	  break;
	}

      if (err)
	{
	  com_err (argv[0], err, "while reading block %u", block_num);
	  exit (4);
	}

      if (amount != block_size)
	{
	  fprintf (stderr, "wrong block size (%u vs. %u)\n",
		   amount, block_size);
	  exit (5);
	}

      /* Create a dummy key for this block */
      chop_block_key_init (&key, key_buffer, sizeof (key_buffer), NULL, NULL);

      err = chop_store_write_block ((chop_block_store_t *)&store,
				    &key,
				    chop_buffer_content (&buffer),
				    chop_buffer_size (&buffer));
      if (err)
	{
	  com_err (argv[0], err, "while writing block %u", block_num);
	  exit (6);
	}

      block_num++;
    }

  err = chop_store_close ((chop_block_store_t *)&store);
  if (err)
    {
      com_err (argv[0], err, "while closing output block store");
      exit (7);
    }

  return 0;
}

