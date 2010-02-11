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

/* This test aims at making sure the anchor-based chopper properly detects
   anchors.  The anchor-based chopper is first run on random input and the
   position of each anchor is recorded.  Then, a modified version of this
   random input is made by inserting a relatively small sequence of bytes
   somewhere in this input.  The position of the anchors for this modified
   input sequence is recorded.  Finally, the positions of the anchors for
   both input sequences are compared and those found for the reference input
   sequence are expected to be found in the modified input sequence too.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/choppers.h>

#include <testsuite.h>


/* Parameters of the anchor-based chopper.  */
#define WINDOW_SIZE         48
#define MAGIC_FPR_MASK    2047

/* Since this test operates on random data, it's best to run it several
   times.  This is the number of iterations.  */
#define ITERATION_COUNT      3

/* Parameters of the data sets.  */
#define SIZE_OF_INPUT      1779773
#define SIZE_OF_INSERTION  (MAGIC_FPR_MASK + (MAGIC_FPR_MASK >> 1))


/* The input (reference) buffer.  */
static char input[SIZE_OF_INPUT];

/* Number of blocks yielded by the chopper for INPUT.  */
static size_t input_block_count = 0;

/* Offset of the input block boundaries.  */
static size_t input_block_offsets[SIZE_OF_INPUT / 2];


/* A modified version of INPUT, where SIZE_OF_INSERTION bytes have been
   inserted at INSERTION_OFFSET.  */
static char insertion[SIZE_OF_INPUT + SIZE_OF_INSERTION];

static size_t insertion_offset = 0;

static size_t insertion_block_count = 0;

static size_t insertion_block_offsets[(SIZE_OF_INPUT + SIZE_OF_INSERTION) / 2];



static void
read_blocks_from_chopper (chop_chopper_t *chopper,
			  size_t *offset_vector, size_t offset_vector_size,
			  size_t *block_count)
{
  errcode_t err;
  size_t block_size;
  chop_buffer_t buffer;

  err = chop_buffer_init (&buffer, chop_chopper_typical_block_size (chopper));
  test_check_errcode (err, "allocating buffer");

  offset_vector[0] = 0;
  *block_count = 1;

  do
    {
      err = chop_chopper_read_block (chopper, &buffer, &block_size);
      test_assert ((!err) || (err == CHOP_STREAM_END));
      test_assert (chop_buffer_size (&buffer) == block_size);

      if (!err)
	{
	  test_assert (*block_count < offset_vector_size);
	  offset_vector[*block_count]  = offset_vector[*block_count - 1];
	  offset_vector[*block_count] += block_size;
	  (*block_count)++;
	}
    }
  while (!err);

  chop_buffer_return (&buffer);

  test_debug ("read %u blocks, last offset is %u\n", *block_count,
	      offset_vector[*block_count - 1]);
}

static void
copy_input_with_random_insertion (const char *input, size_t input_size,
				  char *insertion,
				  size_t insertion_size,
				  size_t insertion_offset)
{
  memcpy (insertion, input, insertion_offset);
  insertion += insertion_offset;
  input += insertion_offset;

  test_randomize_input (insertion, insertion_size);
  insertion += insertion_size;

  memcpy (insertion, input, input_size - insertion_offset);
}

static int
compare_block_boundaries (size_t *ref_offsets, size_t ref_block_count,
			  size_t *insertion_offsets, size_t insertion_block_count,
			  size_t insertion_offset, size_t insertion_size,
			  size_t window_size, size_t magic_fpr_mask)
{
  size_t insertion_end_offset;
  size_t ref = 0, mod = 0;

  test_assert ((ref_offsets[0] == 0) && (ref_offsets[0] == insertion_offsets[0]));

  /* In the worst case, the original data can yield one anchor more than the
     modified data.  This can happen if this very anchor is located within
     WINDOW_SIZE bytes after INSERTION_OFFSET.  */
  test_assert (insertion_block_count + 1 >= ref_block_count);

  /* INSERTION_END_OFFSET is the position in the input after which the input
     and modified input should have the same block boundaries.  */
  insertion_end_offset = insertion_offset + insertion_size + window_size;

  while (insertion_offsets[mod] < insertion_offset)
    {
      /* Before the insertion, all block boundaries should be the same.  */
      test_assert (insertion_offsets[mod] == ref_offsets[ref]);
      ref++, mod++;
    }

  /* Skip the offsets that represent the boundaries of block which occur
     within the segment of bytes that were inserted.  */
  while (insertion_offsets[mod] < insertion_end_offset)
    mod++;

  /* The same goes for the reference offsets: the anchors that were found
     within the WINDOW_SIZE bytes after the insertion point must be
     ignored.  */
  while (ref_offsets[ref] < insertion_offset + window_size)
    ref++;

  test_debug ("insertion yielded %u additional blocks\n", mod - ref);
  test_debug ("offsets: mod=%u ref=%u insertion=[%u .. %u]\n",
	      insertion_offsets[mod], ref_offsets[ref],
	      insertion_offset, insertion_end_offset);

  while (ref < ref_block_count)
    {
      test_assert (insertion_offsets[mod] == ref_offsets[ref] + insertion_size);
      ref++, mod++;
    }

  test_assert (mod == insertion_block_count);

  return 1;
}

static int
do_test (void)
{
  errcode_t err;
  int succeeded = 0;
  chop_stream_t *stream;
  chop_chopper_t *chopper;

  test_randomize_input (input, sizeof (input));

  stream = chop_class_alloca_instance (&chop_mem_stream_class);
  chopper =
    chop_class_alloca_instance ((chop_class_t *)&chop_anchor_based_chopper_class);

  test_stage ("the anchor positions");

  /* Read and chop the input stream.  */
  test_stage_intermediate ("reference");
  chop_mem_stream_open (input, sizeof (input), NULL, stream);
  err = chop_anchor_based_chopper_init (stream, WINDOW_SIZE, MAGIC_FPR_MASK,
					chopper);
  test_check_errcode (err, "initializing chopper");

  read_blocks_from_chopper (chopper,
			    input_block_offsets,
			    sizeof (input_block_offsets)
			    / sizeof (input_block_offsets[0]),
			    &input_block_count);
  chop_object_destroy ((chop_object_t *)chopper);
  chop_object_destroy ((chop_object_t *)stream);

  /* Create a modified input stream.  */
  test_stage_intermediate ("insertion");
  insertion_offset = random () % (sizeof (input) / 2);
  copy_input_with_random_insertion (input, sizeof (input),
				    insertion,
				    sizeof (insertion) - sizeof (input),
				    insertion_offset);

  chop_mem_stream_open (insertion, sizeof (insertion), NULL, stream);
  err = chop_anchor_based_chopper_init (stream, WINDOW_SIZE, MAGIC_FPR_MASK,
					chopper);
  test_check_errcode (err, "initializing chopper for insertion");

  read_blocks_from_chopper (chopper,
			    insertion_block_offsets,
			    sizeof (insertion_block_offsets)
			    / sizeof (insertion_block_offsets[0]),
			    &insertion_block_count);
  chop_object_destroy ((chop_object_t *)chopper);
  chop_object_destroy ((chop_object_t *)stream);

  /* Compare the offsets of the block boundaries.  */
  test_stage_intermediate ("compare");
  succeeded =
    compare_block_boundaries (input_block_offsets, input_block_count,
			      insertion_block_offsets, insertion_block_count,
			      insertion_offset,
			      sizeof (insertion) - sizeof (input),
			      WINDOW_SIZE, MAGIC_FPR_MASK);

  test_stage_result (succeeded);

  return succeeded;
}


int
main (int argc, char *argv[])
{
  errcode_t err;
  size_t iterations;
  int succeeded = 0;

  test_init (argv[0]);
  test_init_random_seed ();

  /* Initialize libchop, create one zip filter and one unzip filter.  */
  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  for (iterations = 0; iterations < ITERATION_COUNT; iterations++)
    succeeded = do_test () || succeeded;

  return (succeeded ? 0 : 1);
}

/* arch-tag: 86a0448c-b245-4fc1-a475-57731d54933c
 */
