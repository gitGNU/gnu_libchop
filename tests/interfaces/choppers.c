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

/* Test the compliance of various chopper implementations with the interface
   specifications.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/choppers.h>

#include <testsuite.h>

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>


/* #define DEBUG 1 */

#define CHOPPER_BLOCK_SIZE  (4565)

int
main (int argc, char *argv[])
{
  static const chop_chopper_class_t *classes[] =
    {
      &chop_fixed_size_chopper_class,
      &chop_whole_stream_chopper_class,
      &chop_anchor_based_chopper_class,
      NULL
    };
  static char mem_stream_contents[1000777];
  char *mem;
  const chop_chopper_class_t **class;
  chop_stream_t *input;
  chop_chopper_t *chopper;
  chop_buffer_t buffer;
  chop_error_t err;

  test_init (argv[0]);
  test_init_random_seed ();

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  for (mem = mem_stream_contents;
       mem - mem_stream_contents < sizeof (mem_stream_contents);
       mem++)
    {
      *mem = random () % 256;
    }

  chop_buffer_init (&buffer, 0);
  input = chop_class_alloca_instance (&chop_mem_stream_class);

  for (class = classes;
       *class != NULL;
       class++)
    {
      size_t bytes_read = 0, input_size;

      input_size = sizeof (mem_stream_contents) - (random () % 60);
      test_stage ("chopper class `%s', %zu input bytes",
		  chop_class_name ((chop_class_t *)*class),
		  input_size);

      /* Open the input stream.  */
      chop_mem_stream_open (mem_stream_contents, input_size,
			    NULL, input);

      /* Open the chopper.  */
      chopper = chop_class_alloca_instance ((chop_class_t *)*class);
      err = chop_chopper_generic_open (*class, input,
				       CHOPPER_BLOCK_SIZE, chopper);
      if (err)
	{
	  chop_error (err, "while initializing `%s' chopper",
		      chop_class_name ((chop_class_t *) *class));
	  exit (1);
	}

#ifdef DEBUG
      if (*class == &chop_anchor_based_chopper_class)
	{
	  chop_log_t *log = chop_anchor_based_chopper_log (chopper);
	  chop_log_attach (log, 2, 0);
	}
#endif

      while (!err)
	{
	  size_t amount = 0;

	  err = chop_chopper_read_block (chopper, &buffer, &amount);
	  if (!err)
	    {
	      if (amount > 0)
		{
		  /* Verify that CHOPPER complies with the interface
		     specifications.  */
		  test_assert (amount == chop_buffer_size (&buffer));
		  test_assert (!memcmp (mem_stream_contents + bytes_read,
					chop_buffer_content (&buffer),
					amount));
		  bytes_read += amount;
		}
	      else
		{
		  fprintf (stderr, "chopper of class `%s' returned no data\n",
			   chop_class_name ((chop_class_t *)*class));
		  exit (2);
		}
	    }
	  else
	    /* When the chopper fails and returns an error, it should left
	       AMOUNT untouched or make it zero.  */
	    test_assert (amount == 0);
	}

      if (err != CHOP_STREAM_END)
	{
	  chop_error (err, "while reading block from a `%s' chopper",
		      chop_class_name ((chop_class_t *) *class));
	  exit (3);
	}

      chop_stream_close (input);

      if (bytes_read != input_size)
	{
	  fprintf (stderr, "%s: `%s' chopper gave %zu bytes instead of %u\n",
		   argv[0], chop_class_name ((chop_class_t *)*class),
		   bytes_read, input_size);
	  exit (4);
	}

      chop_object_destroy ((chop_object_t *) chopper);
      chop_object_destroy ((chop_object_t *) input);

      test_stage_result (1);
    }

  return 0;
}
