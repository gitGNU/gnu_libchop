/* Test the compliance of various chopper implementations with the interface
   specifications.  */

#include <chop/chop.h>
#include <chop/choppers.h>

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <time.h>

/* #define DEBUG 1 */

int
main (int argc, char *argv[])
{
  static const chop_chopper_class_t *classes[] =
    {
      &chop_fixed_size_chopper_class,
      &chop_anchor_based_chopper_class,
      NULL
    };
  static char mem_stream_contents[1000007];
  char *mem;
  const chop_chopper_class_t **class;
  chop_stream_t *input;
  chop_chopper_t *chopper;
  chop_buffer_t buffer;
  errcode_t err;
  struct timeval tv;

  gettimeofday (&tv, NULL);
  srandom (tv.tv_sec);
  for (mem = mem_stream_contents;
       mem - mem_stream_contents < sizeof (mem_stream_contents);
       mem++)
    {
/*       *mem = (random () % 64) + '!'; */
      *mem = random () % 255;
    }

  chop_buffer_init (&buffer, 0);
  input = chop_class_alloca_instance (&chop_mem_stream_class);

  for (class = classes;
       *class != NULL;
       class++)
    {
      size_t bytes_read = 0;

      fprintf (stdout, "Testing chopper class `%s'...\n",
	       chop_class_name ((chop_class_t *)*class));

      /* Open the input stream.  */
      chop_mem_stream_open (mem_stream_contents, sizeof (mem_stream_contents),
			    NULL, input);

      /* Open the chopper.  */
      chopper = chop_class_alloca_instance ((chop_class_t *)*class);
      err = chop_chopper_generic_open (*class, input, chopper);
      if (err)
	{
	  com_err (argv[0], err, "while initializing `%s' chopper",
		   chop_class_name ((chop_class_t *)*class));
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
		  assert (amount == chop_buffer_size (&buffer));
		  assert (!memcmp (mem_stream_contents + bytes_read,
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
	}

      if (err != CHOP_STREAM_END)
	{
	  com_err (argv[0], err, "while reading block from a `%s' chopper",
		   chop_class_name ((chop_class_t *)*class));
	  exit (3);
	}

      chop_chopper_close (chopper);
      chop_stream_close (input);

      if (bytes_read != sizeof (mem_stream_contents))
	{
	  fprintf (stderr, "%s: `%s' chopper gave %u bytes instead of %u\n",
		   argv[0], chop_class_name ((chop_class_t *)*class),
		   bytes_read, sizeof (mem_stream_contents));
	  exit (4);
	}
    }

  return 0;
}
