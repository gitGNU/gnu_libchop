/* This test basically ensures that filters honor the input fault mechanism.
   It does not attempt to check whether the filter's output corresponds to
   its input.  */

#include <chop/chop.h>
#include <chop/filters.h>

#include <stdio.h>
#include <assert.h>

#define SIZE_OF_INPUT  2777
static char input[SIZE_OF_INPUT];
static size_t input_offset = 0;

static errcode_t
handle_input_fault (chop_filter_t *filter, size_t amount, void *data)
{
  errcode_t err;
  size_t available, pushed = 0;

  if (input_offset >= SIZE_OF_INPUT)
    return CHOP_STREAM_END;

  fprintf (stderr, "serving input fault for the `%s' (%u bytes)\n",
	   chop_class_name (chop_object_get_class ((chop_object_t *)filter)),
	   amount);
  available = SIZE_OF_INPUT - input_offset;
  amount = (amount > available) ? available : amount;

  err = chop_filter_push (filter, input + input_offset, amount, &pushed);
  input_offset += pushed;

  return err;
}


int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_filter_t *filter;
  chop_log_t *log;
  char output[123];
  size_t output_size = 0;
  size_t pulled = 0;

  err = chop_init ();
  if (err)
    {
      com_err (argv[0], err, "while initializing libchop");
      return 1;
    }

  filter = chop_class_alloca_instance (&chop_zlib_zip_filter_class);
  err = chop_zlib_zip_filter_init (-1, 0, filter);
  if (err)
    return 1;

  /* Attach FILTER's log to stderr.  */
  log = chop_filter_log (filter);
  chop_log_attach (log, 2, 0);

  chop_filter_set_input_fault_handler (filter, handle_input_fault, NULL);

  while (!err)
    {
      err = chop_filter_pull (filter, 0,
			      output, sizeof (output), &pulled);
      output_size += pulled;
      if (err)
	{
	  if (err == CHOP_STREAM_END)
	    break;

	  com_err (argv[0], err, "while pulling data");
	  return 2;
	}
    }

  do
    {
      pulled = 0;
      err = chop_filter_pull (filter, 1, output, sizeof (output), &pulled);
      output_size += pulled;
    }
  while (!err);

  if (err != CHOP_FILTER_EMPTY)
    {
      com_err (argv[0], err, "while flushing filter's input");
      exit (3);
    }

  fprintf (stdout, "input size was: %u; output size was: %u\n",
	   SIZE_OF_INPUT, output_size);

  /* Make sure everything was read.  */
  assert (input_offset == SIZE_OF_INPUT);

  return 0;
}
