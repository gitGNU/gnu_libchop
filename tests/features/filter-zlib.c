#include <chop/chop.h>
#include <chop/filters.h>

#include <stdio.h>
#include <assert.h>

#define SIZE_OF_INPUT  2777
static char input[SIZE_OF_INPUT];
static size_t input_offset = 0;

static errcode_t
handle_random_input_fault (chop_filter_t *filter,
			   size_t amount, void *data)
{
  errcode_t err;
  size_t available;

  if (input_offset >= SIZE_OF_INPUT)
    return CHOP_STREAM_END;

  fprintf (stderr, "serving input fault for %u bytes\n", amount);
  available = SIZE_OF_INPUT - input_offset;
  amount = (amount > available) ? available : amount;

  err = chop_filter_push (filter, input + input_offset, amount);
  input_offset += amount;

  return err;
}

/* Handle input faults for UNZIP_FILTER, i.e. feed it with data from the zip
   filter.  */
static errcode_t
handle_zipped_input_fault (chop_filter_t *unzip_filter,
			   size_t amount, void *data)
{
  errcode_t err;
  char *buffer;
  size_t pulled;
  static int flushing = 0;
  chop_filter_t *zip_filter = (chop_filter_t *)data;

  /* Obviously, ZIP_FILTER is supposed to be, well, a zip filter.  */
  assert (chop_object_is_a ((chop_object_t *)zip_filter,
			    &chop_zlib_zip_filter_class));

  fprintf (stderr, "handling input fault for the unzip filter (%u bytes)\n",
	   amount);

  buffer = alloca (amount);
  err = chop_filter_pull (zip_filter, flushing,
			  buffer, amount, &pulled);
  if (err)
    {
      if (err == CHOP_STREAM_END)
	{
	  /* Next time, we'll start flushing ZIP_FILTER.  */
	  flushing = 1;
	  err = 0;
	}
      else
	return err;
    }

  if (pulled)
    err = chop_filter_push (unzip_filter, buffer, pulled);

  return err;
}


int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_filter_t *zip_filter, *unzip_filter;
  chop_log_t *zip_log, *unzip_log;
  char output[SIZE_OF_INPUT];
  size_t output_size = 0;
  size_t pulled = 0;

  err = chop_init ();
  if (err)
    {
      com_err (argv[0], err, "while initializing libchop");
      return 1;
    }

  zip_filter = chop_class_alloca_instance (&chop_zlib_zip_filter_class);
  unzip_filter = chop_class_alloca_instance (&chop_zlib_unzip_filter_class);
  err = chop_zlib_zip_filter_init (-1, 0, zip_filter);
  if (err)
    return 1;

  err = chop_zlib_unzip_filter_init (0, unzip_filter);
  if (err)
    return 2;

  /* Attach filters' logs to stderr.  */
  zip_log = chop_filter_log (zip_filter);
  unzip_log = chop_filter_log (unzip_filter);
  chop_log_attach (zip_log, 2, 0);
  chop_log_attach (unzip_log, 2, 0);

  /* Feed the zip filter with random input.  */
  chop_filter_set_input_fault_handler (zip_filter,
				       handle_random_input_fault, NULL);

  /* Feed the unzip filter with ZIP_FILTER's output.  */
  chop_filter_set_input_fault_handler (unzip_filter,
				       handle_zipped_input_fault,
				       zip_filter);

  while (!err)
    {
      err = chop_filter_pull (unzip_filter, 0,
			      output + output_size,
			      sizeof (output) - output_size,
			      &pulled);
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
      err = chop_filter_pull (unzip_filter, 1,
			      output + output_size,
			      sizeof (output) - output_size,
			      &pulled);
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

  return 0;
}
