/* This test is aimed at testing the zip/unzip filters.  It creates an zip
   filter whose input faults are "resolved" by pushing it some random data,
   and an unzip filter whose input faults are resolved by pushing it data
   from the zip filter.  Data is pulled from the unzip filter and we
   eventually make sure this produces the same data as the one that was given
   to the zip filter.


   Further details
   ---------------

   Pulling data from the unzip filter (which sits at the end of the filter
   chain) results in cascading input faults: since the unzip filter is empty
   when the program starts, it raises an "input fault" which is handled by
   pulling data from the zip filter; since the zip filter is empty too, it
   raises an input fault which is handled by pushing random data into it.  */

#include <chop/chop.h>
#include <chop/filters.h>

#include <stdio.h>
#include <assert.h>

#define SIZE_OF_INPUT  27773
static char input[SIZE_OF_INPUT];
static size_t input_offset = 0;


/* Write SIZE poorly random bytes into buffer.  */
static void
randomize_input (char *buffer, size_t size)
{
  char *p;

  for (p = buffer; p < buffer + size; p++)
    *p = random ();
}

/* Handle input faults for FILTER (actually the zip filter) and provide it
   with random data taken from INPUT.  */
static errcode_t
handle_random_input_fault (chop_filter_t *filter,
			   size_t amount, void *data)
{
  errcode_t err;
  size_t available;

  if (input_offset >= SIZE_OF_INPUT)
    return CHOP_STREAM_END;

  fprintf (stderr, "serving input fault for the `%s' (%u bytes)\n",
	   chop_class_name (chop_object_get_class ((chop_object_t *)filter)),
	   amount);
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

  fprintf (stderr, "handling input fault for the `%s' (%u bytes)\n",
	   chop_class_name (chop_object_get_class ((chop_object_t *)unzip_filter)),
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

  /* Initialize libchop, create one zip filter and one unzip filter.  */
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

#if 0
  /* Attach filters' logs to stderr (for debugging).  */
  zip_log = chop_filter_log (zip_filter);
  unzip_log = chop_filter_log (unzip_filter);
  chop_log_attach (zip_log, 2, 0);
  chop_log_attach (unzip_log, 2, 0);
#endif

  /* Feed the zip filter with random input.  */
  chop_filter_set_input_fault_handler (zip_filter,
				       handle_random_input_fault, NULL);

  /* Feed the unzip filter with ZIP_FILTER's output.  */
  chop_filter_set_input_fault_handler (unzip_filter,
				       handle_zipped_input_fault,
				       zip_filter);

  /* Randomize the input (which hasn't been read yet).  */
  randomize_input (input, sizeof (input));

  /* Pull data from UNZIP_FILTER until an end-of-stream error is caught.  */
  while (!err)
    {
      err = chop_filter_pull (unzip_filter, 0 /* don't flush */,
			      output + output_size,
			      sizeof (output) - output_size,
			      &pulled);
      output_size += pulled;
      if (err)
	{
	  if (err == CHOP_FILTER_EMPTY)
	    /* This CHOP_FILTER_EMPTY error actually comes from the zip
	       filter which has been called by HANDLE_ZIPPED_INPUT_FAULT with
	       FLUSH set to 1, and actually finished compressing its input. */
	    break;

	  com_err (argv[0], err, "while pulling data");
	  return 2;
	}
    }

  /* Flush the remaining data from UNZIP_FILTER, i.e. without pulling new
     data.  */
  do
    {
      pulled = 0;
      err = chop_filter_pull (unzip_filter, 1 /* start flushing! */,
			      output + output_size,
			      sizeof (output) - output_size,
			      &pulled);
      output_size += pulled;
    }
  while ((output_size < sizeof (output)) && (!err));

  if ((err) && (err != CHOP_FILTER_EMPTY))
    {
      com_err (argv[0], err, "while flushing filter's input");
      exit (3);
    }


  /* We're done.  */
  fprintf (stdout, "input size was: %u; output size was: %u\n",
	   SIZE_OF_INPUT, output_size);

  assert (output_size == SIZE_OF_INPUT);
  assert (!memcmp (input, output, SIZE_OF_INPUT));

  return 0;
}
