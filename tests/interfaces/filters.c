/* This test basically ensures that filters honor the input fault mechanism.
   It does not attempt to check whether the filter's output corresponds to
   its input.  */

#include <chop/chop.h>
#include <chop/filters.h>

#include <testsuite.h>

#include <stdio.h>
#include <assert.h>

#define SIZE_OF_INPUT  27777
static char input[SIZE_OF_INPUT];
static char zipped_input[SIZE_OF_INPUT];
static char unzipped_output[SIZE_OF_INPUT];



static int
test_filter (chop_filter_t *filter,
	     const char *input, size_t input_size,
	     char *output, size_t *output_size)
{
  errcode_t err = 0;
  size_t bytes_read = 0;
  int flush = 0;
  const chop_class_t *filter_class;

  filter_class = chop_object_get_class ((chop_object_t *)filter);
  test_stage ("input filter `%s'", chop_class_name (filter_class));

  if (test_debug_mode ())
    {
      /* Attach FILTER's log to stderr.  */
      chop_log_t *log;

      log = chop_filter_log (filter);
      chop_log_attach (log, 2, 0);
    }

  chop_filter_set_input_from_buffer (filter, input, input_size);
  *output_size = 0;

  /* Copied from `chop_filter_through ()'.  */
  while (1)
    {
      char block[837];
      size_t pulled = 0;

      err = chop_filter_pull (filter, flush,
			      block, sizeof (block), &pulled);
      if (err)
	{
	  test_assert (pulled == 0);
	  if (err == CHOP_FILTER_EMPTY)
	    {
	      if (!flush)
		flush = 1;
	      else
		{
		  /* Normal termination.  */
		  err = 0;
		  break;
		}
	    }
	  else
	    break;
	}
      else
	{
	  test_assert (pulled <= sizeof (block));
	  memcpy (output + *output_size, block, pulled);
	  *output_size += pulled;
	}
    }

  test_check_errcode (err, "pulling data from filter");

  test_debug ("input size was: %u; output size was: %u",
	      input_size, *output_size);

  chop_filter_finish_input_from_buffer (filter, &bytes_read);

  /* Make sure everything was read.  */
  test_assert (bytes_read == input_size);

  test_stage_result (1);

  return 1;
}


/* Non-nominal test case.  */

static errcode_t
badly_handle_input_fault (chop_filter_t *filter, size_t how_much, void *data)
{
  /* Raise a non-filter related exception.  This may happen, for instance,
     when a filtered-stream is layed on top of an indexer retrieval
     stream.  */
  return CHOP_INDEXER_ERROR;
}

static int
test_filter_non_nominal (chop_filter_t *filter)
{
  errcode_t err;
  char buf[1234];
  size_t pulled;
  const chop_class_t *filter_class;

  filter_class = chop_object_get_class ((chop_object_t *)filter);
  test_stage ("input filter `%s' (non-nominal)",
	      chop_class_name (filter_class));

  chop_filter_set_input_fault_handler (filter, badly_handle_input_fault,
				       NULL);

  err = chop_filter_pull (filter, 0, buf, sizeof (buf), &pulled);

  /* FILTER should not hide the exception raised by its input fault
     handler.  */
  test_stage_result (err == CHOP_INDEXER_ERROR);

  return (err == CHOP_INDEXER_ERROR);
}


int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_filter_t *zip_filter, *unzip_filter;
  size_t zipped_size = 0, unzipped_size = 0;

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  zip_filter = chop_class_alloca_instance (&chop_zlib_zip_filter_class);
  err = chop_zlib_zip_filter_init (-1, 0, zip_filter);
  test_check_errcode (err, "initializing zlib zip filter");

  if (!test_filter (zip_filter, input, sizeof (input),
		    zipped_input, &zipped_size))
    return 1;

  /* Now, test an unzip filter.  */
  unzip_filter = chop_class_alloca_instance (&chop_zlib_unzip_filter_class);
  err = chop_zlib_unzip_filter_init (0, unzip_filter);
  test_check_errcode (err, "initializing zlib unzip filter");

  if (!test_filter (unzip_filter, zipped_input, zipped_size,
		    unzipped_output, &unzipped_size))
    return 1;

  /* The following assertions are specific to the zip/unzip filters.  */
  test_assert (unzipped_size == sizeof (input));
  test_assert (!memcmp (unzipped_output, input, sizeof (input)));

  chop_object_destroy ((chop_object_t *)zip_filter);
  chop_object_destroy ((chop_object_t *)unzip_filter);

  /* Test the non-nominal situation.  */
  err = chop_zlib_unzip_filter_init (0, unzip_filter);
  test_check_errcode (err, "initializing zlib unzip filter");
  err = chop_zlib_zip_filter_init (-1, 0, zip_filter);
  test_check_errcode (err, "initializing zlib zip filter");

  if (!test_filter_non_nominal (zip_filter))
    return -1;
  if (!test_filter_non_nominal (unzip_filter))
    return -1;

  chop_object_destroy ((chop_object_t *)zip_filter);
  chop_object_destroy ((chop_object_t *)unzip_filter);

  return 0;
}
