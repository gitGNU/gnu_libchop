/* This test basically ensures that filters honor the input fault mechanism.
   It does not attempt to check whether the filter's output corresponds to
   its input.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/filters.h>

#include <testsuite.h>

#include <stdio.h>


/* Iterations for each filter, to test reusability of filters.  */
#define ITERATIONS_PER_FILTER 15

#define SIZE_OF_INPUT  27777
static char input[SIZE_OF_INPUT];

/* Given that we randomize INPUT, the zipped data may actually be slightly
   larger than the input data, hence the need for a larger buffer.  Likewise,
   unzipping could fail and produce more data than needed, hence the larger
   buffer.  */
static char zipped_input[SIZE_OF_INPUT + SIZE_OF_INPUT/2];
static char unzipped_output[SIZE_OF_INPUT + SIZE_OF_INPUT/3];



/* Write SIZE poorly random bytes into buffer.  */
static void
randomize_input (char *buffer, size_t size)
{
  char *p;

  for (p = buffer; p < buffer + size; p++)
    *p = random ();
}


static int
test_filter (chop_filter_t *filter,
	     const char *input, size_t input_size,
	     char *output, size_t *output_size)
{
  errcode_t err = 0;
  size_t bytes_read = 0, prev_bytes_read = 0;
  unsigned int iteration = 0;
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

  /* Run the test several times for each filter to make sure that filters can
     be re-used.  */
  for (iteration = 0; iteration < ITERATIONS_PER_FILTER; iteration++)
    {
      int flush = 0;

      test_debug ("starting iteration %u", iteration);

      chop_filter_set_input_from_buffer (filter, input, input_size);
      *output_size = 0;
      prev_bytes_read = bytes_read;

      /* Copied from `chop_filter_through ()'.  */
      while (1)
	{
	  char block[837];
	  size_t pulled = 0;

	  err = chop_filter_pull (filter, flush,
				  block, sizeof (block), &pulled);
	  if (err)
	    {
	      if (err == CHOP_FILTER_EMPTY)
		{
		  test_assert (pulled == 0);
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

      /* Make sure the filter behaves deterministically when it's reused.  */
      if (prev_bytes_read)
	test_assert (bytes_read == prev_bytes_read);
    }

  test_stage_result (1);

  return 1;
}


/* Non-nominal test case.  */

static errcode_t
badly_handle_input_fault (chop_filter_t *filter, size_t how_much, void *data)
{
  /* Raise a non-filter related exception.  This may happen, for instance,
     when a filtered-stream is layered on top of an indexer retrieval
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


/* Characterization of zip/unzip filter implementations.  */

typedef struct
{
  const chop_zip_filter_class_t   *zip_class;
  const chop_unzip_filter_class_t *unzip_class;
} zip_implementation_t;



int
main (int argc, char *argv[])
{
  static const zip_implementation_t implementations[] =
    {
      { &chop_zlib_zip_filter_class,
	&chop_zlib_unzip_filter_class },
#ifdef HAVE_LIBBZ2
      { &chop_bzip2_zip_filter_class,
	&chop_bzip2_unzip_filter_class },
#endif
#ifdef HAVE_LZO
      { &chop_lzo_zip_filter_class,
	&chop_lzo_unzip_filter_class },
#endif
      { NULL, NULL }
    };

  errcode_t err;
  int succeeded = 1;
  const zip_implementation_t *implementation;

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  for (implementation = &implementations[0];
       implementation->zip_class != NULL;
       implementation++)
    {
      chop_filter_t *zip_filter = NULL, *unzip_filter = NULL;
      size_t zipped_size = 0, unzipped_size = 0;

      zip_filter =
	chop_class_alloca_instance ((chop_class_t *) implementation->zip_class);
      err = chop_zip_filter_generic_open (implementation->zip_class,
					  CHOP_ZIP_FILTER_DEFAULT_COMPRESSION,
					  0, zip_filter);
      test_check_errcode (err, "initializing zip filter");

      randomize_input (input, sizeof (input));
      if (!test_filter (zip_filter, input, sizeof (input),
			zipped_input, &zipped_size))
	{
	  /* We failed zipping.  Let's skip to the next implementation.  */
	  succeeded = 0;
	  goto finish_test;
	}

      /* Now, test an unzip filter.  */
      unzip_filter =
	chop_class_alloca_instance ((chop_class_t *) implementation->unzip_class);
      err = chop_unzip_filter_generic_open (implementation->unzip_class,
					    0, unzip_filter);
      test_check_errcode (err, "initializing unzip filter");

      if (!test_filter (unzip_filter, zipped_input, zipped_size,
			unzipped_output, &unzipped_size))
	succeeded = 0;
      else
	{
	  /* The following assertions are specific to the zip/unzip
	     filters.  */
	  test_assert (unzipped_size == sizeof (input));
	  test_assert (!memcmp (unzipped_output, input, sizeof (input)));

	  chop_object_destroy ((chop_object_t *) zip_filter);
	  chop_object_destroy ((chop_object_t *) unzip_filter);

	  /* Test the non-nominal situation.  */
	  err = chop_unzip_filter_generic_open (implementation->unzip_class,
						0, unzip_filter);
	  test_check_errcode (err, "initializing unzip filter");
	  err = chop_zip_filter_generic_open (implementation->zip_class,
					      CHOP_ZIP_FILTER_DEFAULT_COMPRESSION,
					      0, zip_filter);
	  test_check_errcode (err, "initializing zip filter");

	  if (!test_filter_non_nominal (zip_filter))
	    succeeded = 0;
	  if (!test_filter_non_nominal (unzip_filter))
	    succeeded = 0;
	}

    finish_test:
      chop_object_destroy ((chop_object_t *) zip_filter);
      chop_object_destroy ((chop_object_t *) unzip_filter);

    }

  return (succeeded == 0);
}
