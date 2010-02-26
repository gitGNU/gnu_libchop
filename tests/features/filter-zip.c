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

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/filters.h>

#include <testsuite.h>

#include <stdio.h>
#include <assert.h>


#define SIZE_OF_INPUT  1779773
static char input[SIZE_OF_INPUT];
static size_t input_offset = 0;



/* Handle input faults for FILTER (actually the zip filter) and provide it
   with random data taken from INPUT.  */
static chop_error_t
handle_random_input_fault (chop_filter_t *filter,
			   size_t amount, void *data)
{
  chop_error_t err;
  size_t available, pushed = 0;

  if (input_offset >= SIZE_OF_INPUT)
    return CHOP_STREAM_END;

  test_debug ("serving input fault for the `%s' (%zu bytes)",
	      chop_class_name (chop_object_get_class ((chop_object_t *)filter)),
	      amount);
  available = SIZE_OF_INPUT - input_offset;
  amount = (amount > available) ? available : amount;

  err = chop_filter_push (filter, input + input_offset, amount, &pushed);
  input_offset += pushed;

  return err;
}


/* Information passed to `handle_zipped_input_fault ()'.  */
typedef struct
{
  chop_filter_t *zip_filter;
  int            flushing;
} zipped_input_fault_handler_data_t;

/* Handle input faults for UNZIP_FILTER, i.e. feed it with data from the zip
   filter.  */
static chop_error_t
handle_zipped_input_fault (chop_filter_t *unzip_filter,
			   size_t amount, void *data)
{
  chop_error_t err;
  char *buffer;
  size_t pulled, pushed;
  zipped_input_fault_handler_data_t *zdata;

  zdata = (zipped_input_fault_handler_data_t *) data;

  /* Obviously, ZIP_FILTER is supposed to be, well, a zip filter.  */
  test_assert (
	  chop_object_is_a ((chop_object_t *) zdata->zip_filter,
			    (chop_class_t *) &chop_zlib_zip_filter_class)
#ifdef HAVE_LIBBZ2
	  ||
	  chop_object_is_a ((chop_object_t *) zdata->zip_filter,
			    (chop_class_t *) &chop_bzip2_zip_filter_class)
#endif
#ifdef HAVE_LZO
	  ||
	  chop_object_is_a ((chop_object_t *) zdata->zip_filter,
			    (chop_class_t *) &chop_lzo_zip_filter_class)
#endif
	  );

  test_debug ("handling input fault for the `%s' (%zu bytes)",
	      chop_class_name (chop_object_get_class
			       ((chop_object_t *) unzip_filter)),
	      amount);

  buffer = alloca (amount);
  err = chop_filter_pull (zdata->zip_filter, zdata->flushing,
			  buffer, amount, &pulled);
  if (err)
    {
      if (err == CHOP_STREAM_END)
	{
	  /* Next time, we'll start flushing ZIP_FILTER.  */
	  test_debug ("%s: got end-of-stream", __FUNCTION__);
	  zdata->flushing = 1;
	  err = 0;
	}
      else
	return err;
    }

  if (pulled)
    err = chop_filter_push (unzip_filter, buffer, pulled, &pushed);

  return err;
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

  chop_error_t err;
  const zip_implementation_t *implementation;

  test_init (argv[0]);
  test_init_random_seed ();

  /* Initialize libchop, create one zip filter and one unzip filter.  */
  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  for (implementation = &implementations[0];
       implementation->zip_class != NULL;
       implementation++)
    {
      /* The output buffer is made bigger as if we didn't know how many bytes
	 we'll be able to pull.  */
      char output[SIZE_OF_INPUT + 100];
      size_t output_size = 0;
      size_t pulled = 0;
      chop_filter_t *zip_filter, *unzip_filter;
      chop_log_t *zip_log, *unzip_log;
      zipped_input_fault_handler_data_t zifh_data;

      zip_filter =
	chop_class_alloca_instance ((chop_class_t *) implementation->zip_class);
      unzip_filter =
	chop_class_alloca_instance ((chop_class_t *) implementation->unzip_class);

      err = chop_zip_filter_generic_open (implementation->zip_class,
					  CHOP_ZIP_FILTER_DEFAULT_COMPRESSION,
					  0, zip_filter);
      test_check_errcode (err, "initializing zlib zip filter");

      err = chop_unzip_filter_generic_open (implementation->unzip_class,
					    0, unzip_filter);
      test_check_errcode (err, "initializing zlib unzip filter");

      if (test_debug_mode ())
	{
	  /* Attach filters' logs to stderr (for debugging).  */
	  zip_log = chop_filter_log (zip_filter);
	  unzip_log = chop_filter_log (unzip_filter);
	  chop_log_attach (zip_log, 2, 0);
	  chop_log_attach (unzip_log, 2, 0);
	}

      /* Feed the zip filter with random input.  */
      input_offset = 0;
      chop_filter_set_input_fault_handler (zip_filter,
					   handle_random_input_fault, NULL);

      /* Feed the unzip filter with ZIP_FILTER's output.  */
      zifh_data.zip_filter = zip_filter;
      zifh_data.flushing = 0;
      chop_filter_set_input_fault_handler (unzip_filter,
					   handle_zipped_input_fault,
					   &zifh_data);

      /* Randomize the input (which hasn't been read yet).  */
      test_randomize_input (input, sizeof (input));

      test_stage ("pull from the `%s' filter",
		  chop_class_name ((chop_class_t *)
				   implementation->unzip_class));

      /* Pull data from UNZIP_FILTER until an end-of-stream error is
	 caught.  */
      test_stage_intermediate ("initial data");
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
		   filter which has been called by HANDLE_ZIPPED_INPUT_FAULT
		   with FLUSH set to 1, and actually finished compressing its
		   input. */
		break;

	      chop_error (err, "while pulling data");
	      return 2;
	    }
	}

      /* Flush the remaining data from UNZIP_FILTER, i.e. without pulling new
	 data.  */
      test_stage_intermediate ("flush");
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
	  chop_error (err, "while flushing filter's input");
	  exit (3);
	}


      /* We're done.  */
      test_debug ("input size was: %i; output size was: %zu",
		  SIZE_OF_INPUT, output_size);

      test_assert (output_size == SIZE_OF_INPUT);
      test_assert (!memcmp (input, output, SIZE_OF_INPUT));

      chop_object_destroy ((chop_object_t *) zip_filter);
      chop_object_destroy ((chop_object_t *) unzip_filter);

      test_stage_result (1);
    }

  return 0;
}
