/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2011  Ludovic Courtès <ludo@gnu.org>
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

/* Stack a memory stream, a zip-filtered stream, and an unzip-filtered
   stream, and make sure the output yielded is the same as the input.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/filters.h>

#include <testsuite.h>


/* The input data of the source stream.  */
#define SIZE_OF_INPUT  5123123
static char input[SIZE_OF_INPUT];


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

  static const size_t input_sizes[] =
    { 2, 5, 17, 79, 101, SIZE_OF_INPUT, 0 };

  chop_error_t err;
  const size_t *input_size;
  const zip_implementation_t *implementation;

  test_init (argv[0]);
  test_init_random_seed ();

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  test_randomize_input (input, sizeof input);

  for (implementation = &implementations[0];
       implementation->zip_class != NULL;
       implementation++)
    for (input_size = &input_sizes[0];
	 *input_size > 0;
	 input_size++)
      {
	size_t total_read = 0, quarter = 0;
	chop_filter_t *zip_filter, *unzip_filter;
	chop_stream_t *source_stream, *zipped_stream, *unzipped_stream;

	test_stage
	  ("%zi input bytes, stacked `%s'/`%s' filtered streams",
	   *input_size,
	   chop_class_name ((chop_class_t *) implementation->zip_class),
	   chop_class_name ((chop_class_t *) implementation->unzip_class));

	source_stream = chop_class_alloca_instance (&chop_mem_stream_class);
	chop_mem_stream_open (input, *input_size, NULL, source_stream);

	zip_filter = chop_class_alloca_instance ((chop_class_t *)
						 implementation->zip_class);
	err = chop_zip_filter_generic_open (implementation->zip_class,
					    CHOP_ZIP_FILTER_DEFAULT_COMPRESSION,
					    0, zip_filter);
	test_check_errcode (err, "initializing zip filter");

	zipped_stream =
	  chop_class_alloca_instance (&chop_filtered_stream_class);
	err = chop_filtered_stream_open (source_stream,
					 CHOP_PROXY_EVENTUALLY_DESTROY,
					 zip_filter, 1,
					 zipped_stream);
	test_check_errcode (err, "initializing zip-filtered stream");

	unzip_filter =
	  chop_class_alloca_instance ((chop_class_t *)
				      implementation->unzip_class);
	err = chop_unzip_filter_generic_open (implementation->unzip_class,
					      0, unzip_filter);
	test_check_errcode (err, "initializing unzip filter");

	unzipped_stream =
	  chop_class_alloca_instance (&chop_filtered_stream_class);
	err = chop_filtered_stream_open (zipped_stream,
					 CHOP_PROXY_EVENTUALLY_DESTROY,
					 unzip_filter, 1,
					 unzipped_stream);
	test_check_errcode (err, "initializing unzip-filtered stream");

	/* Go ahead: read from UNZIPPED_STREAM and make sure we get the same
	   data as in INPUT.  */
	while (1)
	  {
	    char buffer[4077];
	    size_t read;

	    err = chop_stream_read (unzipped_stream, buffer,
				    sizeof (buffer), &read);
	    if (!err)
	      {
		test_assert (read <= sizeof (buffer));
		test_assert (!memcmp (input + total_read, buffer, read));
		total_read += read;

		if ((total_read > *input_size / 4) && (quarter == 0))
		  {
		    test_stage_intermediate ("25%%");
		    quarter++;
		  }
		else if ((total_read > *input_size / 2) && (quarter == 1))
		  {
		    test_stage_intermediate ("50%%");
		    quarter++;
		  }
		else if ((total_read > (3 * *input_size) / 4)
			 && (quarter == 2))
		  {
		    test_stage_intermediate ("75%%");
		    quarter++;
		  }
	      }

	    if (err)
	      break;
	  }

	test_assert (err == CHOP_STREAM_END);
	test_assert (total_read == *input_size);

	test_stage_result (1);

	/* The magic here is that the following line destroys all the objects
	   we've been using.  */
	chop_object_destroy ((chop_object_t *) unzipped_stream);
      }

  return 0;
}

/* arch-tag: 2d70ca16-2c6c-46a0-81e7-8fe5c51a8274
 */
