/* libchop -- a utility library for distributed storage
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

#ifndef CHOP_FILTERS_H
#define CHOP_FILTERS_H

/* Filters, as in GStreamer's `GstElement', `GstBin' and `GstPipeline',
   `GstPad' also.
   http://gstreamer.freedesktop.org/data/doc/gstreamer/head/gstreamer/html/GstElement.html
*/

#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/logs.h>


/* A few examples of filters: rsync, zlib/bzlib, ciphering.  */

struct chop_filter;

/* A filter fault handler is a function that gets called whenever an input or
   output fault occurs within a filter.  I.e. a filter will call its input
   (resp. output) handler whenever it does not have sufficient input
   (resp. output) data to handle a pull (resp. push) request.  Note that
   cascading faults are avoided by means of the WITHIN_FAULT_HANDLER
   attributes of `chop_filter_t' objects.  */
typedef struct
{
  chop_error_t (* handle) (struct chop_filter *, size_t, void *);
  void *data;
} chop_filter_fault_handler_t;

/* Declare `chop_filter_t' which inherits from `chop_object_t' and
   CHOP_FILTER_CLASS.  */
CHOP_DECLARE_RT_CLASS (filter, object,
		       chop_log_t log;

		       chop_filter_fault_handler_t input_fault_handler;
		       chop_filter_fault_handler_t output_fault_handler;
		       int within_fault_handler;

		       chop_error_t (* push) (struct chop_filter *,
					      const char *, size_t, size_t *);
		       chop_error_t (* pull) (struct chop_filter *, int,
					      char *, size_t, size_t *););




/* Return the log object of FILTER.  */
static __inline__ chop_log_t *chop_filter_log (chop_filter_t *__filter)
{
  return (&__filter->log);
}

/* Push SIZE bytes from BUFFER into FILTER's input.  This may trigger an
   output fault in FILTER.  CHOP_FILTER_FULL is returned if the output fault
   could not be handled and FILTER's input is consequently still full, and
   not a single byte was pushed in.  PUSHED is set to the number of bytes
   actually pushed.  Any other error returned by the fault handler may be
   returned by this function.  */
static __inline__ chop_error_t
chop_filter_push (chop_filter_t *__filter,
		  const char *__buffer, size_t __size, size_t *__pushed)
{
  return (__filter->push (__filter, __buffer, __size, __pushed));
}

/* Pull at most SIZE bytes from FILTER into BUFFER.  The number of bytes
   actually pulled is returned in PULLED.  If FLUSH is zero, this may trigger
   an input fault in FILTER and CHOP_FILTER_EMPTY may be returned is FILTER
   is lacking input data.  If FLUSH is set, then all pending input data will
   be flushed (processed); CHOP_FILTER_EMPTY is returned when all input data
   has been processed, otherwise zero is returned and the function must be
   called again with FLUSH set.  Any other error returned by the fault
   handler may be returned by this function.  */
static __inline__ chop_error_t
chop_filter_pull (chop_filter_t *__filter, int __flush,
		  char *__buffer, size_t __size, size_t *__pulled)
{
  return (__filter->pull (__filter, __flush, __buffer, __size, __pulled));
}


static __inline__ chop_filter_fault_handler_t
chop_filter_input_fault_handler (const chop_filter_t *__filter)
{
  return (__filter->input_fault_handler);
}

static __inline__ chop_filter_fault_handler_t
chop_filter_output_fault_handler (const chop_filter_t *__filter)
{
  return (__filter->output_fault_handler);
}

static __inline__ void
chop_filter_set_input_fault_handler (chop_filter_t *__filter,
				     chop_error_t (* __h) (chop_filter_t *,
							   size_t, void *),
				     void *__hdata)
{
  __filter->input_fault_handler.handle = __h;
  __filter->input_fault_handler.data = __hdata;
}

static __inline__ void
chop_filter_set_output_fault_handler (chop_filter_t *__filter,
				      chop_error_t (* __h) (chop_filter_t *,
							    size_t, void *),
				      void *__hdata)
{
  __filter->output_fault_handler.handle = __h;
  __filter->output_fault_handler.data = __hdata;
}

/* This function should only be used internally by filters.  Handle input
   fault for FILTER by providing it with (preferrably) AMOUNT bytes (via its
   `push' method).  This function may return CHOP_FILTER_UNHANDLED_FAULT if
   no input fault handler was defined or if FILTER was already handling an
   output (presumably) fault.  */
static __inline__ chop_error_t
chop_filter_handle_input_fault (chop_filter_t *__filter,
				size_t __amount)
{
  chop_error_t __err;
  void *__hdata = __filter->input_fault_handler.data;

  if ((!__filter->input_fault_handler.handle)
      || (__filter->within_fault_handler))
    return CHOP_FILTER_UNHANDLED_FAULT;

  __filter->within_fault_handler = 1;
  __err = __filter->input_fault_handler.handle (__filter, __amount, __hdata);
  __filter->within_fault_handler = 0;

  return __err;
}

/* This function should only be used internally by filters.  Handle output
   fault for FILTER by providing it with (preferrably) AMOUNT bytes (via its
   `push' method).  This function may return CHOP_FILTER_UNHANDLED_FAULT if
   no output fault handler was defined or if FILTER was already handling an
   input (presumably) fault.  */
static __inline__ chop_error_t
chop_filter_handle_output_fault (chop_filter_t *__filter,
				 size_t __amount)
{
  chop_error_t __err;
  void *__hdata = __filter->output_fault_handler.data;

  if ((!__filter->output_fault_handler.handle)
      || (__filter->within_fault_handler))
    return CHOP_FILTER_UNHANDLED_FAULT;

  __filter->within_fault_handler = 1;
  __err = __filter->output_fault_handler.handle (__filter, __amount, __hdata);
  __filter->within_fault_handler = 0;

  return __err;
}


/* Example input fault handlers.  */

/* Change FILTER's input fault handler so that if fetches its input data from
   INPUT which contains INPUT_SIZE bytes.  */
extern chop_error_t
chop_filter_set_input_from_buffer (chop_filter_t *filter,
				   const char *input, size_t input_size);

/* Free resources that were allocated with FILTER when
   CHOP_FILTER_SET_INPUT_FROM_BUFFER was called, and store in BYTES_READ the
   number of bytes that were read from the input buffer.  Additionally,
   restore the input fault handler that was in place before.  */
extern void
chop_filter_finish_input_from_buffer (chop_filter_t *filter,
				      size_t *bytes_read);

/* Filter INPUT (of INPUT_SIZE bytes) through FILTER and store the result in
   OUTPUT.  This function may temporarily modify FILTER's fault handlers.  */
extern chop_error_t chop_filter_through (chop_filter_t *filter,
				      const char *input, size_t input_size,
				      chop_buffer_t *output);


/* The `chop_{zip,unzip}_filter_class_t' metaclasses which provide a generic
   zip/unzip filter creation method (a "factory").  */

CHOP_DECLARE_RT_CLASS (zip_filter_class, class,
		       chop_error_t (* generic_open) (int, size_t,
						      chop_filter_t *););


CHOP_DECLARE_RT_CLASS (unzip_filter_class, class,
		       chop_error_t (* generic_open) (size_t, chop_filter_t *););


/* The default zip filter compression level.  */
#define CHOP_ZIP_FILTER_DEFAULT_COMPRESSION (-1)

/* Initialize FILTER as an instance of KLASS, using an input buffer of size
   INPUT_SIZE.  COMPRESSION_LEVEL should be either
   `CHOP_ZIP_FILTER_DEFAULT_COMPRESSION', in which case the implementation
   will choose some default compression level, or an integer between 0 and 9
   inclusive.  */
static __inline__ chop_error_t
chop_zip_filter_generic_open (const chop_zip_filter_class_t *klass,
			      int compression_level, size_t input_size,
			      chop_filter_t *filter)
{
  chop_error_t err;

  if (CHOP_EXPECT_TRUE (klass->generic_open != NULL))
    err = klass->generic_open (compression_level, input_size, filter);
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

/* Initialize FILTER as an instance of KLASS, using an input buffer of size
   INPUT_SIZE.  */
static __inline__ chop_error_t
chop_unzip_filter_generic_open (const chop_unzip_filter_class_t *klass,
				size_t input_size,
				chop_filter_t *filter)
{
  chop_error_t err;

  if (CHOP_EXPECT_TRUE (klass->generic_open != NULL))
    err = klass->generic_open (input_size, filter);
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}



/* The zlib-based compressing and uncompressing filter classes.  */

extern const chop_zip_filter_class_t   chop_zlib_zip_filter_class;
extern const chop_unzip_filter_class_t chop_zlib_unzip_filter_class;

/* Initialize the zlib-based compression filter with compression level
   ZLIB_COMPRESSION_LEVEL (an integer between 0 and 9) with an input buffer
   of INPUT_SIZE bytes.  If ZLIB_COMPRESSION_LEVEL is -1, then zlib's default
   compression level is used.  If INPUT_SIZE is zero, then a default size is
   used.  */
extern chop_error_t
chop_zlib_zip_filter_init (int zlib_compression_level, size_t input_size,
			   chop_filter_t *filter);

/* Initialize the zlib-based decompressiong filter with an input buffer of
   INPUT_SIZE bytes.  If INPUT_SIZE is zero, then a default size is used.  */
extern chop_error_t
chop_zlib_unzip_filter_init (size_t input_size,
			     chop_filter_t *filter);



/* The (optional) bzip2-based compressing and uncompressing filter
   classes.  */

extern const chop_zip_filter_class_t   chop_bzip2_zip_filter_class;
extern const chop_unzip_filter_class_t chop_bzip2_unzip_filter_class;

/* Initialize the bzip2-based compression filter using BLOCK_COUNT_100K
   blocks for compression internally (the higher, the better), and using
   WORK_FACTOR to determine how compression behaves when presented the worst
   case repetitive input (see the `libbzip2' manual for details).  The
   returned filter will internally use a buffer of INPUT_SIZE bytes.  */
extern chop_error_t
chop_bzip2_zip_filter_init (size_t block_count_100k, size_t work_factor,
			    size_t input_size, chop_filter_t *filter);

/* Initialize the bzip2-based decompressiong filter with an input buffer of
   INPUT_SIZE bytes.  If INPUT_SIZE is zero, then a default size is used.  If
   SMALL is non-zero, then `libbzip2' will use an alternate decompression
   algorithm that is slower but uses less memory.  */
extern chop_error_t
chop_bzip2_unzip_filter_init (int small, size_t input_size,
			      chop_filter_t *filter);



/* The (optional) LZO-based compression and decompression filters.  LZO is
   much faster than zlib and bzip2, at the cost of lower compression
   rates.  */

extern const chop_zip_filter_class_t   chop_lzo_zip_filter_class;
extern const chop_unzip_filter_class_t chop_lzo_unzip_filter_class;


/* Initialize FILTER as an LZO compression filter, using a input buffer of
   INPUT_SIZE bytes.  In practice, since LZO is state-less, this means that
   input data will be compressed by blocks of INPUT_SIZE bytes.  Thus, it is
   necessary to use a reasonably large buffer size to obtain reasonable
   compression.  If INPUT_SIZE is zero, a reasonable default is used.  The
   compression algorithm that is used is LZO1X.  */
extern chop_error_t chop_lzo_zip_filter_init (size_t input_size,
					      chop_filter_t *filter);

/* Initialize FILTER as an LZO decompression buffer, using INPUT_SIZE as the
   default input buffer size.  In practice, the input buffer will be grown as
   needed anyway.  */
extern chop_error_t chop_lzo_unzip_filter_init (size_t input_size,
						chop_filter_t *filter);

#endif
