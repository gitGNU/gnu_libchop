/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2011, 2013  Ludovic Courtès <ludo@gnu.org>
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

#include <chop/chop-config.h>

#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>
#include <assert.h>

#include <arpa/inet.h>
#include <stdlib.h>  /* For Gnulib's `malloc ()' */

#include <lzo/lzo1x.h>


/* Define `chop_lzo_unzip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (lzo_unzip_filter, filter,
				      unzip_filter_class,

				      lzo_voidp  work_mem;
				      lzo_bytep  input_buffer;
				      size_t     input_buffer_size;
				      size_t     avail_in;
				      size_t     input_offset;
				      lzo_bytep  output_buffer;
				      size_t     output_buffer_size;
				      size_t     avail_out;
				      size_t     output_offset;);


/* LZO global initialization.  */
extern chop_error_t chop_initialize_lzo (void);



static chop_error_t
chop_lzo_unzip_pull (chop_filter_t *filter, int flush,
		     char *buffer, size_t size, size_t *pulled)
{
  chop_error_t err = 0;
  chop_lzo_unzip_filter_t *zfilter;

#ifndef STRINGIFY
# define _STRINGIFY(_x) # _x
# define STRINGIFY(_z) _STRINGIFY(_z)
#endif

#define ENSURE_LARGE_ENOUGH_BUFFER(_which, _size)			\
  if (zfilter-> _which ## _buffer_size					\
      < (_size) + zfilter-> _which ## _offset)				\
    {									\
      lzo_bytep new_buf;						\
      size_t new_size = zfilter-> _which ## _buffer_size;		\
									\
      do								\
	{								\
	  new_size <<= 1;						\
	}								\
      while (new_size < (_size) + zfilter-> _which ## _offset);		\
									\
      chop_log_printf (&filter->log,					\
		       STRINGIFY (_which)				\
		       " buffer is too small (%zu bytes "		\
		       "but %u needed), growing to %zu bytes",		\
		       zfilter-> _which ## _buffer_size,		\
		       (_size), new_size);				\
      new_buf =								\
	(lzo_bytep) chop_realloc (zfilter-> _which ## _buffer,		\
				  new_size,				\
				  (chop_class_t *)			\
				  &chop_lzo_unzip_filter_class);	\
      if (!new_buf)							\
	{								\
	  err = ENOMEM;							\
	  break;							\
	}								\
									\
      zfilter-> _which ## _buffer = new_buf;				\
      zfilter-> _which ## _buffer_size = new_size;			\
    }

  zfilter = (chop_lzo_unzip_filter_t *) filter;

  chop_log_printf (&filter->log, "pulling %zu bytes, flush=%s",
		   size, (flush ? "yes" : "no"));

  *pulled = 0;
  while ((*pulled < size) && (err == 0))
    {
      if (zfilter->avail_out > 0)
	{
	  /* Push already decompressed data.  */
	  size_t amount;

	  amount = (zfilter->avail_out > size) ? size : zfilter->avail_out;
	  memcpy (buffer, zfilter->output_buffer + zfilter->output_offset,
		  amount);
	  *pulled                += amount;
	  buffer                 += amount;
	  size                   -= amount;
	  zfilter->output_offset += amount;
	  zfilter->avail_out     -= amount;
	  if (zfilter->avail_out == 0)
	    zfilter->output_offset = 0;
	}
      else if (flush)
	{
	  /* Don't try to decompress more data, exit.  */
	  if (*pulled == 0)
	    err = CHOP_FILTER_EMPTY;
	  else
	    break;
	}
      else if (zfilter->avail_in < 8)
	{
	  /* Get at least enough input data to decode IN32 and OUT32.  */
	  err = chop_filter_handle_input_fault (filter, 8);
	  if (err)
	    chop_log_printf (&filter->log,
			     "input fault unhandled: %s",
			     chop_error_message (err));
	}
      else
	{
	  /* Decompress data to FILTER's output buffer.  */

	  /* Fetch the size of the compressed block so that we can eventually
	     proceed with decompression.  */
	  unsigned in32, out32;

	  memcpy (&in32,  zfilter->input_buffer, 4);
	  memcpy (&out32, zfilter->input_buffer + 4, 4);
	  in32  = ntohl (in32);
	  out32 = ntohl (out32);

	  zfilter->input_offset += 8;
	  zfilter->avail_in     -= 8;
	  size                  -= 8;

	  /* Grow the buffers as needed.  Hopefully, buffers should only need
	     to be grown once since we expect the input to use fixed-size
	     input buffers.  */
	  ENSURE_LARGE_ENOUGH_BUFFER (input, in32);
	  ENSURE_LARGE_ENOUGH_BUFFER (output, out32);

	  if (in32 > zfilter->avail_in)
	    {
	      /* Fetch the (compressed) input data.  */
	      chop_log_printf (&filter->log,
			       "input fault: requesting %zu bytes of "
			       "compressed data",
			       in32 - zfilter->avail_in);
	      err = chop_filter_handle_input_fault (filter,
						    in32 - zfilter->avail_in);
	    }

	  if (err)
	    {
	      chop_log_printf (&filter->log,
			       "unexpected input fault unhandled: %s",
			       chop_error_message (err));
	      err = CHOP_FILTER_ERROR;
	    }
	  else if (zfilter->avail_in < in32)
	    {
	      /* Failed to get enough input data.  */
	      chop_log_printf (&filter->log,
			       "not enough input data ("
			       "%zu bytes available)",
			       zfilter->avail_in);
	      err = CHOP_FILTER_ERROR;
	    }
	  else
	    {
	      /* Actually decompress.  */
	      chop_log_printf (&filter->log,
			       "pull: processing stream (input: %u@%zu, "
			       "output: %u, flush: %s)",
			       in32, zfilter->input_offset, out32,
			       flush ? "yes" : "no");

	      zfilter->avail_out = zfilter->output_buffer_size;
	      err = lzo1x_decompress_safe (zfilter->input_buffer
					   + zfilter->input_offset, in32,
					   zfilter->output_buffer,
					   &zfilter->avail_out,
					   zfilter->work_mem);
	      if (err != LZO_E_OK)
		{
		  chop_log_printf (&filter->log,
				   "pull: decompression failed (%i)",
				   (int) err);
		  err = CHOP_FILTER_ERROR;
		}
	      else
		{
		  assert (zfilter->avail_out == out32);

		  err = 0;
		  zfilter->avail_in = 0;
		  zfilter->output_offset = zfilter->input_offset = 0;
		}
	    }
	}
    }

  if (err == CHOP_FILTER_UNHANDLED_FAULT)
    {
      /* Only return `CHOP_FILTER_EMPTY' when not a single byte was
	 pulled.  */
      if (*pulled == 0)
	err = CHOP_FILTER_EMPTY;
      else
	err = 0;
    }

  return err;

#undef ENSURE_LARGE_ENOUGH_BUFFER
#undef STRINGIFY
}



static chop_error_t lzo_unzip_filter_ctor (chop_object_t *object,
					   const chop_class_t *class);
static void lzo_unzip_filter_dtor (chop_object_t *object);


chop_error_t
chop_lzo_unzip_filter_init (size_t input_size, chop_filter_t *filter);


static chop_error_t
luf_open (size_t input_size, chop_filter_t *filter)
{
  return (chop_lzo_unzip_filter_init (input_size, filter));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (lzo_unzip_filter, filter,
				     unzip_filter_class, /* Metaclass */

				     /* Metaclass inits.  */
				     .generic_open = luf_open,

				     lzo_unzip_filter_ctor,
				     lzo_unzip_filter_dtor,
				     NULL, NULL, /* No copy, equalp */
				     NULL, NULL  /* No serial, deserial */);

chop_error_t
chop_lzo_unzip_filter_init (size_t input_size, chop_filter_t *filter)
{
  chop_error_t err;
  chop_lzo_unzip_filter_t *zfilter;

  zfilter = (chop_lzo_unzip_filter_t *) filter;

  err = chop_initialize_lzo ();
  if (err)
    return err;

  err =
    chop_object_initialize ((chop_object_t *) filter,
			    (chop_class_t *) &chop_lzo_unzip_filter_class);
  if (err)
    return err;

  input_size = input_size ? input_size : 1024;
  zfilter->input_buffer =
    (lzo_bytep) chop_malloc (input_size,
			     (chop_class_t *) &chop_lzo_unzip_filter_class);
  if (!zfilter->input_buffer)
    goto mem_err;

  zfilter->input_buffer_size = input_size;

  /* We may eventually grow the output buffer if needed.  */
  zfilter->output_buffer_size = input_size << 1;
  zfilter->output_buffer =
    (lzo_bytep) chop_malloc (zfilter->output_buffer_size,
			     (chop_class_t *) &chop_lzo_unzip_filter_class);
  if (!zfilter->output_buffer)
    goto mem_err;

  zfilter->work_mem =
    (lzo_voidp) chop_malloc (LZO1X_1_MEM_COMPRESS,
			     (chop_class_t *) &chop_lzo_unzip_filter_class);
  if (!zfilter->work_mem)
    goto mem_err;

  return 0;

 mem_err:
  chop_object_destroy ((chop_object_t *) zfilter);
  return ENOMEM;
}


/* The push and pull methods.  */
#define ZIP_DIRECTION   unzip

#include "filter-lzo-common.c"

/* arch-tag: 5e03b018-1cf3-4a52-8765-4ac2803770f0

 */
