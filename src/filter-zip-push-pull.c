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

/* Template for the push and pull methods for zip/unzip filters.  This file
   expects a number of macros to be defined.  It is designed in such a way
   that it can be reused for both compression/decompression, and both zlib
   and bzlib.  */

#if (!defined ZIP_TYPE) || (!defined ZIP_DIRECTION)
# error "This file is meant to be included in some other source file."
#endif


#ifndef CONCAT5
# define _CONCAT5(_x, _y, _z, _p, _q)  _x ## _y ## _z ## _p ## _q
# define CONCAT5(_a, _b, _c, _d, _e)  _CONCAT5 (_a, _b, _c, _d, _e)
#endif

#define ZIP_PUSH_METHOD  CONCAT5 (chop_, ZIP_TYPE, _, ZIP_DIRECTION, _push)
#define ZIP_PULL_METHOD  CONCAT5 (chop_, ZIP_TYPE, _, ZIP_DIRECTION, _pull)
#define ZIP_FILTER_TYPE  CONCAT5 (chop_, ZIP_TYPE, _, ZIP_DIRECTION, _filter_t)
#define ZIP_FILTER_CLASS CONCAT5 (chop_, ZIP_TYPE, _, ZIP_DIRECTION, _filter_class)

static errcode_t
ZIP_PUSH_METHOD (chop_filter_t *filter,
		 const char *buffer, size_t size, size_t *pushed)
{
  errcode_t err;
  ZIP_FILTER_TYPE *zfilter;

  zfilter = (ZIP_FILTER_TYPE *)filter;

  if (zfilter->zstream.avail_in == 0)
    zfilter->zstream.next_in = (ZIP_BUFFER_TYPE *) zfilter->input_buffer;

  *pushed = 0;
  while (size > 0)
    {
      size_t available, amount;

      if (zfilter->zstream.avail_in >= zfilter->input_buffer_size)
	{
	  chop_log_printf (&filter->log, "filter is full, output fault");
	  err = chop_filter_handle_output_fault (filter,
						 zfilter->input_buffer_size);
	  if (err)
	    {
	      chop_log_printf (&filter->log,
			       "filter-full event unhandled: %s",
			       error_message (err));
	      if ((err != 0) && (err != CHOP_FILTER_UNHANDLED_FAULT))
		return err;

	      /* Only return CHOP_FILTER_FULL is not a single byte was
		 absorbed.  */
	      return ((*pushed == 0) ? CHOP_FILTER_FULL : 0);
	    }

	  continue;
	}

      available = zfilter->input_buffer_size - zfilter->zstream.avail_in;
      amount = (available > size) ? size : available;
      memcpy (zfilter->input_buffer + zfilter->zstream.avail_in,
	      buffer, amount);

      zfilter->zstream.avail_in += amount;
      buffer += amount;
      size -= amount;
      *pushed += amount;
    }

  return 0;
}

static errcode_t
ZIP_PULL_METHOD (chop_filter_t *filter, int flush,
		 char *buffer, size_t size, size_t *pulled)
{
  int zret = ZIP_OK;
  errcode_t err = 0;
  ZIP_FILTER_TYPE *zfilter;

  zfilter = (ZIP_FILTER_TYPE *)filter;

  zfilter->zstream.avail_out = size;
  zfilter->zstream.next_out = (ZIP_BUFFER_TYPE *) buffer;
  *pulled = 0;
  while (*pulled < size)
    {
      if (((zfilter->zstream.avail_in == 0)
	   || ZIP_NEED_MORE_INPUT (&zfilter->zstream, zret))
	  && (!flush))
	{
	  size_t howmuch;

	  howmuch = zfilter->input_buffer_size - zfilter->zstream.avail_in;
	  chop_log_printf (&filter->log,
			   "filter is empty, input fault "
			   "(requesting %u bytes)",
			   howmuch);

	  err = chop_filter_handle_input_fault (filter, howmuch);
	  if (err)
	    {
	      chop_log_printf (&filter->log,
			       "input fault unhandled: %s",
			       error_message (err));
	      if (err != CHOP_FILTER_UNHANDLED_FAULT)
		break;

	      err = CHOP_FILTER_EMPTY;
	      break;
	    }

	  continue;
	}


      chop_log_printf (&filter->log,
		       "pull: processing stream (input: %u, output: %u, "
		       "flush: %s)",
		       zfilter->zstream.avail_in, zfilter->zstream.avail_out,
		       flush ? "yes" : "no");
      zret = ZIP_PROCESS (&zfilter->zstream,
			  flush ? ZIP_FLUSH : ZIP_NO_FLUSH);

      if (ZIP_INPUT_CORRUPTED (zret))
	{
	  /* This only makes sense when decompressing a stream.  */
	  chop_log_printf (&filter->log, "pull: input stream corrupted");
	  err = CHOP_FILTER_ERROR;
	  break;
	}

      *pulled = size - zfilter->zstream.avail_out;
      if (flush)
	{
	  if ((ZIP_STREAM_ENDED (&zfilter->zstream, zret))
	      && (*pulled == 0))
	    {
	      /* We're done with this bunch of input processing.  So we can
		 reset the zip stream so that we can start processing input
		 anew eventually.  */
	      chop_log_printf (&filter->log, "pull: done with stream "
			       "processing");
	      ZIP_RESET_PROCESSING (&zfilter->zstream);
	      err = CHOP_FILTER_EMPTY;
	      break;
	    }

	  /* There is data remaining to be flushed so the user must call us
	     again (with FLUSH set again).  */
	  chop_log_printf (&filter->log, "pull: flush was requested but "
			   "data is still available; user should call "
			   "again (zret: %i, pulled: %i)", zret, *pulled);
	  err = 0;
	  break;
	}

      if (ZIP_CANT_PRODUCE_MORE (&zfilter->zstream, zret))
	{
	  /* BUFFER is not filled yet but producing more output would require
	     a larger buffer.  This can happen when compressing a file.  */
	  err = 0;
	  break;
	}

      if (zret != ZIP_OK)
	{
	  chop_log_printf (&filter->log, "pull: zip error: zret=%i",
			   zret);
	  err = CHOP_FILTER_ERROR;
	  break;
	}
    }

  if (err == CHOP_FILTER_EMPTY)
    return (*pulled ? 0 : err);

  return err;
}


/* Custom memory allocators.  */

static void *
custom_alloc (void *opaque, ZIP_CUSTOM_ALLOC_ITEM_T items,
	      ZIP_CUSTOM_ALLOC_ITEM_T size)
{
  return (chop_malloc (items * size,
		       (chop_class_t *) &ZIP_FILTER_CLASS));
}

static void
custom_free (void *opaque, void *address)
{
  chop_free (address, (chop_class_t *) &ZIP_FILTER_CLASS);
}

#undef ZIP_PUSH_METHOD
#undef ZIP_PULL_METHOD
#undef ZIP_FILTER_TYPE
#undef ZIP_FILTER_CLASS
