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

static errcode_t
ZIP_PUSH_METHOD (chop_filter_t *filter,
		 const char *buffer, size_t size, size_t *pushed)
{
  errcode_t err;
  ZIP_FILTER_TYPE *zfilter;

  zfilter = (ZIP_FILTER_TYPE *)filter;

  if (zfilter->zstream.avail_in == 0)
    zfilter->zstream.next_in = zfilter->input_buffer;

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
  errcode_t err;
  ZIP_FILTER_TYPE *zfilter;

  zfilter = (ZIP_FILTER_TYPE *)filter;

  zfilter->zstream.avail_out = size;
  zfilter->zstream.next_out = buffer;
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
			   "filter is empty, input fault (%u bytes)",
			   howmuch);

	  err = chop_filter_handle_input_fault (filter, howmuch);
	  if (err)
	    {
	      chop_log_printf (&filter->log,
			       "input fault unhandled: %s",
			       error_message (err));
	      if ((err != 0) && (err != CHOP_FILTER_UNHANDLED_FAULT))
		return err;

	      return CHOP_FILTER_EMPTY;
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
	  return CHOP_FILTER_ERROR;
	}

      *pulled = size - zfilter->zstream.avail_out;
      if (flush)
	{
	  if ((zret == ZIP_STREAM_END) && (*pulled == 0))
	    {
	      /* We're done with this bunch of input processing.  So we can
		 reset the zip stream so that we can start processing input
		 anew eventually.  */
	      ZIP_RESET_PROCESSING (&zfilter->zstream);
	      return CHOP_FILTER_EMPTY;
	    }

	  /* There is data remaining to be flushed so the user must call us
	     again (with FLUSH set again).  */
	  chop_log_printf (&filter->log, "pull: flush was requested but "
			   "data is still available; user should call "
			   "again");
	  return 0;
	}

      if (ZIP_CANT_PRODUCE_MORE (&zfilter->zstream, zret))
	/* BUFFER is not filled yet but producing more output would require a
	   larger buffer.  This can happen when compressing a file.  */
	return 0;
    }

  return 0;
}

#undef ZIP_PUSH_METHOD
#undef ZIP_PULL_METHOD
#undef ZIP_FILTER_TYPE