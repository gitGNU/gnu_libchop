#include <chop/chop.h>
#include <chop/serializable.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <zlib.h>


static void
filter_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_filter_t *filter = (chop_filter_t *)object;

  filter->input_fault_handler.handle = NULL;
  filter->output_fault_handler.handle = NULL;
  filter->within_fault_handler = 0;
}

CHOP_DEFINE_RT_CLASS (filter, object,
		      filter_ctor, NULL,
		      NULL, NULL);

/* Define `chop_zlib_zip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS (zlib_zip_filter, filter,
		       char *input_buffer;
		       size_t input_buffer_size;
		       z_stream zstream;);



static errcode_t
chop_zlib_zip_push (chop_filter_t *filter,
		    const char *buffer, size_t size);

static errcode_t
chop_zlib_zip_pull (chop_filter_t *filter, int flush,
		    char *buffer, size_t size, size_t *pulled);

static void
zlib_zip_filter_ctor (chop_object_t *object,
		      const chop_class_t *class)
{
  chop_zlib_zip_filter_t *zfilter;
  zfilter = (chop_zlib_zip_filter_t *)object;

  zfilter->filter.push = chop_zlib_zip_push;
  zfilter->filter.pull = chop_zlib_zip_pull;
  zfilter->zstream.zalloc = Z_NULL;
  zfilter->zstream.zfree = Z_NULL;
  zfilter->zstream.opaque = Z_NULL;
  chop_log_init ("zlib-zip-filter", &zfilter->filter.log);
}

CHOP_DEFINE_RT_CLASS (zlib_zip_filter, filter,
		      zlib_zip_filter_ctor, NULL,
		      NULL, NULL);

errcode_t
chop_zlib_zip_filter_init (int zlib_compression_level, size_t input_size,
			   chop_filter_t *filter)
{
  chop_zlib_zip_filter_t *zfilter;

  zfilter = (chop_zlib_zip_filter_t *)filter;

  chop_object_initialize ((chop_object_t *)filter,
			  &chop_zlib_zip_filter_class);

  input_size = input_size ? input_size : 1024;
  zfilter->input_buffer = malloc (input_size);
  if (!zfilter->input_buffer)
    return ENOMEM;

  zfilter->input_buffer_size = input_size;

  deflateInit (&zfilter->zstream,
	       (zlib_compression_level >= 0)
	       ? zlib_compression_level : Z_DEFAULT_COMPRESSION);

  zfilter->zstream.next_in = zfilter->input_buffer;
  zfilter->zstream.avail_in = 0;

  return 0;
}

static errcode_t
chop_zlib_zip_push (chop_filter_t *filter,
		    const char *buffer, size_t size)
{
  errcode_t err;
  chop_zlib_zip_filter_t *zfilter;

  zfilter = (chop_zlib_zip_filter_t *)filter;

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

	      return CHOP_FILTER_FULL;
	    }

	  continue;
	}

      available = zfilter->input_buffer_size - zfilter->zstream.avail_in;
      amount = (available > size) ? size : available;
      memcpy (zfilter->zstream.next_in, buffer, amount);

      zfilter->zstream.next_in += amount;
      zfilter->zstream.avail_in += amount;
      buffer += amount;
      size -= amount;
    }

  return 0;
}

static errcode_t
chop_zlib_zip_pull (chop_filter_t *filter, int flush,
		    char *buffer, size_t size, size_t *pulled)
{
  int zret;
  errcode_t err;
  chop_zlib_zip_filter_t *zfilter;

  zfilter = (chop_zlib_zip_filter_t *)filter;

  zfilter->zstream.avail_out = size;
  zfilter->zstream.next_out = buffer;
  *pulled = 0;
  while (size > 0)
    {
      if ((zfilter->zstream.avail_in == 0) && (!flush))
	{
	  chop_log_printf (&filter->log, "filter is empty, input fault");
	  err = chop_filter_handle_input_fault (filter,
						zfilter->input_buffer_size);
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


      zret = deflate (&zfilter->zstream, flush ? Z_FINISH : 0);
      if (flush)
	{
	  *pulled += size - zfilter->zstream.avail_out;
	  if (zret == Z_STREAM_END)
	    /* We're done with this.  */
	    return CHOP_FILTER_EMPTY;

	  /* There is data remaining to be flushed so the user must call us
	     again (with FLUSH set again).  */
	  chop_log_printf (&filter->log, "pull: flush was requested but "
			   "data is still available; user should call "
			   "again");
	  return 0;
	}
      else
	*pulled += size - zfilter->zstream.avail_out;
    }

  return 0;
}
