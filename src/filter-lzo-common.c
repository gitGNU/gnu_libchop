/* Template for the push and pull methods for the LZO zip/unzip filters.
   This file expects a number of macros to be defined.  */

#if (!defined ZIP_DIRECTION)
# error "This file is meant to be included in some other source file."
#endif


#ifndef CONCAT3
# define _CONCAT3(_x, _y, _z)  _x ## _y ## _z
# define CONCAT3(_a, _b, _c)  _CONCAT3 (_a, _b, _c)
#endif

#ifndef STRINGIFY
# define _STRINGIFY(_x) # _x
# define STRINGIFY(_z) _STRINGIFY(_z)
#endif

#define ZIP_PUSH_METHOD  CONCAT3 (chop_lzo_, ZIP_DIRECTION, _push)
#define ZIP_PULL_METHOD  CONCAT3 (chop_lzo_, ZIP_DIRECTION, _pull)
#define ZIP_FILTER_TYPE  CONCAT3 (chop_lzo_, ZIP_DIRECTION, _filter_t)

static errcode_t
ZIP_PUSH_METHOD (chop_filter_t *filter,
		 const char *buffer, size_t size, size_t *pushed)
{
  errcode_t err;
  ZIP_FILTER_TYPE *zfilter;

  zfilter = (ZIP_FILTER_TYPE *)filter;

  *pushed = 0;
  while ((size > 0) && (err == 0))
    {
      size_t available, amount;

      if (zfilter->avail_out > 0)
	{
	  chop_log_printf (&filter->log, "filter is full, output fault");
	  err = chop_filter_handle_output_fault (filter,
						 zfilter->output_buffer_size);
	  if (err)
	    {
	      chop_log_printf (&filter->log,
			       "filter-full event unhandled: %s",
			       error_message (err));
	      if ((err != 0) && (err != CHOP_FILTER_UNHANDLED_FAULT))
		return err;

	      /* Only return CHOP_FILTER_FULL when not a single byte was
		 absorbed.  */
	      return ((*pushed == 0) ? CHOP_FILTER_FULL : 0);
	    }

	  zfilter->output_buffer_size = 0;
	}

      available = zfilter->input_buffer_size - zfilter->avail_in;
      if (available > 0)
	{
	  amount = (available > size) ? size : available;
	  memcpy (zfilter->input_buffer + zfilter->avail_in,
		  buffer, amount);

	  zfilter->avail_in += amount;
	  size -= amount;
	  *pushed += amount;
	}
      else

    }

  return err;
}



/* Constructor and destructor.  */

static errcode_t
lzo_ ## ZIP_DIRECTION ## _filter_ctor (chop_object_t *object,
				       const chop_class_t *class)
{
  ZIP_FILTER_TYPE *zfilter;
  zfilter = (ZIP_FILTER_TYPE *) object;

  zfilter->filter.push = chop_lzo_zip_push;
  zfilter->filter.pull = chop_lzo_zip_pull;
  zfilter->input_buffer_size = zfilter->output_buffer_size = 0;
  zfilter->avail_in = zfilter->avail_out = 0;
  zfilter->input_buffer = zfilter->output_buffer = NULL;

  return chop_log_init ("lzo-" STRINGIFY (ZIP_DIRECTION) "-filter",
			&zfilter->filter.log);
}

static void
lzo_ ## ZIP_DIRECTION ## _filter_dtor (chop_object_t *object)
{
  ZIP_FILTER_TYPE *zfilter;
  zfilter = (ZIP_FILTER_TYPE *) object;

  if (zfilter->input_buffer)
    free (zfilter->input_buffer);
  if (zfilter->output_buffer)
    free (zfilter->output_buffer);
  if (zfilter->work_mem)
    free (zfilter->work_mem);

  zfilter->input_buffer = zfilter->output_buffer = NULL;
  zfilter->input_buffer_size = zfilter->output_buffer_size = 0;
  zfilter->avail_in = zfilter->avail_out = 0;

  chop_object_destroy ((chop_object_t *)&zfilter->filter.log);
}

/* arch-tag: 95b8ed0f-7671-44da-8357-18d79cc42879
 */
