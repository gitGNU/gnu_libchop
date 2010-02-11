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
#define ZIP_FILTER_CTOR  CONCAT3 (lzo_, ZIP_DIRECTION, _filter_ctor)
#define ZIP_FILTER_DTOR  CONCAT3 (lzo_, ZIP_DIRECTION, _filter_dtor)
#define ZIP_FILTER_CLASS CONCAT3 (chop_lzo_, ZIP_DIRECTION, _filter_class)

static errcode_t
ZIP_PUSH_METHOD (chop_filter_t *filter,
		 const char *buffer, size_t size, size_t *pushed)
{
  errcode_t err = 0;
  size_t offset;
  ZIP_FILTER_TYPE *zfilter;

  zfilter = (ZIP_FILTER_TYPE *)filter;

  *pushed = 0;
  offset = zfilter->input_offset + zfilter->avail_in;

  assert (offset <= zfilter->input_buffer_size);

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
			       "push: filter-full event unhandled: %s",
			       error_message (err));
	      if ((err != 0) && (err != CHOP_FILTER_UNHANDLED_FAULT))
		return err;

	      /* Only return CHOP_FILTER_FULL when not a single byte was
		 absorbed.  */
	      return ((*pushed == 0) ? CHOP_FILTER_FULL : 0);
	    }

	  zfilter->output_buffer_size = 0;
	}

      available = zfilter->input_buffer_size - offset;
      if (available > 0)
	{
	  amount = (available > size) ? size : available;
	  memcpy (zfilter->input_buffer + offset,
		  buffer, amount);

	  chop_log_printf (&filter->log,
			   "push: got %u bytes at offset %u",
			   amount, offset);

	  offset            += amount;
	  zfilter->avail_in += amount;
	  size              -= amount;
	  *pushed           += amount;
	}
    }

  return err;
}



/* Constructor and destructor.  */

static errcode_t
ZIP_FILTER_CTOR (chop_object_t *object, const chop_class_t *class)
{
  ZIP_FILTER_TYPE *zfilter;
  zfilter = (ZIP_FILTER_TYPE *) object;

  zfilter->filter.push = ZIP_PUSH_METHOD;
  zfilter->filter.pull = ZIP_PULL_METHOD;
  zfilter->input_buffer_size = zfilter->output_buffer_size = 0;
  zfilter->avail_in = zfilter->avail_out = 0;
  zfilter->input_offset = zfilter->output_offset = 0;
  zfilter->input_buffer = zfilter->output_buffer = NULL;

  return chop_log_init ("lzo-" STRINGIFY (ZIP_DIRECTION) "-filter",
			&zfilter->filter.log);
}

static void
ZIP_FILTER_DTOR (chop_object_t *object)
{
  ZIP_FILTER_TYPE *zfilter;
  zfilter = (ZIP_FILTER_TYPE *) object;

  if (zfilter->input_buffer)
    chop_free (zfilter->input_buffer, (chop_class_t *) &ZIP_FILTER_CLASS);
  if (zfilter->output_buffer)
    chop_free (zfilter->output_buffer, (chop_class_t *) &ZIP_FILTER_CLASS);
  if (zfilter->work_mem)
    chop_free (zfilter->work_mem, (chop_class_t *) &ZIP_FILTER_CLASS);

  zfilter->input_buffer = zfilter->output_buffer = NULL;
  zfilter->input_buffer_size = zfilter->output_buffer_size = 0;
  zfilter->avail_in = zfilter->avail_out = 0;

  chop_object_destroy ((chop_object_t *)&zfilter->filter.log);
}

/* arch-tag: 95b8ed0f-7671-44da-8357-18d79cc42879
 */
