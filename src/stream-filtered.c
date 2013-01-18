/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2013  Ludovic Courtès <ludo@gnu.org>
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

#include <alloca.h>

#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/filters.h>

#include <assert.h>

CHOP_DECLARE_RT_CLASS (filtered_stream, stream,
		       chop_stream_t *backend;
		       chop_filter_t *filter;
		       chop_filter_fault_handler_t filter_input_handler;
		       chop_proxy_semantics_t backend_ps;
		       int owns_filter;
		       int flushing;
		       int finished;);

static chop_error_t
fs_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_filtered_stream_t *stream;

  stream = (chop_filtered_stream_t *)object;
  stream->backend = NULL;
  stream->filter = NULL;
  stream->backend_ps = CHOP_PROXY_LEAVE_AS_IS;
  stream->owns_filter = 0;
  stream->finished = 0;

  return 0;
}

CHOP_DEFINE_RT_CLASS (filtered_stream, stream,
		      fs_ctor, NULL, /* the dtor of `stream' calls `close' */
		      NULL, NULL,
		      NULL, NULL);



static chop_error_t
handle_input_fault (chop_filter_t *filter, size_t how_much, void *data)
{
  chop_error_t err;
  chop_filtered_stream_t *stream;
  size_t read;
  char *buffer;

  stream = (chop_filtered_stream_t *)data;
  buffer = alloca (how_much);

  err = chop_stream_read (stream->backend, buffer, how_much, &read);
  if (!err)
    {
      size_t pushed;

      err = chop_filter_push (filter, buffer, read, &pushed);
      if (!err)
	/* XXX: PUSHED will certainly always be equal to READ because READ <=
	   HOW_MUCH.  */
	assert (pushed == read);
    }

  if (err == CHOP_STREAM_END)
    /* Comply with the filter interface.  */
    err = CHOP_FILTER_UNHANDLED_FAULT;

  return err;
}

static chop_error_t
filtered_stream_read (chop_stream_t *raw_stream,
		      char *buffer, size_t howmuch, size_t *read)
{
  chop_error_t err;
  chop_filtered_stream_t *stream;

  stream = (chop_filtered_stream_t *)raw_stream;
  if (stream->finished)
    return CHOP_STREAM_END;

  *read = 0;
  err = chop_filter_pull (stream->filter, stream->flushing,
			  buffer, howmuch, read);
  if ((err == CHOP_FILTER_EMPTY) && (!stream->flushing))
    {
      size_t some_more;

      stream->flushing = 1;
      err = chop_filter_pull (stream->filter, 1,
			      buffer + *read, howmuch - *read,
			      &some_more);
      if (!err)
	*read += some_more;
    }

  if (stream->flushing && (err == CHOP_FILTER_EMPTY))
    {
      /* Comply with the stream interface.  */
      if (*read)
	err = 0;
      else
	{
	  stream->finished = 1;
	  err = CHOP_STREAM_END;
	}
    }

  return err;
}

static void
filtered_stream_close (chop_stream_t *raw_stream)
{
  chop_filtered_stream_t *stream;

  stream = (chop_filtered_stream_t *)raw_stream;

  if (stream->filter)
    {
      if (stream->owns_filter)
	chop_object_destroy ((chop_object_t *)stream->filter);
      else
	chop_filter_set_input_fault_handler
	  (stream->filter,
	   stream->filter_input_handler.handle,
	   stream->filter_input_handler.data);
    }

  if (stream->backend)
    {
      switch (stream->backend_ps)
	{
	case CHOP_PROXY_LEAVE_AS_IS:
	  /* Leave BACKEND as is.  */
	  break;

	case CHOP_PROXY_EVENTUALLY_CLOSE:
	  /* Close without destroy STREAM->BACKEND.  */
	  chop_stream_close (stream->backend);
	  break;

	case CHOP_PROXY_EVENTUALLY_DESTROY:
	  /* Close and destroy STREAM->BACKEND.  */
	  chop_object_destroy ((chop_object_t *)stream->backend);
	  break;

	case CHOP_PROXY_EVENTUALLY_FREE:
	  chop_object_destroy ((chop_object_t *)stream->backend);
	  free (stream->backend);
	  break;

	default:
	  abort ();
	}
    }

  stream->filter = NULL;
  stream->backend = NULL;
  stream->owns_filter = 0;
}



chop_error_t
chop_filtered_stream_open (chop_stream_t *backend,
			   chop_proxy_semantics_t bps,
			   chop_filter_t *filter,
			   int owns_filter,
			   chop_stream_t *raw_stream)
{
  chop_error_t err;
  chop_filtered_stream_t *stream;

  err = chop_object_initialize ((chop_object_t *)raw_stream,
				&chop_filtered_stream_class);
  if (err)
    return err;

  stream = (chop_filtered_stream_t *)raw_stream;
  stream->stream.read = filtered_stream_read;
  stream->stream.close = filtered_stream_close;
  stream->stream.preferred_block_size =
    chop_stream_preferred_block_size (backend);

  stream->backend = backend;
  stream->filter = filter;
  stream->owns_filter = owns_filter;
  stream->backend_ps = bps;
  stream->flushing = stream->finished = 0;

  stream->filter_input_handler = chop_filter_input_fault_handler (filter);
  chop_filter_set_input_fault_handler (filter, handle_input_fault,
				       stream);

  return 0;
}

/* arch-tag: 02dccaaf-4d9a-4f40-8c4b-39b620538fa1
 */
