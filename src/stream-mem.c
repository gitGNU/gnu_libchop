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

/* A memory-based stream.  */

#include <chop/chop-config.h>

#include <chop/chop.h>
#include <chop/streams.h>

CHOP_DECLARE_RT_CLASS (mem_stream, stream,
		       const char *base;
		       size_t offset;
		       size_t size;
		       void (* free_func) (void *););

static void chop_mem_stream_close (chop_stream_t *);
static chop_error_t chop_mem_stream_read (chop_stream_t *,
					  char *, size_t, size_t *);

/* The constructor.  */
static chop_error_t
mem_stream_ctor (chop_object_t *object,
		 const chop_class_t *class)
{
  chop_mem_stream_t *stream;

  stream = (chop_mem_stream_t *)object;
  stream->stream.close = chop_mem_stream_close;
  stream->stream.read = chop_mem_stream_read;
  stream->stream.preferred_block_size = 8192;

  stream->base = NULL;
  stream->offset = stream->size = 0;
  stream->free_func = NULL;

  return 0;
}


CHOP_DEFINE_RT_CLASS (mem_stream, stream,
		      mem_stream_ctor, NULL,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);



void
chop_mem_stream_open (const char *base, size_t size,
		      void (* free_func) (void *),
		      chop_stream_t *stream)
{
  chop_mem_stream_t *mem_stream;

  chop_object_initialize ((chop_object_t *)stream,
			  &chop_mem_stream_class);

  mem_stream = (chop_mem_stream_t *)stream;
  mem_stream->base = base;
  mem_stream->size = size;
  mem_stream->free_func = free_func;
}

static void
chop_mem_stream_close (chop_stream_t *stream)
{
  chop_mem_stream_t *mem_stream = (chop_mem_stream_t *)stream;

  if ((mem_stream->base) && (mem_stream->free_func))
    mem_stream->free_func ((void *)mem_stream->base);

  mem_stream->base = NULL;
  mem_stream->offset = mem_stream->size = 0;
}

static chop_error_t
chop_mem_stream_read (chop_stream_t *stream,
		      char *buffer, size_t howmuch, size_t *read)
{
  chop_mem_stream_t *mem_stream = (chop_mem_stream_t *)stream;
  size_t remaining;

  if (mem_stream->offset >= mem_stream->size)
    {
      *read = 0;
      return CHOP_STREAM_END;
    }

  remaining = mem_stream->size - mem_stream->offset;
  *read = (howmuch > remaining) ? remaining : howmuch;

  memcpy (buffer, &mem_stream->base[mem_stream->offset], *read);
  mem_stream->offset += *read;

  return 0;
}

