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

#include <chop/chop.h>
#include <chop/streams.h>

#include <stdlib.h>

/* Definition of the `chop_stream_t' class.  */

static errcode_t
stream_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_stream_t *stream;

  stream = (chop_stream_t *)object;
  stream->name = NULL;
  stream->read = NULL;
  stream->close = NULL;
  stream->preferred_block_size = 0;

  return 0;
}

static void
stream_dtor (chop_object_t *object)
{
  chop_stream_t *stream;

  stream = (chop_stream_t *)object;

  /* Because we call `close' here, sub-classes usually don't need to define a
     destructor (provided they define `close').
     XXX:  This is actually quite evil since at this point STREAM is already
     partly destroyed.  */
  chop_stream_close (stream);

  if (stream->name)
    /* We are assuming that subclasses will use the standard libchop
       allocation functions for NAME.  */
    chop_free (stream->name, chop_object_get_class (object));

  stream->name = NULL;
}

CHOP_DEFINE_RT_CLASS (stream, object,
		      stream_ctor, stream_dtor,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);


/* arch-tag: e83ca09d-301f-47c6-9826-e911c535a23a
 */
