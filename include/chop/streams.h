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


#ifndef CHOP_STREAMS_H
#define CHOP_STREAMS_H

/* Input data streams.  */

#include <chop/chop.h>
#include <chop/objects.h>


_CHOP_BEGIN_DECLS

/* Declare the input stream class `chop_stream_t' (represented at run-time by
   CHOP_STREAM_CLASS) inheriting from `chop_object_t'.  */
CHOP_DECLARE_RT_CLASS (stream, object,
		       char *name;
		       size_t preferred_block_size;

		       /* common methods */
		       chop_error_t (* read)  (struct chop_stream *,
					       char *, size_t, size_t *);
		       void (* close) (struct chop_stream *););


extern const chop_class_t chop_file_stream_class;
extern const chop_class_t chop_mem_stream_class;
extern const chop_class_t chop_filtered_stream_class;



/* Stream methods */

/* Return the preferred block size for reading STREAM.  */
static __inline__ size_t
chop_stream_preferred_block_size (const chop_stream_t *__stream)
{
  return (__stream->preferred_block_size);
}

/* Read at most SIZE bytes from STREAM into BUFFER.  On failure, return an
   error code (non-zero).  Otherwise, return in READ the number of bytes
   actually read.  */
static __inline__ chop_error_t
chop_stream_read (chop_stream_t *__stream,
		  char *__buffer,
		  size_t __size,
		  size_t *__read)
{
  return (__stream->read (__stream, __buffer, __size, __read));
}

/* Close STREAM, i.e. deallocate any resources associated to it.  */
static __inline__ void
chop_stream_close (chop_stream_t *__stream)
{
  __stream->close (__stream);
}



/* Specific stream constructors.  */

/* Open file located at PATH and initialize STREAM as a file stream
   representing this file.  STREAM has to point to a large-enough memory area
   to hold an object whose class is CHOP_FILE_STREAM_CLASS.  */
extern chop_error_t chop_file_stream_open (const char *path,
					   chop_stream_t *stream);

/* Same as above, except that data is read from FD, an open file descriptor.
   If EVENTUALLY_CLOSE is true, then `chop_stream_close' will invoke close(2)
   on FD; otherwise it will leave it open.  */
extern chop_error_t chop_file_stream_open_fd (int fd, int eventually_close,
					      chop_stream_t *stream);


/* Open a memory-backed stream, i.e. a stream whose input is read from BASE
   which is SIZE byte-long.  If FREE_FUNC is not NULL, it is called upon
   closing STREAM.  */
extern void chop_mem_stream_open (const char *base, size_t size,
				  void (* free_func) (void *),
				  chop_stream_t *stream);

#include <chop/filters.h>

/* Initialize STREAM as a filtered stream that reads input data from BACKEND
   through FILTER.  BPS defines the semantics of STREAM as a proxy of BACKEND
   (whether BACKEND should eventually be destroyed, etc.).  Similarly, if
   OWNS_FILTER is true, then closing STREAM will destroy FILTER.  */
extern chop_error_t chop_filtered_stream_open (chop_stream_t *backend,
					       chop_proxy_semantics_t bps,
					       chop_filter_t *filter,
					       int owns_filter,
					       chop_stream_t *stream);


_CHOP_END_DECLS;

#endif
