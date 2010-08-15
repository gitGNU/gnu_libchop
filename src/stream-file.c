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

/* Uncomment the following line to have file streams use `mmap'.  This is
   quite unsafe, since "the effect of references to portions of the mapped
   region that correspond to added or removed portions of the file is
   unspecified" when the file is changed concurrently:

   http://www.opengroup.org/onlinepubs/000095399/functions/mmap.html

   On GNU/Linux, accessing to an mmapped file that has been modified (e.g.,
   truncated) soon yields a segmentation fault.  */
/* #define USE_MMAP */

#include <chop/chop.h>
#include <chop/streams.h>

#include <sys/types.h>
#include <sys/stat.h>
#ifdef USE_MMAP
# include <sys/mman.h>
#endif

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include <full-read.h>


/* Class definition.  */

/* File stream class that inherits from `chop_stream_t'.  This declares
   CHOP_FILE_STREAM_CLASS, the object representing this class at
   run-time.  */
CHOP_DECLARE_RT_CLASS (file_stream, stream,
		       int    fd;
		       size_t size;
		       char  *map;
		       size_t position;);


/* Note that the destructor of class `stream' calls `chop_stream_close ()',
   so we don't need to define our own destructor.  */
CHOP_DEFINE_RT_CLASS (file_stream, stream,
		      NULL, NULL,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);



static void chop_file_stream_close (chop_stream_t *);
static chop_error_t chop_file_stream_read (chop_stream_t *,
					   char *, size_t, size_t *);

chop_error_t
chop_file_stream_open (const char *path,
		       chop_stream_t *raw_stream)
{
  chop_error_t err;
  int fd;
  void *map;
  struct stat file_stats;
  chop_file_stream_t *stream = (chop_file_stream_t *)raw_stream;

  err = stat (path, &file_stats);
  if (err)
    return errno;

  fd = open (path, O_RDONLY);
  if (fd == -1)
    return errno;

#ifdef USE_MMAP
  map = mmap (0, file_stats.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (map == MAP_FAILED)
    {
      close (fd);
      return errno;
    }

  madvise (map, file_stats.st_size, MADV_SEQUENTIAL);
#else
  map = NULL;
#endif

  chop_object_initialize ((chop_object_t *)raw_stream,
			  &chop_file_stream_class);

  stream->fd = fd;
  stream->map = map;

  stream->position = 0;
  stream->stream.close = chop_file_stream_close;
  stream->stream.read = chop_file_stream_read;
  stream->stream.name = chop_strdup (path, &chop_file_stream_class);
  stream->stream.preferred_block_size = file_stats.st_blksize;
  stream->size = file_stats.st_size;

  return 0;
}

static chop_error_t
chop_file_stream_read (chop_stream_t *stream,
		       char *buffer, size_t howmuch, size_t *bytes_read)
{
  chop_file_stream_t *file = (chop_file_stream_t *)stream;

#ifdef USE_MMAP

  size_t remaining;

  if (file->position >= file->size)
    {
      *bytes_read = 0;
      return CHOP_STREAM_END;
    }

  remaining = file->size - file->position;
  *bytes_read = (howmuch > remaining) ? remaining : howmuch;

  memcpy (buffer, &file->map[file->position], *bytes_read);

#else

  *bytes_read = full_read (file->fd, buffer, howmuch);
  if (*bytes_read == 0 && errno == 0)
    return CHOP_STREAM_END;

#endif

  file->position += *bytes_read;

  return 0;
}

static void
chop_file_stream_close (chop_stream_t *stream)
{
  chop_file_stream_t *file = (chop_file_stream_t *)stream;

#ifdef USE_MMAP
  if (file->map != NULL)
    munmap (file->map, file->size);
#endif

  if (file->fd > 2)
    close (file->fd);

  file->map = NULL;
  file->fd = -1;
}
