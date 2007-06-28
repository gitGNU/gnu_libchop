#include <chop/chop.h>
#include <chop/streams.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>



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
static errcode_t chop_file_stream_read (chop_stream_t *,
					char *, size_t, size_t *);

errcode_t
chop_file_stream_open (const char *path,
		       chop_stream_t *raw_stream)
{
  errcode_t err;
  int fd;
  void *map;
  struct stat file_stats;
  chop_file_stream_t *stream = (chop_file_stream_t *)raw_stream;

  err = stat (path, &file_stats);
  if (err)
    return err;

  fd = open (path, O_RDONLY);
  if (fd == -1)
    return errno;

  map = mmap (0, file_stats.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (map == MAP_FAILED)
    {
      close (fd);
      return errno;
    }

  madvise (map, file_stats.st_size, MADV_SEQUENTIAL);

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

static errcode_t
chop_file_stream_read (chop_stream_t *stream,
		       char *buffer, size_t howmuch, size_t *read)
{
  chop_file_stream_t *file = (chop_file_stream_t *)stream;
  size_t remaining;

  if (file->position >= file->size)
    {
      *read = 0;
      return CHOP_STREAM_END;
    }

  remaining = file->size - file->position;
  *read = (howmuch > remaining) ? remaining : howmuch;

  memcpy (buffer, &file->map[file->position], *read);
  file->position += *read;

  return 0;
}

static void
chop_file_stream_close (chop_stream_t *stream)
{
  chop_file_stream_t *file = (chop_file_stream_t *)stream;

  munmap (file->map, file->size);
  if (file->fd > 2)
    close (file->fd);

  file->map = NULL;
  file->fd = -1;
}
