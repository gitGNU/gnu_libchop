#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/blocks.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>



static void chop_file_stream_close (chop_stream_t *);
static errcode_t chop_file_stream_read (chop_stream_t *,
					char *, size_t, size_t *);

errcode_t
chop_file_stream_open (const char *path,
		       chop_file_stream_t *stream)
{
  errcode_t err;
  struct stat file_stats;

  err = stat (path, &file_stats);
  if (err)
    return err;

  stream->fd = open (path, O_RDONLY);
  if (stream->fd == -1)
    return errno;

  stream->map = mmap (0, file_stats.st_size, PROT_READ, MAP_SHARED,
		      stream->fd, 0);
  if (stream->map == MAP_FAILED)
    {
      close (stream->fd);
      return errno;
    }

  madvise (stream->map, stream->size, MADV_SEQUENTIAL);

  stream->position = 0;
  stream->stream.close = chop_file_stream_close;
  stream->stream.read = chop_file_stream_read;
  stream->stream.name = strdup (path);
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

  memcpy (buffer, file->map, *read);
  file->position += *read;

  return 0;
}

static void
chop_file_stream_close (struct chop_stream *s)
{
  chop_file_stream_t *file = (chop_file_stream_t *)s;

  free (s->name);

  munmap (file->map, file->size);
  close (file->fd);

  file->map = NULL;
  file->fd = -1;
}
