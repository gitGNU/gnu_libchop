#include "chop.h"
#include "streams.h"
#include "blocks.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>



static void chop_file_stream_close (chop_stream_t *);
static size_t chop_file_block_size (const chop_stream_t *);
static errcode_t chop_file_stream_break (chop_file_stream_t *,
					 size_t, size_t *);
static errcode_t chop_file_stream_block (chop_file_stream_t *,
					 size_t blocknum,
					 chop_block_t *);

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

  stream->stream.close = chop_file_stream_close;
  stream->stream.preferred_block_size = chop_file_block_size;
  stream->stream.sbreak = chop_file_stream_break;
  stream->stream.get_block = chop_file_stream_block;

  stream->stream.name = strdup (path);
  stream->stream.block_size = 0;
  stream->fsys_block_size = file_stats.st_blksize;
  stream->size = file_stats.st_size;

  return 0;
}

static errcode_t
chop_file_stream_break (chop_file_stream_t *file,
			size_t block_size,
			size_t *nblocks)
{
  *nblocks = file->size / block_size + (file->size % block_size ? 1 : 0);
  return 0;
}

static errcode_t
chop_file_stream_block (chop_file_stream_t *file,
			size_t blocknum,
			chop_block_t *block)
{
  size_t offset;
  block->size = file->stream.block_size;
  offset = block->size * blocknum;
  block->content = (char *)file->map + offset;

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

static size_t
chop_file_block_size (const chop_stream_t *s)
{
  chop_file_stream_t *f = (chop_file_stream_t *)s;
  return (f->fsys_block_size);
}
