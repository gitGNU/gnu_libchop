#include "chop.h"
#include "streams.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <ext2fs/ext2fs.h>

struct chop_ext2_stream
{
  chop_stream_t stream;

  ext2_filsys fsys;
  ext2_file_t file;
};


static void chop_ext2_stream_close (struct chop_stream *);

errcode_t
chop_ext2_stream_open (const char *path, const char *fs,
		       chop_ext2_stream_t *stream)
{
  errcode_t err;
  struct stat file_stats;

  err = stat (path, &file_stats);
  if (err)
    return err;

  err = ext2fs_open (fs, 0 /* read-only */, 0 /* superblock */,
		     0 /* block size */, unix_io_manager, &stream->fsys);
  if (err)
    return err;

  err = ext2fs_file_open (stream->fsys, file_stats.st_ino,
			  0, &stream->file);
  if (err)
    return err;

  stream->stream.name = strdup (path);
  stream->stream.close = chop_ext2_stream_close;

  return 0;
}

static void
chop_ext2_stream_close (struct chop_stream *s)
{
  chop_ext2_stream_t *e2 = (chop_ext2_stream_t *)s;

  free (s->name);

  ext2fs_free (e2->fsys);
}

