/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <stdlib.h>
#include <errno.h>

static errcode_t
chop_file_stream_open_alloc (const char *path, chop_stream_t **stream)
{
  errcode_t err;

  *stream = malloc (chop_class_instance_size (&chop_file_stream_class));
  if (!*stream)
    return ENOMEM;

  err = chop_file_stream_open (path, *stream);
  if (err)
    free (*stream);

  return err;
}

static void
chop_stream_close_dealloc (chop_stream_t *stream)
{
  if (stream)
    {
      chop_stream_close (stream);
      free (stream);
    }
}



