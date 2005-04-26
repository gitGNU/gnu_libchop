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

static chop_stream_t *
chop_mem_stream_open_alloc (SCM u8vector)
{
  chop_stream_t *stream = NULL;
  const char *elements;
  char *elements_copy;
  scm_t_array_handle handle;
  size_t size;
  ssize_t increment;

  elements = scm_u8vector_elements (u8vector, &handle, &size, &increment);
  if (increment != 1)
    /* Lazyness... */
    goto end;

  elements_copy = malloc (size);
  if (!elements_copy)
    goto end;

  memcpy (elements_copy, elements, size);

  stream = malloc (chop_class_instance_size (&chop_mem_stream_class));
  if (!stream)
    {
      free (elements_copy);
      goto end;
    }

  /* ELEMENTS_COPY will be automatically freed with `free ()' when STREAM is
     closed.  */
  chop_mem_stream_open (elements_copy, size, free, stream);

 end:
  scm_array_handle_release (&handle);

  return stream;
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



