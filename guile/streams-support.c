/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <stdlib.h>
#include <errno.h>

static errcode_t
chop_file_stream_open_alloc (const char *path, chop_stream_t **stream)
{
  errcode_t err;

  *stream = scm_malloc (chop_class_instance_size (&chop_file_stream_class));

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

  elements_copy = scm_malloc (size);
  memcpy (elements_copy, elements, size);

  stream = scm_malloc (chop_class_instance_size (&chop_mem_stream_class));

  /* ELEMENTS_COPY will be automatically freed with `free ()' when STREAM is
     closed.  */
  chop_mem_stream_open (elements_copy, size, free, stream);

 end:
  scm_array_handle_release (&handle);

  return stream;
}

static errcode_t
chop_filtered_stream_open_alloc (chop_stream_t *backend,
				 chop_filter_t *filter,
				 int close_backend,
				 chop_stream_t **stream)
{
  errcode_t err;

  *stream = scm_malloc (chop_class_instance_size (&chop_filtered_stream_class));
  err = chop_filtered_stream_open (backend,
				   /* Never destroy BACKEND: this is the GC's
				      job.  At most, close it when *STORE
				      gets closed.  */
				   close_backend
				   ? CHOP_PROXY_EVENTUALLY_CLOSE
				   : CHOP_PROXY_LEAVE_AS_IS,
				   filter, 0, *stream);
  if (err)
    {
      free (*stream);
      *stream = NULL;
    }

  return err;
}
