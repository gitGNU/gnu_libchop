/* Contructors with a functional style that perform memory allocation by
   themselves.  */

static chop_stream_t *
chop_file_stream_open_alloc (const char *path)
{
  return (malloc (chop_class_instance_size (&chop_file_stream_class)));
}

static void
chop_stream_close_dealloc (chop_stream_t *stream)
{
  chop_stream_close (stream);
  free (stream);
}



