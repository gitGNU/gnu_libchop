/* A memory-based stream.  */

#include <chop/chop.h>
#include <chop/streams.h>

CHOP_DECLARE_RT_CLASS (mem_stream, stream,
		       const char *base;
		       size_t offset;
		       size_t size;
		       void (* free_func) (void *););

static void chop_mem_stream_close (chop_stream_t *);
static errcode_t chop_mem_stream_read (chop_stream_t *,
				       char *, size_t, size_t *);

/* The constructor.  */
static errcode_t
mem_stream_ctor (chop_object_t *object,
		 const chop_class_t *class)
{
  chop_mem_stream_t *stream;

  stream = (chop_mem_stream_t *)object;
  stream->stream.close = chop_mem_stream_close;
  stream->stream.read = chop_mem_stream_read;
  stream->stream.preferred_block_size = 8192;

  stream->base = NULL;
  stream->offset = stream->size = 0;
  stream->free_func = NULL;

  return 0;
}

static void
mem_stream_dtor (chop_object_t *object)
{
  chop_mem_stream_t *mem_stream = (chop_mem_stream_t *)object;

  chop_stream_close ((chop_stream_t *)mem_stream);
}


CHOP_DEFINE_RT_CLASS (mem_stream, stream,
		      mem_stream_ctor, mem_stream_dtor,
		      NULL, NULL  /* No serializer/deserializer */);



void
chop_mem_stream_open (const char *base, size_t size,
		      void (* free_func) (void *),
		      chop_stream_t *stream)
{
  chop_mem_stream_t *mem_stream;

  chop_object_initialize ((chop_object_t *)stream,
			  &chop_mem_stream_class);

  mem_stream = (chop_mem_stream_t *)stream;
  mem_stream->base = base;
  mem_stream->size = size;
  mem_stream->free_func = free_func;
}

static void
chop_mem_stream_close (chop_stream_t *stream)
{
  chop_mem_stream_t *mem_stream = (chop_mem_stream_t *)stream;

  if ((mem_stream->base) && (mem_stream->free_func))
    mem_stream->free_func ((void *)mem_stream->base);

  mem_stream->base = NULL;
  mem_stream->offset = mem_stream->size = 0;
}

static errcode_t
chop_mem_stream_read (chop_stream_t *stream,
		      char *buffer, size_t howmuch, size_t *read)
{
  chop_mem_stream_t *mem_stream = (chop_mem_stream_t *)stream;
  size_t remaining;

  if (mem_stream->offset >= mem_stream->size)
    {
      *read = 0;
      return CHOP_STREAM_END;
    }

  remaining = mem_stream->size - mem_stream->offset;
  *read = (howmuch > remaining) ? remaining : howmuch;

  memcpy (buffer, &mem_stream->base[mem_stream->offset], *read);
  mem_stream->offset += *read;

  return 0;
}

