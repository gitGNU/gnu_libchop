#include <chop/chop.h>
#include <chop/streams.h>

#include <stdlib.h>

/* Definition of the `chop_stream_t' class.  */

static void
stream_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_stream_t *stream;

  stream = (chop_stream_t *)object;
  stream->name = NULL;
  stream->read = NULL;
  stream->close = NULL;
}

static void
stream_dtor (chop_object_t *object)
{
  chop_stream_t *stream;

  stream = (chop_stream_t *)object;
  if (stream->name)
    /* We are assuming that subclasses will use the standard libc allocation
       functions for NAME.  */
    free (stream->name);

  stream->name = NULL;
}

CHOP_DEFINE_RT_CLASS (stream, object,
		      stream_ctor, stream_dtor,
		      NULL, NULL  /* No serializer/deserializer */);


/* arch-tag: e83ca09d-301f-47c6-9826-e911c535a23a
 */
