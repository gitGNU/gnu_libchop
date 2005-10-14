#include <chop/chop.h>
#include <chop/serializable.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <zlib.h>


/* Define `chop_zlib_unzip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS (zlib_unzip_filter, filter,
		       char *input_buffer;
		       size_t input_buffer_size;
		       z_stream zstream;);



static errcode_t
chop_zlib_unzip_push (chop_filter_t *filter,
		    const char *buffer, size_t size, size_t *pushed);

static errcode_t
chop_zlib_unzip_pull (chop_filter_t *filter, int flush,
		      char *buffer, size_t size, size_t *pulled);

static errcode_t
zlib_unzip_filter_ctor (chop_object_t *object,
		      const chop_class_t *class)
{
  chop_zlib_unzip_filter_t *zfilter;
  zfilter = (chop_zlib_unzip_filter_t *)object;

  zfilter->filter.push = chop_zlib_unzip_push;
  zfilter->filter.pull = chop_zlib_unzip_pull;
  zfilter->zstream.zalloc = Z_NULL;
  zfilter->zstream.zfree = Z_NULL;
  zfilter->zstream.opaque = Z_NULL;
  return chop_log_init ("zlib-unzip-filter", &zfilter->filter.log);
}

CHOP_DEFINE_RT_CLASS (zlib_unzip_filter, filter,
		      zlib_unzip_filter_ctor, NULL,
		      NULL, NULL);


errcode_t
chop_zlib_unzip_filter_init (size_t input_size,
			     chop_filter_t *filter)
{
  chop_zlib_unzip_filter_t *zfilter;

  zfilter = (chop_zlib_unzip_filter_t *)filter;

  chop_object_initialize ((chop_object_t *)filter,
			  &chop_zlib_unzip_filter_class);

  input_size = input_size ? input_size : 1024;
  zfilter->input_buffer = malloc (input_size);
  if (!zfilter->input_buffer)
    return ENOMEM;

  zfilter->input_buffer_size = input_size;

  inflateInit (&zfilter->zstream);

  zfilter->zstream.next_in = zfilter->input_buffer;
  zfilter->zstream.avail_in = 0;

  return 0;
}


/* The push and pull methods.  */
#define ZIP_TYPE       zlib
#define ZIP_DIRECTION  unzip

#define ZIP_FLUSH       Z_SYNC_FLUSH
#define ZIP_NO_FLUSH    0
#define ZIP_STREAM_END  Z_STREAM_END
#define ZIP_OK          Z_OK
#define ZIP_NO_PROGRESS Z_BUF_ERROR

#define ZIP_PROCESS(_zstream, _flush)  inflate ((_zstream), (_flush))
#define ZIP_NEED_MORE_INPUT(_zstream, _zret)   ((_zret) == Z_BUF_ERROR)
#define ZIP_CANT_PRODUCE_MORE(_zstream, _zret) (0)
#define ZIP_INPUT_CORRUPTED(_zret)             ((_zret) == Z_DATA_ERROR)
#define ZIP_RESET_PROCESSING(_zstream)         inflateReset ((_zstream))

#include "filter-zip-push-pull.c"

