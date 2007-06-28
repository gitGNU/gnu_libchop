#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <zlib.h>


/* Define `chop_zlib_zip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (zlib_zip_filter, filter,
				      zip_filter_class,

				      chop_malloc_t malloc;
				      chop_free_t free;

				      size_t  block_count_100k;
				      char *input_buffer;
				      size_t input_buffer_size;
				      z_stream zstream;);



static errcode_t
chop_zlib_zip_push (chop_filter_t *filter,
		    const char *buffer, size_t size, size_t *pushed);

static errcode_t
chop_zlib_zip_pull (chop_filter_t *filter, int flush,
		    char *buffer, size_t size, size_t *pulled);

static void *
custom_alloc (voidp opaque, uInt items, uInt size);

static void
custom_free (voidpf opaque, voidp address);


static errcode_t
zlib_zip_filter_ctor (chop_object_t *object,
		      const chop_class_t *class)
{
  chop_zlib_zip_filter_t *zfilter;
  zfilter = (chop_zlib_zip_filter_t *)object;

  zfilter->filter.push = chop_zlib_zip_push;
  zfilter->filter.pull = chop_zlib_zip_pull;
  zfilter->zstream.zalloc = Z_NULL;
  zfilter->zstream.zfree = Z_NULL;
  zfilter->zstream.opaque = Z_NULL;
  zfilter->malloc = NULL;
  zfilter->free = NULL;
  return chop_log_init ("zlib-zip-filter", &zfilter->filter.log);
}

static void
zlib_zip_filter_dtor (chop_object_t *object)
{
  chop_zlib_zip_filter_t *zfilter;
  zfilter = (chop_zlib_zip_filter_t *)object;

  deflateEnd (&zfilter->zstream);
  zfilter->zstream.zalloc = Z_NULL;
  zfilter->zstream.zfree = Z_NULL;
  zfilter->zstream.opaque = Z_NULL;

  if (zfilter->input_buffer)
    {
      if (zfilter->free)
	zfilter->free (zfilter->input_buffer,
		       (chop_class_t *) &chop_bzip2_zip_filter_class);
      else
	free (zfilter->input_buffer);
    }

  zfilter->input_buffer = NULL;
  zfilter->input_buffer_size = 0;

  chop_object_destroy ((chop_object_t *)&zfilter->filter.log);
}

static errcode_t
zzf_open (int compression_level, size_t input_size,
	  chop_malloc_t malloc, chop_realloc_t realloc, chop_free_t free,
	  chop_filter_t *filter)
{
  return (chop_zlib_zip_filter_init2 (compression_level, input_size,
				      malloc, realloc, free,
				      filter));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (zlib_zip_filter, filter,
				     zip_filter_class, /* Metaclass */

				     /* Metaclass inits */
				     .generic_open = zzf_open,

				     zlib_zip_filter_ctor,
				     zlib_zip_filter_dtor,
				     NULL, NULL,
				     NULL, NULL);

errcode_t
chop_zlib_zip_filter_init2 (int zlib_compression_level, size_t input_size,
			    chop_malloc_t alloc, chop_realloc_t realloc,
			    chop_free_t free,
			    chop_filter_t *filter)
{
  errcode_t err;
  chop_zlib_zip_filter_t *zfilter;

  zfilter = (chop_zlib_zip_filter_t *)filter;

  err = chop_object_initialize ((chop_object_t *) filter,
				(chop_class_t *) &chop_zlib_zip_filter_class);
  if (err)
    return err;

  input_size = input_size ? input_size : 1024;
  if (alloc)
    zfilter->input_buffer =
      alloc (input_size, (chop_class_t *) &chop_zlib_zip_filter_class);
  else
    zfilter->input_buffer = malloc (input_size);

  if (!zfilter->input_buffer)
    return ENOMEM;

  zfilter->input_buffer_size = input_size;

  if ((alloc != NULL) && (free != NULL))
    {
      zfilter->zstream.zalloc = custom_alloc;
      zfilter->zstream.zfree  = custom_free;
      zfilter->zstream.opaque = zfilter;
      zfilter->malloc          = alloc;
      zfilter->free            = free;
    }

  deflateInit (&zfilter->zstream,
	       (zlib_compression_level >= 0)
	       ? zlib_compression_level : Z_DEFAULT_COMPRESSION);

  zfilter->zstream.next_in = (unsigned char *)zfilter->input_buffer;
  zfilter->zstream.avail_in = 0;

  return 0;
}

errcode_t
chop_zlib_zip_filter_init (int zlib_compression_level, size_t input_size,
			   chop_filter_t *filter)
{
  return (chop_zlib_zip_filter_init2 (zlib_compression_level, input_size,
				      NULL, NULL, NULL,
				      filter));
}


/* The push and pull methods.  */
#define ZIP_TYPE        zlib
#define ZIP_DIRECTION   zip
#define ZIP_BUFFER_TYPE unsigned char
#define ZIP_CUSTOM_ALLOC_ITEM_T uInt

#define ZIP_FLUSH       Z_FINISH
#define ZIP_NO_FLUSH    0
#define ZIP_OK          Z_OK
#define ZIP_NO_PROGRESS Z_BUF_ERROR

#define ZIP_STREAM_ENDED(_zstream, _zret)      ((_zret) == Z_STREAM_END)
#define ZIP_PROCESS(_zstream, _flush)          deflate ((_zstream), (_flush))
#define ZIP_NEED_MORE_INPUT(_zstream, _zret)   (0)
#define ZIP_CANT_PRODUCE_MORE(_zstream, _zret) ((_zret) == Z_BUF_ERROR)
#define ZIP_INPUT_CORRUPTED(_zret)             (0)
#define ZIP_RESET_PROCESSING(_zstream)         deflateReset ((_zstream))

#include "filter-zip-push-pull.c"

