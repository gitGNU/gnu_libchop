#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <zlib.h>


/* Define `chop_zlib_unzip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (zlib_unzip_filter, filter,
				      unzip_filter_class,

				      chop_malloc_t malloc;
				      chop_free_t free;

				      size_t  block_count_100k;
				      char *input_buffer;
				      size_t input_buffer_size;
				      z_stream zstream;);



static errcode_t
chop_zlib_unzip_push (chop_filter_t *filter,
		    const char *buffer, size_t size, size_t *pushed);

static errcode_t
chop_zlib_unzip_pull (chop_filter_t *filter, int flush,
		      char *buffer, size_t size, size_t *pulled);

static void *
custom_alloc (voidp opaque, uInt items, uInt size);

static void
custom_free (voidpf opaque, voidp address);


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
  zfilter->malloc = NULL;
  zfilter->free = NULL;
  return chop_log_init ("zlib-unzip-filter", &zfilter->filter.log);
}

static void
zlib_unzip_filter_dtor (chop_object_t *object)
{
  chop_zlib_unzip_filter_t *zfilter;
  zfilter = (chop_zlib_unzip_filter_t *)object;

  inflateEnd (&zfilter->zstream);
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
zuf_open (size_t input_size,
	  chop_malloc_t malloc, chop_realloc_t realloc, chop_free_t free,
	  chop_filter_t *filter)
{
  return (chop_zlib_unzip_filter_init2 (input_size,
					malloc, realloc, free,
					filter));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (zlib_unzip_filter, filter,
				     unzip_filter_class, /* Metaclass */

				     /* Metaclass inits */
				     .generic_open = zuf_open,

				     zlib_unzip_filter_ctor,
				     zlib_unzip_filter_dtor,
				     NULL, NULL,
				     NULL, NULL);


errcode_t
chop_zlib_unzip_filter_init2 (size_t input_size,
			      chop_malloc_t alloc, chop_realloc_t realloc,
			      chop_free_t free,
			      chop_filter_t *filter)
{
  errcode_t err;
  chop_zlib_unzip_filter_t *zfilter;

  zfilter = (chop_zlib_unzip_filter_t *)filter;

  err =
    chop_object_initialize ((chop_object_t *)filter,
			    (chop_class_t *) &chop_zlib_unzip_filter_class);
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
      zfilter->malloc         = alloc;
      zfilter->free           = free;
    }

  inflateInit (&zfilter->zstream);

  zfilter->zstream.next_in = (unsigned char *)zfilter->input_buffer;
  zfilter->zstream.avail_in = 0;

  return 0;
}

errcode_t
chop_zlib_unzip_filter_init (size_t input_size, chop_filter_t *filter)
{
  return (chop_zlib_unzip_filter_init2 (input_size,
					NULL, NULL, NULL,
					filter));
}


/* The push and pull methods.  */
#define ZIP_TYPE        zlib
#define ZIP_DIRECTION   unzip
#define ZIP_BUFFER_TYPE unsigned char
#define ZIP_CUSTOM_ALLOC_ITEM_T uInt

#define ZIP_FLUSH       Z_SYNC_FLUSH
#define ZIP_NO_FLUSH    0
#define ZIP_OK          Z_OK
#define ZIP_NO_PROGRESS Z_BUF_ERROR

#define ZIP_STREAM_ENDED(_zstream, _zret)      ((_zret) == Z_STREAM_END)
#define ZIP_PROCESS(_zstream, _flush)  inflate ((_zstream), (_flush))
#define ZIP_NEED_MORE_INPUT(_zstream, _zret)   ((_zret) == Z_BUF_ERROR)
#define ZIP_CANT_PRODUCE_MORE(_zstream, _zret) ((_zret) == Z_STREAM_END)
#define ZIP_INPUT_CORRUPTED(_zret)             ((_zret) == Z_DATA_ERROR)
#define ZIP_RESET_PROCESSING(_zstream)         inflateReset ((_zstream))

#include "filter-zip-push-pull.c"

