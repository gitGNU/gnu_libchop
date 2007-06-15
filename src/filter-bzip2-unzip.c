#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <bzlib.h>


/* Define `chop_bzip2_unzip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (bzip2_unzip_filter, filter,
				      unzip_filter_class,

				      char *input_buffer;
				      size_t input_buffer_size;
				      bz_stream zstream;);

/* Bzip2 debugging.  */
#define CHOP_BZIP2_VERBOSITY  0



static errcode_t
chop_bzip2_unzip_push (chop_filter_t *filter,
		       const char *buffer, size_t size, size_t *pushed);

static errcode_t
chop_bzip2_unzip_pull (chop_filter_t *filter, int flush,
		      char *buffer, size_t size, size_t *pulled);

static errcode_t
bzip2_unzip_filter_ctor (chop_object_t *object,
			 const chop_class_t *class)
{
  chop_bzip2_unzip_filter_t *zfilter;
  zfilter = (chop_bzip2_unzip_filter_t *)object;

  zfilter->filter.push = chop_bzip2_unzip_push;
  zfilter->filter.pull = chop_bzip2_unzip_pull;
  zfilter->zstream.bzalloc = NULL;
  zfilter->zstream.bzfree = NULL;
  zfilter->zstream.opaque = NULL;
  return chop_log_init ("bzip2-unzip-filter", &zfilter->filter.log);
}

static void
bzip2_unzip_filter_dtor (chop_object_t *object)
{
  chop_bzip2_unzip_filter_t *zfilter;
  zfilter = (chop_bzip2_unzip_filter_t *)object;

  BZ2_bzDecompressEnd (&zfilter->zstream);
  zfilter->zstream.bzalloc = NULL;
  zfilter->zstream.bzfree = NULL;
  zfilter->zstream.opaque = NULL;

  if (zfilter->input_buffer)
    free (zfilter->input_buffer);
  zfilter->input_buffer = NULL;
  zfilter->input_buffer_size = 0;

  chop_object_destroy ((chop_object_t *)&zfilter->filter.log);
}


static errcode_t
buf_open (size_t input_size, chop_filter_t *filter)
{
  return (chop_bzip2_unzip_filter_init (input_size, filter));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (bzip2_unzip_filter, filter,
				     unzip_filter_class, /* Metaclass */

				     /* Metaclass inits */
				     .generic_open = buf_open,

				     bzip2_unzip_filter_ctor,
				     bzip2_unzip_filter_dtor,
				     NULL, NULL,
				     NULL, NULL);


errcode_t
chop_bzip2_unzip_filter_init (size_t input_size,
			      chop_filter_t *filter)
{
  errcode_t err;
  chop_bzip2_unzip_filter_t *zfilter;

  zfilter = (chop_bzip2_unzip_filter_t *)filter;

  err =
    chop_object_initialize ((chop_object_t *) filter,
			    (chop_class_t *) &chop_bzip2_unzip_filter_class);
  if (err)
    return err;

  input_size = input_size ? input_size : 1024;
  zfilter->input_buffer = malloc (input_size);
  if (!zfilter->input_buffer)
    return ENOMEM;

  zfilter->input_buffer_size = input_size;

  err = BZ2_bzDecompressInit (&zfilter->zstream, CHOP_BZIP2_VERBOSITY,
			      0 /* XXX: what's that? */);
  if (err)
    {
      err = CHOP_FILTER_ERROR;
      chop_object_destroy ((chop_object_t *) zfilter);
    }
  else
    {
      zfilter->zstream.next_in = (char *) zfilter->input_buffer;
      zfilter->zstream.avail_in = 0;
    }

  return err;
}


/* The push and pull methods.  */
#define ZIP_TYPE        bzip2
#define ZIP_DIRECTION   unzip
#define ZIP_BUFFER_TYPE char

#define ZIP_FLUSH       BZ_FLUSH
#define ZIP_NO_FLUSH    BZ_RUN
#define ZIP_OK          BZ_OK
#define ZIP_NO_PROGRESS BZ_OUTBUFF_FULL


/* (See explanation in `filter-bzip2-zip.c'.)  */
#define ZIP_STREAM_ENDED(_zstream, _zret)			\
 (((_zret) == BZ_STREAM_END) || ((_zret) == BZ_SEQUENCE_ERROR))

#define ZIP_PROCESS(_zstream, _flush)          BZ2_bzDecompress (_zstream)
#define ZIP_NEED_MORE_INPUT(_zstream, _zret)   ((_zret) == BZ_DATA_ERROR)
#define ZIP_CANT_PRODUCE_MORE(_zstream, _zret) ((_zret) == BZ_STREAM_END)
#define ZIP_INPUT_CORRUPTED(_zret)             ((_zret) == BZ_DATA_ERROR)
#define ZIP_RESET_PROCESSING(_zstream)         BZ2_bzDecompressEnd (_zstream)

#include "filter-zip-push-pull.c"

/* arch-tag: 9938a488-7f53-4834-9f04-99824cb48933
 */
