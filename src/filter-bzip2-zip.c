#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <bzlib.h>


/* Define `chop_bzip2_zip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (bzip2_zip_filter, filter,
				      zip_filter_class,

				      size_t  block_count_100k;
				      size_t  work_factor;
				      char   *input_buffer;
				      size_t  input_buffer_size;
				      bz_stream zstream;);

/* Bzip2 debugging.  */
#define CHOP_BZIP2_VERBOSITY  0



static errcode_t
chop_bzip2_zip_push (chop_filter_t *filter,
		    const char *buffer, size_t size, size_t *pushed);

static errcode_t
chop_bzip2_zip_pull (chop_filter_t *filter, int flush,
		    char *buffer, size_t size, size_t *pulled);

static errcode_t
bzip2_zip_filter_ctor (chop_object_t *object,
		      const chop_class_t *class)
{
  chop_bzip2_zip_filter_t *zfilter;
  zfilter = (chop_bzip2_zip_filter_t *)object;

  zfilter->filter.push = chop_bzip2_zip_push;
  zfilter->filter.pull = chop_bzip2_zip_pull;
  zfilter->zstream.bzalloc = NULL;
  zfilter->zstream.bzfree = NULL;
  zfilter->zstream.opaque = NULL;
  zfilter->work_factor = 0;
  zfilter->block_count_100k = 0;
  zfilter->input_buffer_size = 0;
  zfilter->input_buffer = NULL;

  return chop_log_init ("bzip2-zip-filter", &zfilter->filter.log);
}

static void
bzip2_zip_filter_dtor (chop_object_t *object)
{
  chop_bzip2_zip_filter_t *zfilter;
  zfilter = (chop_bzip2_zip_filter_t *)object;

  BZ2_bzCompressEnd (&zfilter->zstream);
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
bzf_open (int compression_level, size_t input_size,
	  chop_filter_t *filter)
{
  size_t block_count_100k;

  if (compression_level >= 0)
    block_count_100k = (size_t) compression_level;
  else
    block_count_100k = 1; /* default "compression level" */

  return (chop_bzip2_zip_filter_init (block_count_100k,
				      0 /* default work factor */,
				      input_size, filter));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (bzip2_zip_filter, filter,
				     zip_filter_class, /* Metaclass */

				     /* Metaclass inits.  */
				     .generic_open = bzf_open,

				     bzip2_zip_filter_ctor,
				     bzip2_zip_filter_dtor,
				     NULL, NULL, /* No copy, equalp */
				     NULL, NULL  /* No serial, deserial */);

errcode_t
chop_bzip2_zip_filter_init (size_t block_count_100k, size_t work_factor,
			    size_t input_size, chop_filter_t *filter)
{
  errcode_t err;
  chop_bzip2_zip_filter_t *zfilter;

  zfilter = (chop_bzip2_zip_filter_t *)filter;

  err =
    chop_object_initialize ((chop_object_t *) filter,
			    (chop_class_t *) &chop_bzip2_zip_filter_class);
  if (err)
    return err;

  input_size = input_size ? input_size : 1024;
  zfilter->input_buffer = malloc (input_size);
  if (!zfilter->input_buffer)
    return ENOMEM;

  zfilter->input_buffer_size = input_size;
  zfilter->work_factor = work_factor;
  zfilter->block_count_100k = block_count_100k;

  err = BZ2_bzCompressInit (&zfilter->zstream,
			    block_count_100k,
			    CHOP_BZIP2_VERBOSITY,
			    work_factor);
  switch (err)
    {
    case BZ_PARAM_ERROR:
      err = CHOP_INVALID_ARG;
      break;

    case BZ_OK:
      err = 0;
      zfilter->zstream.next_in = (char *) zfilter->input_buffer;
      zfilter->zstream.avail_in = 0;
      break;

    default:
      err = CHOP_FILTER_ERROR;
    }

  if (err)
    chop_object_destroy ((chop_object_t *) zfilter);

  return err;
}


/* The push and pull methods.  */
#define ZIP_TYPE        bzip2
#define ZIP_DIRECTION   zip
#define ZIP_BUFFER_TYPE char

#define ZIP_FLUSH       BZ_FINISH
#define ZIP_NO_FLUSH    BZ_RUN
#define ZIP_OK          BZ_OK
#define ZIP_NO_PROGRESS BZ_OUTBUFF_FULL

static inline int
do_zip_process (bz_stream *zstream, int action)
{
  int err;

  err = BZ2_bzCompress (zstream, action);
  if (action == BZ_FINISH)
    {
      switch (err)
	{
	case BZ_FINISH_OK:
	/* case BZ_FINISHING: */
	  err = BZ_OK;
	  break;
	}
    }
  else if (action == BZ_RUN)
    {
      if (err == BZ_RUN_OK)
	err = BZ_OK;
    }

  return err;
}

/* Dectecting stream end.  In the `pull' method, we only want to return
   `CHOP_FILTER_EMPTY' when zero bytes were pulled.  However, when we get
   `BZ_STREAM_END' we may already have `*pulled != 0'; unfortunately, on the
   next call, we no longer get `BZ_STREAM_END': instead we get
   `BZ_SEQUENCE_ERROR', indicating that we shouldn't be doing that.  Zlib
   behaves differently as it keeps returning `Z_STREAM_END'.  */
#define ZIP_STREAM_ENDED(_zstream, _zret)			\
 (((_zret) == BZ_STREAM_END) || ((_zret) == BZ_SEQUENCE_ERROR))

#define ZIP_PROCESS(_zstream, _flush) do_zip_process ((_zstream), (_flush))
#define ZIP_NEED_MORE_INPUT(_zstream, _zret)   (0)
#define ZIP_CANT_PRODUCE_MORE(_zstream, _zret) ((_zret) == BZ_OUTBUFF_FULL)
#define ZIP_INPUT_CORRUPTED(_zret)             ((_zret) == BZ_DATA_ERROR)
#define ZIP_RESET_PROCESSING(_zstream)					\
  BZ2_bzCompressEnd (_zstream);						\
  BZ2_bzCompressInit ((_zstream), zfilter->block_count_100k,		\
		      CHOP_BZIP2_VERBOSITY, zfilter->work_factor)

#include "filter-zip-push-pull.c"

/* arch-tag: 7a36f1d0-d3b2-40a8-a38b-c83a3e4882cb
 */
