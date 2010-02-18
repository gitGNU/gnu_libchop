/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <zlib.h>


/* Define `chop_zlib_zip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (zlib_zip_filter, filter,
				      zip_filter_class,

				      char *input_buffer;
				      size_t input_buffer_size;
				      z_stream zstream;);



static chop_error_t
chop_zlib_zip_push (chop_filter_t *filter,
		    const char *buffer, size_t size, size_t *pushed);

static chop_error_t
chop_zlib_zip_pull (chop_filter_t *filter, int flush,
		    char *buffer, size_t size, size_t *pulled);

static void *
custom_alloc (voidp opaque, uInt items, uInt size);

static void
custom_free (voidpf opaque, voidp address);


static chop_error_t
zlib_zip_filter_ctor (chop_object_t *object,
		      const chop_class_t *class)
{
  chop_zlib_zip_filter_t *zfilter;
  zfilter = (chop_zlib_zip_filter_t *)object;

  zfilter->filter.push = chop_zlib_zip_push;
  zfilter->filter.pull = chop_zlib_zip_pull;
  if (chop_internal_malloc)
    {
      zfilter->zstream.zalloc = custom_alloc;
      zfilter->zstream.zfree  = custom_free;
    }
  else
    {
      zfilter->zstream.zalloc = Z_NULL;
      zfilter->zstream.zfree = Z_NULL;
    }
  zfilter->zstream.opaque = Z_NULL;
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
    chop_free (zfilter->input_buffer,
	       (chop_class_t *) &chop_zlib_zip_filter_class);
  zfilter->input_buffer = NULL;
  zfilter->input_buffer_size = 0;

  chop_object_destroy ((chop_object_t *)&zfilter->filter.log);
}

static chop_error_t
zzf_open (int compression_level, size_t input_size,
	  chop_filter_t *filter)
{
  return (chop_zlib_zip_filter_init (compression_level, input_size,
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

chop_error_t
chop_zlib_zip_filter_init (int zlib_compression_level, size_t input_size,
			   chop_filter_t *filter)
{
  chop_error_t err;
  chop_zlib_zip_filter_t *zfilter;

  zfilter = (chop_zlib_zip_filter_t *)filter;

  err = chop_object_initialize ((chop_object_t *) filter,
				(chop_class_t *) &chop_zlib_zip_filter_class);
  if (err)
    return err;

  input_size = input_size ? input_size : 1024;
  zfilter->input_buffer =
    chop_malloc (input_size,
		 (chop_class_t *) &chop_zlib_zip_filter_class);
  if (!zfilter->input_buffer)
    return ENOMEM;

  zfilter->input_buffer_size = input_size;

  deflateInit (&zfilter->zstream,
	       (zlib_compression_level >= 0)
	       ? zlib_compression_level : Z_DEFAULT_COMPRESSION);

  zfilter->zstream.next_in = (unsigned char *)zfilter->input_buffer;
  zfilter->zstream.avail_in = 0;

  return 0;
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

