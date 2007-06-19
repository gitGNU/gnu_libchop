#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>
#include <chop/logs.h>

#include <errno.h>

#include <lzo1x.h>


/* Define `chop_lzo_zip_filter_t' which inherits from `chop_filter_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (lzo_zip_filter, filter,
				      zip_filter_class,

				      bytep   work_mem;
				      char   *input_buffer;
				      size_t  input_buffer_size;
				      size_t  avail_in;
				      char   *output_buffer;
				      size_t  output_buffer_size;
				      size_t  avail_out;
				      size_t  out_offset;
				      bz_stream zstream;);


/* LZO global initialization.  */
errcode_t
chop_initialize_lzo (void)
{
  static int initialized = 0;
  errcode_t err = 0;

  if (!initialized)
    {
      err = lzo_init ();
      if (err != LZO_E_OK)
	err = CHOP_FILTER_ERROR;
      else
	err = 0;
    }

  return err;
}


static errcode_t
chop_lzo_zip_pull (chop_filter_t *filter, int flush,
		   char *buffer, size_t size, size_t *pulled)
{
  errcode_t err = 0;
  size_t uncompressed_size = 0;
  chop_lzo_zip_filter_t *zfilter;

  zfilter = (chop_lzo_zip_filter_t *) filter;

  *pulled = 0;
  while ((*pulled < size) && (err == 0))
    {
      if (zfilter->avail_out > 0)
	{
	  /* Push already compressed data.  */
	  size_t amount;

	  if (zfilter->out_offset == 0)
	    {
	      /* We first need to write `avail_out' to allow for
		 recoverability upon decompression.  */
	      if (size < 8)
		{
		  if (*pulled > 0)
		    break;
		  else
		    err = CHOP_INVALID_ARG;
		}
	      else
		{
		  /* Encode both the uncompressed and compressed size to ease
		     decoding.  */
		  /* FIXME: Check endianness and 32-bit-ness.  */
		  unsigned in32, out32;

		  in32  = uncompressed_size;
		  out32 = zfilter->avail_out;
		  memcpy (buffer, &out32, 4);
		  memcpy (buffer + 4, &in32, 4);
		  buffer  += 8;
		  size    -= 8;
		  *pulled += 8;
		}
	    }

	  amount = (zfilter->avail_out > size) ? size : zfilter->avail_out;
	  memcpy (buffer, zfilter->buffer + zfilter->out_offset,
		  amount);
	  *pulled             += amount;
	  zfilter->out_offset += amount;
	  zfilter->avail_out  -= amount;
	  if (zfilter->avail_out == 0)
	    zfilter->out_offset = 0;
	}
      else
	{
	  if ((zfilter->avail_in < zfilter->input_buffer_size)
	      && (!flush))
	    {
	      /* Ask for more input data.  */
	      size_t howmuch;

	      howmuch = zfilter->input_buffer_size - zfilter->avail_in;
	      chop_log_printf (&filter->log,
			       "filter is empty, input fault "
			       "(requesting %u bytes)",
			       howmuch);

	      err = chop_filter_handle_input_fault (filter, howmuch);
	      if (err)
		{
		  chop_log_printf (&filter->log,
				   "input fault unhandled: %s",
				   error_message (err));
		  if (err != CHOP_FILTER_UNHANDLED_FAULT)
		    break;

		  err = CHOP_FILTER_EMPTY;
		  break;
		}

	      continue;
	    }


	  chop_log_printf (&filter->log,
			   "pull: processing stream (input: %u, output: %u, "
			   "flush: %s)",
			   zfilter->avail_in, zfilter->avail_out,
			   flush ? "yes" : "no");

	  uncompressed_size = zfilter->avail_in;
	  err = lzo1x_1_compress (zfilter->input_buffer, zfilter->avail_in,
				  zfilter->output_buffer, &zfilter->avail_out,
				  zfilter->work_mem)

	  if (err != LZO_E_OK)
	    err = CHOP_FILTER_ERROR;
	  else
	    zfilter->avail_in = 0;
	}
    }

  return err;
}



static errcode_t lzo_zip_filter_ctor (chop_object_t *object,
				      const chop_class_t *class);
static void lzo_zip_filter_dtor (chop_object_t *object);


static errcode_t
lzf_open (int compression_level, size_t input_size,
	  chop_filter_t *filter)
{
  size_t block_count_100k;

  /* FIXME: Handle COMPRESSION_LEVEL.  */

  return (chop_lzo_zip_filter_init (input_size, filter));
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (lzo_zip_filter, filter,
				     zip_filter_class, /* Metaclass */

				     /* Metaclass inits.  */
				     .generic_open = bzf_open,

				     lzo_zip_filter_ctor,
				     lzo_zip_filter_dtor,
				     NULL, NULL, /* No copy, equalp */
				     NULL, NULL  /* No serial, deserial */);

errcode_t
chop_lzo_zip_filter_init (size_t input_size, chop_filter_t *filter)
{
  errcode_t err;
  chop_lzo_zip_filter_t *zfilter;

  zfilter = (chop_lzo_zip_filter_t *) filter;

  err = chop_initialize_lzo ();
  if (err)
    return err;

  err =
    chop_object_initialize ((chop_object_t *) filter,
			    (chop_class_t *) &chop_lzo_zip_filter_class);
  if (err)
    return err;

  input_size = input_size ? input_size : 1024;
  zfilter->input_buffer = malloc (input_size);
  if (!zfilter->input_buffer)
    goto mem_err;

  zfilter->input_buffer_size = input_size;

  /* The `LZO.TXT' file reads:

       When dealing with uncompressible data, LZO expands the input
       block by a maximum of 16 bytes per 1024 bytes input.

     Thus, we compute that size and add a few bytes for safety.  */
  zfilter->output_buffer_size = input_size + (input_size >> 6) + 100;
  zfilter->output_buffer = malloc (zfilter->output_buffer_size);
  if (!zfilter->output_buffer)
    goto mem_err;

  zfilter->work_mem = malloc (LZO1X_1_MEM_COMPRESS);
  if (!zfilter->work_mem)
    goto mem_err;

  return 0;

 mem_err:
  chop_object_destroy ((chop_object_t *) zfilter);
  return ENOMEM;
}


/* The push and pull methods.  */
#define ZIP_DIRECTION   zip


#include "filter-lzo-common.c"

/* arch-tag: 5f61b3a0-a19a-4dbc-aaf9-bd150e1ac262
 */
