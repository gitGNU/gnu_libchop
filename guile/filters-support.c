/* Contructors with a functional style that perform memory allocation by
   themselves.

   arch-tag: 55b0ce3e-83aa-4802-8311-b42b2d32395a
   */

#include <stdlib.h>
#include <errno.h>
#include <assert.h>


static __inline__ errcode_t
chop_zlib_zip_filter_init_alloc (int compression, size_t input_size,
				 chop_filter_t **filter)
{
  errcode_t err;

  *filter =
    scm_malloc (chop_class_instance_size (&chop_zlib_zip_filter_class));

  err = chop_zlib_zip_filter_init (compression, input_size, *filter);
  if (err)
    {
      free (*filter);
      *filter = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_zlib_unzip_filter_init_alloc (size_t input_size,
				   chop_filter_t **filter)
{
  errcode_t err;

  *filter =
    scm_malloc (chop_class_instance_size (&chop_zlib_zip_filter_class));

  err = chop_zlib_unzip_filter_init (input_size, *filter);
  if (err)
    {
      free (*filter);
      *filter = NULL;
    }

  return err;
}

static __inline__ void
chop_filter_dealloc (chop_filter_t *filter)
{
  chop_object_destroy ((chop_object_t *)filter);
  free (filter);
}


static __inline__ errcode_t
chop_filtered_store_open_alloc (chop_filter_t *input, chop_filter_t *output,
				chop_block_store_t *backend,
				chop_block_store_t **store)
{
  errcode_t err;

  *store =
    scm_malloc (chop_class_instance_size (&chop_filtered_block_store_class));

  err = chop_filtered_store_open (input, output, backend, *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}
