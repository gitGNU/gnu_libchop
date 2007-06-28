/* Contructors with a functional style that perform memory allocation by
   themselves.

   arch-tag: 55b0ce3e-83aa-4802-8311-b42b2d32395a
   */

#include <chop/chop-config.h>

#include <stdlib.h>
#include <errno.h>
#include <assert.h>

#ifdef DEBUG
# include <stdio.h>
#endif


/* Custom allocator for filters.  */

/* This custom allocators are very important because they allow Guile's GC to
   know how much memory has been allocated, beside memory allocated by
   itself.  Since zip filters consume a *lot* of memory (e.g., zlib allocates
   64K buffers, and bzip2 uses 400K buffers by default), it is important to
   let Guile know that we *are* using a lot of memory, and that it should GC
   more often.  */


/* We end up rolling our own allocator data structure to keep track of buffer
   sizes.  */
typedef struct
{
  size_t size;
  char   data[0];
} alloc_header_t;


static void *
chop_scm_malloc (size_t size, const chop_class_t *klass)
{
  alloc_header_t *header;

  header = scm_gc_malloc (size + sizeof (size_t),
			  chop_class_name (klass));
  header->size = size;

#ifdef DEBUG
  printf ("allocated %u bytes at %p [header %p]\n",
	  size, &header->data[0], header);
#endif

  return (&header->data[0]);
}

static void *
chop_scm_realloc (void *data, size_t size,
		  const chop_class_t *klass)
{
  alloc_header_t *header;

  header = (alloc_header_t *) (data - sizeof (size_t));

  header = scm_gc_realloc (header,
			   header->size + sizeof (size_t),
			   size + sizeof (size_t),
			   chop_class_name (klass));
#ifdef DEBUG
  printf ("reallocated %u -> %u bytes at %p [header %p]\n",
	  header->size, size, data, header);
#endif

  header->size = size;

  return (&header->data[0]);
}

static void
chop_scm_free (void *data, const chop_class_t *klass)
{
  alloc_header_t *header;

  header = (alloc_header_t *) (data - sizeof (size_t));

#ifdef DEBUG
  printf ("freeing %u bytes at %p [header %p]\n",
	  header->size, data, header);
#endif

  scm_gc_free (header, header->size,
	       chop_class_name (klass));
}



/* Constructors.  */

static inline errcode_t
chop_zlib_zip_filter_init_alloc (int compression, size_t input_size,
				 chop_filter_t **filter)
{
  errcode_t err;

  *filter =
    gwrap_chop_malloc ((chop_class_t *) &chop_zlib_zip_filter_class);

  err = chop_zlib_zip_filter_init2 (compression, input_size,
				    chop_scm_malloc, chop_scm_realloc,
				    chop_scm_free,
				    *filter);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) *filter,
	 (chop_class_t *) &chop_zlib_zip_filter_class);
      *filter = NULL;
    }

  return err;
}

static inline errcode_t
chop_zlib_unzip_filter_init_alloc (size_t input_size,
				   chop_filter_t **filter)
{
  errcode_t err;

  *filter =
    gwrap_chop_malloc ((chop_class_t *) &chop_zlib_unzip_filter_class);

  err = chop_zlib_unzip_filter_init2 (input_size,
				      chop_scm_malloc, chop_scm_realloc,
				      chop_scm_free,
				      *filter);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) *filter,
	 (chop_class_t *) &chop_zlib_unzip_filter_class);
      *filter = NULL;
    }

  return err;
}


/* Generic zip/unzip filters.  */

static inline errcode_t
chop_generic_zip_filter_open_alloc (const char *class_nickname,
				    int compression_level, size_t input_size,
				    chop_filter_t **filter)
{
  errcode_t err;
  char *class_name;
  chop_zip_filter_class_t *klass;

  class_name = (char *) alloca (strlen (class_nickname) + 20);
  strcpy (class_name, class_nickname);
  strcat (class_name, "_zip_filter");

  klass = (chop_zip_filter_class_t *) chop_class_lookup (class_name);
  if ((!klass) ||
      (!chop_object_is_a ((chop_object_t *) klass,
			  &chop_zip_filter_class_class)))
    err = CHOP_ERR_NOT_FOUND;
  else
    {
      *filter = gwrap_chop_malloc ((chop_class_t *) klass);
      err = chop_zip_filter_generic_open2 (klass, compression_level,
					   input_size,
					   chop_scm_malloc,
					   chop_scm_realloc,
					   chop_scm_free,
					   *filter);
      if (err)
	gwrap_chop_free_uninitialized ((chop_object_t *) *filter,
				       (chop_class_t *) klass);
    }

  return err;
}

static inline errcode_t
chop_generic_unzip_filter_open_alloc (const char *class_nickname,
				      size_t input_size,
				      chop_filter_t **filter)
{
  errcode_t err;
  char *class_name;
  chop_unzip_filter_class_t *klass;

  class_name = (char *) alloca (strlen (class_nickname) + 20);
  strcpy (class_name, class_nickname);
  strcat (class_name, "_unzip_filter");

  klass = (chop_unzip_filter_class_t *) chop_class_lookup (class_name);
  if ((!klass) ||
      (!chop_object_is_a ((chop_object_t *) klass,
			  &chop_unzip_filter_class_class)))
    err = CHOP_ERR_NOT_FOUND;
  else
    {
      *filter = gwrap_chop_malloc ((chop_class_t *) klass);
      err = chop_unzip_filter_generic_open2 (klass, input_size,
					     chop_scm_malloc,
					     chop_scm_realloc,
					     chop_scm_free,
					     *filter);
      if (err)
	gwrap_chop_free_uninitialized ((chop_object_t *) *filter,
				       (chop_class_t *) klass);
    }

  return err;
}
