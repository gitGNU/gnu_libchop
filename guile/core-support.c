/* Support for the common functionality defined in `core-spec.scm'.  */

#include <libguile.h>
#include <g-wrap/guile-runtime.h>

#include <chop/chop.h>
#include <chop/objects.h>

#include "core-support.h"

#include <stdlib.h>
#ifdef DEBUG
# include <stdio.h>
#endif
#include <assert.h>


CHOP_DEFINE_RT_CLASS (hybrid_scheme_class, class,
		      NULL, NULL,  /* No ctor/dtor */
		      NULL, NULL,  /* No copy/equalp */
		      NULL, NULL   /* No serial/dserial */);



int
gwrap_chop_object_equal (void *o1, void *o2)
{
  return chop_object_equal ((chop_object_t *)o1, (chop_object_t *)o2);
}

size_t
gwrap_chop_object_cleanup (void *wcp)
{
  chop_object_t *object;
  const chop_class_t *class;

  assert (wcp);
  object = (chop_object_t *)wcp;

#ifdef DEBUG
  fprintf (stderr, "%s: freeing object @%p\n",
	   __FUNCTION__, object);
#endif

  class = chop_object_get_class (object);
#ifdef DEBUG
  fprintf (stderr, "%s: freeing %s@%p\n",
	   __FUNCTION__,
	   chop_class_name (class),
	   object);
#endif

  /* We assume that it was allocated with `gwrap_chop_malloc ()'.  */
  gwrap_chop_free (object);

  return (chop_class_instance_size (class)); /* FIXME: Return 0? */
}

SCM
gwrap_chop_object_mark (SCM wcp)
{
  SCM ret = SCM_BOOL_F;
  chop_object_t *object;
  const chop_class_t *class;

  object = (chop_object_t *)gw_wcp_get_ptr (wcp);
  class = chop_object_get_class (object);

  if (chop_object_is_a ((chop_object_t *)class,
			&chop_hybrid_scheme_class_class))
    {
      /* CLASS is a ``hybrid Scheme class'', meaning that OBJECT potentially
	 referenced SCM objects which need to be marked.  */
      const chop_hybrid_scheme_class_t *hybrid_class;

#ifdef DEBUG
      fprintf (stderr, "%s: marking hybrid object %s@%p [SCM %p]\n",
	       __FUNCTION__, chop_class_name (class), object, (void *)wcp);
#endif

      hybrid_class = (const chop_hybrid_scheme_class_t *)class;
      if (hybrid_class->mark)
	ret = hybrid_class->mark (object);
    }

  return ret;
}


/* Custom allocator for libchop.  */

/* This custom allocators are very important because they allow Guile's GC to
   know how much memory has been allocated, beside memory allocated by
   itself.  For instance, zip filters consume a *lot* of memory (e.g., zlib
   allocates 64K buffers, and bzip2 uses 400K buffers by default), and it is
   important to let Guile know that we *are* using a lot of memory, and that
   it should GC more often.  */


/* We end up rolling our own allocator data structure to keep track of buffer
   sizes.  */
typedef struct
{
  size_t size;
  char   data[0];
} alloc_header_t;


void *
chop_scm_malloc (size_t size, const chop_class_t *klass)
{
  alloc_header_t *header;
  const char *what;

  what = klass ? chop_class_name (klass) : "<chop-internal>";

  header = scm_gc_malloc (size + sizeof (size_t), what);
  header->size = size;

#ifdef DEBUG
  printf ("allocated %u bytes at %p [header %p], class `%s'\n",
	  size, &header->data[0], header, what);
#endif

  return (&header->data[0]);
}

void *
chop_scm_realloc (void *data, size_t size,
		  const chop_class_t *klass)
{
  alloc_header_t *header;
  const char *what;

  what = klass ? chop_class_name (klass) : "<chop-internal>";
  header = (alloc_header_t *) (data - sizeof (size_t));

  header = scm_gc_realloc (header,
			   header->size + sizeof (size_t),
			   size + sizeof (size_t),
			   what);
#ifdef DEBUG
  printf ("reallocated %u -> %u bytes at %p [header %p], class `%s'\n",
	  header->size, size, data, header, what);
#endif

  header->size = size;

  return (&header->data[0]);
}

void
chop_scm_free (void *data, const chop_class_t *klass)
{
  alloc_header_t *header;
  const char *what;

  what = klass ? chop_class_name (klass) : "<chop-internal>";
  header = (alloc_header_t *) (data - sizeof (size_t));

#ifdef DEBUG
  printf ("freeing %u bytes at %p [header %p], class `%s'\n",
	  header->size, data, header, what);
#endif

  scm_gc_free (header, header->size, what);
}


/* arch-tag: fb93cde4-28b3-485e-a0fd-dd2df2e1c5c6
 */
