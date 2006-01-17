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



size_t
gwrap_chop_object_cleanup (void *wcp)
{
  chop_object_t *object;
  const chop_class_t *class;

  assert (wcp);
  object = (chop_object_t *)wcp;

#ifdef DEBUG
  fprintf (stderr, "%s: freeing object @%p [SCM: %p]\n",
	   __FUNCTION__, object, (void *)wcp);
#endif

  class = chop_object_get_class (object);
#ifdef DEBUG
  fprintf (stderr, "%s: freeing %s@%p\n",
	   __FUNCTION__,
	   chop_class_name (class),
	   object);
#endif
  chop_object_destroy (object);

  /* We assume this was allocated with the libc standard functions.  */
  free (object);

  return (chop_class_instance_size (class));
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

/* arch-tag: fb93cde4-28b3-485e-a0fd-dd2df2e1c5c6
 */
