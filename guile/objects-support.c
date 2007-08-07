/* Support code for the object API.  */

#include <chop/chop.h>
#include <chop/objects.h>

#include <g-wrap/guile-runtime.h>

#include "core-support.h"

static inline int
chop_scm_object_is_a (SCM obj, const chop_class_t *c_class)
{
  if (gw_wcp_p (obj))
    {
      const chop_object_t *c_obj;

      c_obj = (chop_object_t *) gw_wcp_get_ptr (obj);
      return (chop_object_is_a (c_obj, c_class));
    }

  return 0;
}

/* arch-tag: 81116c21-55dd-4ec4-b4bd-0db91964ff29
 */
