/* Support for the common functionality defined in `core-spec.scm'.  */

#ifndef __CHOP_CORE_SUPPORT_H__
#define __CHOP_CORE_SUPPORT_H__

#include <chop/chop.h>
#include <chop/objects.h>

#include <libguile.h>
#include <stdlib.h>


_CHOP_BEGIN_DECLS

/* Metaclass for classes that are ``hybrid'', in the sense that their
   instances contain references to Scheme object (i.e. they have fields whose
   type is `SCM').  Such classes need to be able to mark the Scheme objects
   they embed during the GC mark phase.  */
CHOP_DECLARE_RT_CLASS (hybrid_scheme_class, class,

		       /* The class-specific method to mark the given chop
			  object.  */
		       SCM (* mark) (chop_object_t *););


/* Initialize G-Wrap/Guile support for `chop_object_t' objects.  */
extern void gwrap_chop_object_support_init (void);

/* The function that destroys WCP, a wrapped `chop_object_t' pointer, by
   calling `chop_object_destroy ()' and then freeing the object itself.  */
extern size_t gwrap_chop_object_cleanup (SCM wcp);

/* The function that gets called to mark WCP, a wrapped C pointer to a
   `chop_object_t'.  */
extern SCM gwrap_chop_object_mark (SCM wcp);


_CHOP_END_DECLS


/* arch-tag: 30d7f062-6749-4261-ae04-f90ef559c1c9
 */

#endif
