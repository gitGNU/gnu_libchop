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

/* Support for the common functionality defined in `core-spec.scm'.  */

#ifndef __CHOP_CORE_SUPPORT_H__
#define __CHOP_CORE_SUPPORT_H__

#include <chop/chop-config.h>
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



/* Custom allocator.  */

extern void *chop_scm_malloc (size_t size, const chop_class_t *klass);
extern void *chop_scm_realloc (void *data, size_t size,
			       const chop_class_t *klass);
extern void  chop_scm_free (void *data, const chop_class_t *klass);



/* This function calls `chop_object_equal ()' on O1 and O2.  */
extern int gwrap_chop_object_equal (void *o1, void *o2);

/* The function that destroys WCP, a wrapped `chop_object_t' pointer, by
   calling `chop_object_destroy ()' and then freeing the object itself.  */
extern size_t gwrap_chop_object_cleanup (void *wcp);

/* The function that gets called to mark WCP, a wrapped C pointer to a
   `chop_object_t'.  */
extern SCM gwrap_chop_object_mark (SCM wcp);

/* Allocate an instance of type KLASS.  */
static inline void *
gwrap_chop_malloc (const chop_class_t *klass)
{
  size_t size;
  chop_object_t *object;

  size = chop_class_instance_size (klass);

  object = (chop_object_t *) scm_gc_malloc (size, chop_class_name (klass));

  return (object);
}

/* Destroy and free OBJECT which was previously allocated by
   `gwrap_chop_malloc ()'.  */
static inline void
gwrap_chop_free (chop_object_t *object)
{
  const chop_class_t *klass;

  klass = chop_object_get_class (object);

  chop_object_destroy (object);
  scm_gc_free (object, chop_class_instance_size (klass),
	       chop_class_name (klass));
}

/* Free OBJECT, an uninitialized object of type KLASS.  */
static inline void
gwrap_chop_free_uninitialized (chop_object_t *object,
			       const chop_class_t *klass)
{
  scm_gc_free (object, chop_class_instance_size (klass),
	       chop_class_name (klass));
}


_CHOP_END_DECLS


/* arch-tag: 30d7f062-6749-4261-ae04-f90ef559c1c9
 */

#endif
