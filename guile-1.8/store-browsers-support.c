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

/* Support functions for store browsers and in particular callbacks to Scheme
   code.  */

#include <chop/objects.h>
#include <chop/store-browsers.h>

#include "core-support.h"


CHOP_DECLARE_RT_CLASS_WITH_METACLASS (scheme_store_browser, store_browser,
				      hybrid_scheme_class, /* metaclass */

				      chop_store_browser_t *backend;
				      SCM discovery_proc;
				      SCM removal_proc;);


/* Discovery/removal callback trampolines.  */

static int
ssb_discovery_trampoline (chop_store_browser_t *browser,
			  const char *service_name,
			  const char *host,
			  unsigned port,
			  chop_hash_method_spec_t hash_spec,
			  const chop_class_t *client_class,
			  void *userdata)
{
  SCM result;
  chop_scheme_store_browser_t *scm;

  scm = (chop_scheme_store_browser_t *)userdata;
  result = scm_call_3 (scm->discovery_proc,
		       scm_from_locale_string (service_name),
		       scm_from_locale_string (host),
		       scm_from_uint (port));
  if (scm_is_false (result))
    /* Issue a quit request.  */
    return 1;

  return 0;
}

static int
ssb_removal_trampoline (chop_store_browser_t *browser,
			const char *service_name,
			void *userdata)
{
  SCM result;
  chop_scheme_store_browser_t *scm;

  scm = (chop_scheme_store_browser_t *)userdata;
  result = scm_call_1 (scm->removal_proc,
		       scm_from_locale_string (service_name));
  if (scm_is_false (result))
    /* Issue a quit request.  */
    return 1;

  return 0;
}

static chop_error_t
ssb_iterate (chop_store_browser_t *browser, unsigned msecs)
{
  chop_scheme_store_browser_t *scm;

  scm = (chop_scheme_store_browser_t *)browser;
  if (!scm->backend)
    return CHOP_INVALID_ARG;
  else
    return chop_store_browser_iterate (scm->backend, msecs);
}

static chop_error_t
ssb_loop (chop_store_browser_t *browser)
{
  chop_scheme_store_browser_t *scm;

  scm = (chop_scheme_store_browser_t *)browser;
  if (!scm->backend)
    return CHOP_INVALID_ARG;
  else
    return chop_store_browser_loop (scm->backend);
}


/* The constructor.  */

static chop_error_t
chop_avahi_store_browser_open_alloc (const char *domain,
				     SCM discovery, SCM removal,
				     chop_store_browser_t **browser)
{
  chop_error_t err;
  chop_store_browser_t *backend;
  chop_scheme_store_browser_t *scm;

  *browser = NULL;
  backend =
    gwrap_chop_malloc (&chop_avahi_store_browser_class);
  scm =
    gwrap_chop_malloc ((chop_class_t *) &chop_scheme_store_browser_class);

  err = chop_avahi_store_browser_open (domain,
				       ssb_discovery_trampoline, scm,
				       ssb_removal_trampoline, scm,
				       backend);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) scm,
	 (chop_class_t *) &chop_scheme_store_browser_class);
      gwrap_chop_free_uninitialized
	((chop_object_t *) backend,
	 &chop_avahi_store_browser_class);
      return err;
    }

  err =
    chop_object_initialize ((chop_object_t *)scm,
			    (chop_class_t *)&chop_scheme_store_browser_class);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) scm,
	 (chop_class_t *) &chop_scheme_store_browser_class);
      gwrap_chop_free ((chop_object_t *) backend);
      return err;
    }

  *browser = (chop_store_browser_t *)scm;

  scm->store_browser.loop = ssb_loop;
  scm->store_browser.iterate = ssb_iterate;
  scm->store_browser.discovery_data = NULL;
  scm->store_browser.removal_data = NULL;

  scm->backend = backend;
  scm->discovery_proc = discovery;
  scm->removal_proc = removal;

  return err;
}

static chop_log_t *
chop_scm_avahi_store_browser_log (const chop_store_browser_t *browser)
{
  chop_scheme_store_browser_t *scm;

  if (!chop_object_is_a ((chop_object_t *) browser,
			 (chop_class_t *) &chop_scheme_store_browser_class))
    return NULL;

  scm = (chop_scheme_store_browser_t *) browser;

  return (chop_avahi_store_browser_log (scm->backend));
}


/* Bookkeeping of the `chop_scheme_store_browser_t' objects.  */

static void
ssb_dtor (chop_object_t *object)
{
  chop_scheme_store_browser_t *scm;

  scm = (chop_scheme_store_browser_t *)object;
  if (scm->backend)
    {
      gwrap_chop_free ((chop_object_t *) scm->backend);
      scm->backend = NULL;
    }
}

/* Mark the closures embedded into our C object.  */
static SCM
ssb_mark (chop_object_t *object)
{
  chop_scheme_store_browser_t *scm;

  scm = (chop_scheme_store_browser_t *)object;
  scm_gc_mark (scm->discovery_proc);

  return (scm->removal_proc);
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (scheme_store_browser, store_browser,
				     hybrid_scheme_class, /* metaclass */

				     /* metaclass inits */
				     .mark = ssb_mark,

				     NULL, ssb_dtor,
				     NULL, NULL,
				     NULL, NULL);


/* arch-tag: 7b99f655-0706-4f09-9ea6-add4556a8cdc
 */
