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
#include <chop/block-server.h>

#include <libguile.h>

static inline chop_error_t
chop_avahi_store_publisher_open_alloc (const char *service_name,
				       const char *host, unsigned int port,
				       int use_tls,
				       const char *openpgp_fpr,
				       size_t openpgp_fpr_size,
				       chop_store_publisher_t **publisher)
{
  static chop_hash_method_spec_t hash_spec =
    { CHOP_HASH_SPEC_NONE, CHOP_HASH_NONE };

  chop_error_t err;

  *publisher =
    gwrap_chop_malloc (&chop_avahi_store_publisher_class);

  err = chop_avahi_store_publisher_open (service_name, host, port,
					 hash_spec, use_tls,
					 openpgp_fpr, openpgp_fpr_size,
					 *publisher);
  if (err)
    gwrap_chop_free_uninitialized ((chop_object_t *) *publisher,
				   &chop_avahi_store_publisher_class);

  return err;
}


/* Publisher iteration and loop.  */

/* These functions may block outside of Guile for a long time.  Thus, they
   need to be invoked in "non-Guile mode" so that GC can occur.  */

static void *
do_publisher_loop (void *data)
{
  chop_store_publisher_t *publisher;

  publisher = (chop_store_publisher_t *) data;

  return ((void *) chop_store_publisher_loop (publisher));
}

static inline chop_error_t
chop_scm_store_publisher_loop (chop_store_publisher_t *publisher)
{
  chop_error_t err;

  err = (chop_error_t) scm_without_guile (do_publisher_loop, publisher);

  return err;
}

struct iterate_args
{
  chop_store_publisher_t *publisher;
  unsigned int            timeout;
};

static void *
do_publisher_iterate (void *data)
{
  struct iterate_args *args;

  args = (struct iterate_args *) data;

  return ((void *) chop_store_publisher_iterate (args->publisher,
						 args->timeout));
}

static inline chop_error_t
chop_scm_store_publisher_iterate (chop_store_publisher_t *publisher,
				  unsigned int timeout)
{
  chop_error_t err;
  struct iterate_args args;

  args.publisher = publisher;
  args.timeout   = timeout;

  err = (chop_error_t) scm_without_guile (do_publisher_iterate, &args);

  return err;
}

/* arch-tag: 549a1e57-1f4b-445e-a1ec-dbfa877f1dd6
 */
