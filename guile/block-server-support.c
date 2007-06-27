#include <chop/chop.h>
#include <chop/block-server.h>

#include <libguile.h>

static inline errcode_t
chop_avahi_store_publisher_open_alloc (const char *service_name,
				       const char *host, unsigned int port,
				       int use_tls,
				       const char *openpgp_fpr,
				       size_t openpgp_fpr_size,
				       chop_store_publisher_t **publisher)
{
  static chop_hash_method_spec_t hash_spec =
    { CHOP_HASH_SPEC_NONE, CHOP_HASH_NONE };

  errcode_t err;

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

static inline errcode_t
chop_scm_store_publisher_loop (chop_store_publisher_t *publisher)
{
  errcode_t err;

  err = (errcode_t) scm_without_guile (do_publisher_loop, publisher);

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

static inline errcode_t
chop_scm_store_publisher_iterate (chop_store_publisher_t *publisher,
				  unsigned int timeout)
{
  errcode_t err;
  struct iterate_args args;

  args.publisher = publisher;
  args.timeout   = timeout;

  err = (errcode_t) scm_without_guile (do_publisher_iterate, &args);

  return err;
}

/* arch-tag: 549a1e57-1f4b-445e-a1ec-dbfa877f1dd6
 */
