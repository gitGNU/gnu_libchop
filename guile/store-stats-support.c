/* Contructors with a functional style that perform memory allocation by
   themselves.

   arch-tag: 7bf927c0-f737-40bc-ab13-e88357a16782
   */

#include <stdlib.h>
#include <errno.h>
#include <assert.h>


static __inline__ errcode_t
chop_stat_block_store_open_alloc (const char *name,
				  chop_block_store_t *backend,
				  int close_backend,
				  chop_block_store_t **store)
{
  errcode_t err;

  *store =
    scm_malloc (chop_class_instance_size (&chop_stat_block_store_class));

  err = chop_stat_block_store_open (name, backend,
				    /* in any case, let the GC do its work */
				    close_backend
				    ? CHOP_PROXY_EVENTUALLY_CLOSE
				    : CHOP_PROXY_LEAVE_AS_IS,
				    *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static __inline__ chop_block_store_stats_t *
chop_stat_block_store_stats_alloc (chop_block_store_t *store)
{
  errcode_t err;
  const chop_block_store_stats_t *stats;
  chop_block_store_stats_t *result;

  stats = chop_stat_block_store_stats (store);
  if (!stats)
    return NULL;

  result = scm_malloc (sizeof (*result));

  err = chop_object_copy ((const chop_object_t *)stats,
			  (chop_object_t *)result);
  if (err)
    {
      free (result);
      return NULL;
    }

  return result;
}
