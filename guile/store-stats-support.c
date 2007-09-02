/* Contructors with a functional style that perform memory allocation by
   themselves.

   arch-tag: 7bf927c0-f737-40bc-ab13-e88357a16782
   */

#include <chop/chop-config.h>

#include <stdlib.h>
#include <errno.h>
#include <assert.h>


static inline errcode_t
chop_stat_block_store_open_alloc (const char *name,
				  chop_block_store_t *backend,
				  int close_backend,
				  chop_block_store_t **store)
{
  errcode_t err;

  *store =
    gwrap_chop_malloc (&chop_stat_block_store_class);

  err = chop_stat_block_store_open (name, backend,
				    /* in any case, let the GC do its work */
				    close_backend
				    ? CHOP_PROXY_EVENTUALLY_CLOSE
				    : CHOP_PROXY_LEAVE_AS_IS,
				    *store);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *store,
				     &chop_stat_block_store_class);
      *store = NULL;
    }

  return err;
}

static inline chop_block_store_stats_t *
chop_stat_block_store_stats_alloc (chop_block_store_t *store)
{
  errcode_t err;
  const chop_block_store_stats_t *stats;
  chop_block_store_stats_t *result;

  stats = chop_stat_block_store_stats (store);
  if (!stats)
    return NULL;

  result = gwrap_chop_malloc (&chop_block_store_stats_class);

  err = chop_object_copy ((const chop_object_t *)stats,
			  (chop_object_t *)result);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) result,
				     &chop_block_store_stats_class);
      return NULL;
    }

  return result;
}
