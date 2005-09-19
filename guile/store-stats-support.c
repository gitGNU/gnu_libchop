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
				  chop_block_store_t **store)
{
  errcode_t err;

  *store =
    scm_malloc (chop_class_instance_size (&chop_stat_block_store_class));

  err = chop_stat_block_store_open (name, backend,
				    0, /* let the GC do its work */
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
  const chop_block_store_stats_t *stats;
  chop_block_store_stats_t *result;

  stats = chop_stat_block_store_stats (store);
  if (!stats)
    return NULL;

  result = scm_malloc (sizeof (*result));
  memcpy (result, stats, sizeof (*result));

  return result;
}
