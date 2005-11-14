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
  const chop_block_store_stats_t *stats;
  chop_block_store_stats_t *result;

  stats = chop_stat_block_store_stats (store);
  if (!stats)
    return NULL;

  /* FIXME: We need `chop_object_copy ()' here.  */
  result = scm_malloc (sizeof (*result));

  /* XXX: We do this just to make sure libchop's object tracker knows about
     us.  */
  chop_object_initialize ((chop_object_t *)result,
			  &chop_block_store_stats_class);

  result->name = strdup (stats->name);
  result->blocks_written = stats->blocks_written;
  result->bytes_written = stats->bytes_written;
  result->virgin_blocks = stats->virgin_blocks;
  result->virgin_bytes = stats->virgin_bytes;
  result->average_block_size = stats->average_block_size;
  result->min_block_size = stats->min_block_size;
  result->max_block_size = stats->max_block_size;

  return result;
}
