#ifndef __CHOP_STORE_STATS_H__
#define __CHOP_STORE_STATS_H__

#include <chop/chop.h>
#include <chop/logs.h>
#include <chop/stores.h>
#include <chop/serializable.h>

_CHOP_BEGIN_DECLS

CHOP_DECLARE_RT_CLASS (block_store_stats, object,
		       char *name;

		       size_t blocks_written;
		       size_t bytes_written;
		       size_t virgin_writes;

		       float  average_block_size;
		       size_t min_block_size;
		       size_t max_block_size;);


/* Maintaining statistics on the data written to a store.  */

extern const chop_class_t chop_stat_block_store_class;

/* Initialize STORE as an instance of CHOP_STAT_BLOCK_STORE_CLASS, a
   statistic-gathering store.  If BACKEND is not NULL, the STORE will act as
   a proxy to BACKEND.  Otherwise, it will act like a ``dummy'' store, not
   actually writing anything, and being uncapable of providing and data if
   `read_block ()' is called.  If BACKEND is not NULL and TAKEOVER is
   non-zero, then BACKEND will be closed when STORE is.  NAME is the name
   that will be used to identify the underlying block store statistics.  */
extern errcode_t chop_stat_block_store_open (const char *name,
					     chop_block_store_t *backend,
					     int takeover,
					     chop_block_store_t *store);

/* Return the statistics gathered by STAT_STORE which must be a instance of
   CHOP_STAT_BLOCK_STORE_CLASS.  */
extern chop_block_store_stats_t *
chop_stat_block_store_stats (chop_block_store_t *stat_store);



/* Manipulating statistics.  */

extern errcode_t chop_block_store_stats_init (const char *name,
					      chop_block_store_stats_t *stats);

extern void chop_block_store_stats_update (chop_block_store_stats_t *stats,
					   size_t block_size, int virgin_write);

extern void chop_block_store_stats_clear (chop_block_store_stats_t *stats);

extern void chop_block_store_stats_display (const chop_block_store_stats_t *,
					    chop_log_t *log);


_CHOP_END_DECLS

#endif
