/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2012, 2013  Ludovic Courtès <ludo@gnu.org>
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

/* A `stat' block store that computes statistics about the blocks written on
   a store.  */

#include <chop/chop-config.h>

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/store-stats.h>
#include <chop/buffers.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>




/* Class definition.  */

CHOP_DECLARE_RT_CLASS (stat_block_store, block_store,
		       chop_block_store_t *backend;
		       chop_proxy_semantics_t backend_ps;

		       chop_block_store_stats_t stats;);

static void
sbs_dtor (chop_object_t *object)
{
  chop_stat_block_store_t *stat = (chop_stat_block_store_t *)object;

  if (stat->backend)
    {
      switch (stat->backend_ps)
	{
	case CHOP_PROXY_LEAVE_AS_IS:
	  break;

	case CHOP_PROXY_EVENTUALLY_CLOSE:
	  chop_store_close (stat->backend);
	  break;

	case CHOP_PROXY_EVENTUALLY_DESTROY:
	  chop_object_destroy ((chop_object_t *)stat->backend);
	  break;

	case CHOP_PROXY_EVENTUALLY_FREE:
	  chop_object_destroy ((chop_object_t *)stat->backend);
	  free (stat->backend);
	  break;

	default:
	  abort ();
	}
    }

  chop_object_destroy ((chop_object_t *)&stat->stats);

  stat->backend = NULL;
}

CHOP_DEFINE_RT_CLASS (stat_block_store, block_store,
		      NULL, sbs_dtor,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);



/* The block store methods.  */

static chop_error_t
chop_stat_block_store_blocks_exist (chop_block_store_t *store,
				    size_t n, const chop_block_key_t keys[n],
				    bool exists[n])
{
  chop_error_t err = 0;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  if (stat->backend)
    err = chop_store_blocks_exist (stat->backend, n, keys, exists);
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

static chop_error_t
chop_stat_block_store_read_block (chop_block_store_t *store,
				  const chop_block_key_t *key,
				  chop_buffer_t *buffer,
				  size_t *size)
{
  chop_error_t err;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  *size = 0;

  if (stat->backend)
    err = chop_store_read_block (stat->backend, key, buffer, size);
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

static chop_error_t
chop_stat_block_store_write_block (chop_block_store_t *store,
				   const chop_block_key_t *key,
				   const char *block, size_t size)
{
  chop_error_t err = 0;
  int exists = 0;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  if (stat->backend)
    {
      err = chop_store_block_exists (stat->backend, key, &exists);
      if (err)
	return err;

      err = chop_store_write_block (stat->backend, key, block, size);
    }

  if (!err)
    chop_block_store_stats_update (&stat->stats, size, exists ? 0 : 1);

  return err;
}

static chop_error_t
chop_stat_block_store_delete_block (chop_block_store_t *store,
				    const chop_block_key_t *key)
{
  chop_error_t err;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  if (stat->backend)
    err = chop_store_delete_block (stat->backend, key);
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

static chop_error_t
chop_stat_block_store_first_block (chop_block_store_t *store,
				   chop_block_iterator_t *it)
{
  chop_error_t err;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  if (stat->backend)
    err = chop_store_first_block (stat->backend, it);
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

static chop_error_t
chop_stat_block_store_sync (chop_block_store_t *store)
{
  chop_error_t err;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  if (stat->backend)
    err = chop_store_sync (stat->backend);
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

static chop_error_t
chop_stat_block_store_close (chop_block_store_t *store)
{
  chop_error_t err = 0;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  if (stat->backend)
    {
      switch (stat->backend_ps)
	{
	case CHOP_PROXY_LEAVE_AS_IS:
	  break;

	case CHOP_PROXY_EVENTUALLY_CLOSE:
	case CHOP_PROXY_EVENTUALLY_DESTROY:
	case CHOP_PROXY_EVENTUALLY_FREE:
	  err = chop_store_close (stat->backend);
	  break;

	default:
	  abort ();
	}
    }

  return err;
}


/* Initializing a stat block store.  */

chop_error_t
chop_stat_block_store_open (const char *name,
			    chop_block_store_t *backend,
			    chop_proxy_semantics_t bps,
			    chop_block_store_t *store)
{
  chop_error_t err;
  chop_stat_block_store_t *stat =
    (chop_stat_block_store_t *)store;

  chop_object_initialize ((chop_object_t *)store,
			  &chop_stat_block_store_class);

  err = chop_block_store_stats_init (name, &stat->stats);
  if (err)
    return err;

  store->iterator_class = chop_store_iterator_class (backend);
  store->blocks_exist = chop_stat_block_store_blocks_exist;
  store->read_block = chop_stat_block_store_read_block;
  store->write_block = chop_stat_block_store_write_block;
  store->delete_block = chop_stat_block_store_delete_block;
  store->first_block = chop_stat_block_store_first_block;
  store->close = chop_stat_block_store_close;
  store->sync = chop_stat_block_store_sync;

  stat->backend = backend;
  stat->backend_ps = bps;

  return 0;
}

const chop_block_store_stats_t *
chop_stat_block_store_stats (const chop_block_store_t *store)
{
  chop_stat_block_store_t *stat;

  if (!chop_object_is_a ((chop_object_t *)store,
			 &chop_stat_block_store_class))
    return NULL;

  stat = (chop_stat_block_store_t *)store;

  return (&stat->stats);
}


/* The `chop_block_store_stats_t' class.  */

static chop_error_t
stats_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_block_store_stats_t *stats =
    (chop_block_store_stats_t *)object;

  stats->name = NULL;

  chop_block_store_stats_clear (stats);

  return 0;
}

static void
stats_dtor (chop_object_t *object)
{
  chop_block_store_stats_t *stats =
    (chop_block_store_stats_t *)object;

  if (stats->name)
    chop_free (stats->name, &chop_block_store_stats_class);

  stats->name = NULL;

  chop_block_store_stats_clear (stats);
}

static chop_error_t
bss_copy (const chop_object_t *src, chop_object_t *dst)
{
  const chop_block_store_stats_t *source = (chop_block_store_stats_t *)src;
  chop_block_store_stats_t *dest = (chop_block_store_stats_t *)dst;

  if (source->name)
    {
      dest->name = chop_strdup (source->name,
				&chop_block_store_stats_class);
      if (!dest->name)
	return ENOMEM;
    }
  else
    dest->name = NULL;

  dest->blocks_written = source->blocks_written;
  dest->bytes_written = source->bytes_written;
  dest->virgin_blocks = source->virgin_blocks;
  dest->virgin_bytes = source->virgin_bytes;
  dest->average_block_size = source->average_block_size;
  dest->min_block_size = source->min_block_size;
  dest->max_block_size = source->max_block_size;

  return 0;
}


CHOP_DEFINE_RT_CLASS (block_store_stats, object,
		      stats_ctor, stats_dtor,
		      bss_copy, NULL,
		      NULL, NULL);


/* The `chop_block_store_stats_t' methods.  */

chop_error_t
chop_block_store_stats_init (const char *name,
			     chop_block_store_stats_t *stats)
{
  chop_error_t err;

  err = chop_object_initialize ((chop_object_t *)stats,
				&chop_block_store_stats_class);
  if (err)
    return err;

  if (name)
    {
      stats->name = chop_strdup (name, &chop_block_store_stats_class);
      if (!stats->name)
	{
	  chop_object_destroy ((chop_object_t *)stats);
	  return ENOMEM;
	}
    }

  return 0;
}

void
chop_block_store_stats_clear (chop_block_store_stats_t *stats)
{
  stats->blocks_written = stats->bytes_written = 0;
  stats->virgin_blocks = stats->virgin_bytes = 0;

  stats->average_block_size = 0;
  stats->min_block_size = stats->max_block_size = 0;
  stats->min_block_size--;
}

void
chop_block_store_stats_update (chop_block_store_stats_t *stats,
			       size_t block_size, int virgin_write)
{
  stats->average_block_size *= stats->blocks_written;
  stats->average_block_size += block_size;
  stats->blocks_written++;
  stats->average_block_size /= stats->blocks_written;

  stats->min_block_size = (block_size < stats->min_block_size)
    ? block_size : stats->min_block_size;
  stats->max_block_size = (block_size > stats->max_block_size)
    ? block_size : stats->max_block_size;

  stats->bytes_written += block_size;
  if (virgin_write)
    {
      stats->virgin_blocks++;
      stats->virgin_bytes += block_size;
    }
}

void
chop_block_store_stats_display (const chop_block_store_stats_t *stats,
				chop_log_t *log)
{
  chop_log_printf (log, "* store `%s'",
		   stats->name ? stats->name : "<nameless>");

  chop_log_printf (log, "  blocks written:        %7zu",
		   stats->blocks_written);
  chop_log_printf (log, "  bytes written:         %7zu",
		   stats->bytes_written);
  chop_log_printf (log, "  virgin blocks:         %7zu (% 2.1f%%)",
		   stats->virgin_blocks,
		   ((float)stats->virgin_blocks
		    / (float)stats->blocks_written) * 100);
  chop_log_printf (log, "  virgin bytes:          %7zu (% 2.1f%%)",
		   stats->virgin_bytes,
		   ((float)stats->virgin_bytes
		    / (float)stats->bytes_written)* 100);

  chop_log_printf (log, "  average block size:    % 7.2f",
		   stats->average_block_size);
  chop_log_printf (log, "  min block size:        %7zu",
		   stats->min_block_size);
  chop_log_printf (log, "  max block size:        %7zu",
		   stats->max_block_size);
}
