/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2012  Ludovic Courtès <ludo@gnu.org>
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

/* A `smart' block store that only writes a block if it does not already
   exist on the proxied store.  This is typically useful as a proxy to remote
   block stores.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>


/* Class definition.  */

CHOP_DECLARE_RT_CLASS (smart_block_store, block_store,
		       chop_log_t log;
		       chop_proxy_semantics_t backend_ps;
		       chop_block_store_t *backend;);

static void
sbs_dtor (chop_object_t *object)
{
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *) object;

  if (smart->backend != NULL)
    switch (smart->backend_ps)
      {
      case CHOP_PROXY_LEAVE_AS_IS:
	break;

      case CHOP_PROXY_EVENTUALLY_CLOSE:
	chop_store_close (smart->backend);
	break;

      case CHOP_PROXY_EVENTUALLY_DESTROY:
	chop_object_destroy ((chop_object_t *) smart->backend);
	break;

      case CHOP_PROXY_EVENTUALLY_FREE:
	chop_object_destroy ((chop_object_t *) smart->backend);
	free (smart->backend);
	break;

      default:
	abort ();
      }

  smart->backend = NULL;
}

CHOP_DEFINE_RT_CLASS (smart_block_store, block_store,
		      NULL, sbs_dtor, /* No constructor */
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);



static chop_error_t
chop_smart_block_store_blocks_exist (chop_block_store_t *store,
				     size_t n, const chop_block_key_t keys[n],
				     bool exists[n])
{
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  return chop_store_blocks_exist (smart->backend, n, keys, exists);
}

static chop_error_t
chop_smart_block_store_read_block (chop_block_store_t *store,
				   const chop_block_key_t *key,
				   chop_buffer_t *buffer,
				   size_t *size)
{
  chop_error_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  *size = 0;

  err = chop_store_read_block (smart->backend, key, buffer, size);

  return err;
}

static chop_error_t
chop_smart_block_store_write_block (chop_block_store_t *store,
				    const chop_block_key_t *key,
				    const char *block, size_t size)
{
  chop_error_t err;
  int exists = 0;
  char hex_key[1024];
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  chop_block_key_to_hex_string (key, hex_key);
  chop_log_printf (&smart->log,
		   "smart: write_block (%s@%p, 0x%s,\n"
		   "                    %p, %zu)\n",
		   store->name, store, hex_key, block, size);

  err = chop_store_block_exists (smart->backend, key, &exists);
  if (err)
    return err;

  if (!exists)
    err = chop_store_write_block (smart->backend, key, block, size);
  else
    chop_log_printf (&smart->log, "smart: block not actually written");

  return err;
}

static chop_error_t
chop_smart_block_store_delete_block (chop_block_store_t *store,
				     const chop_block_key_t *key)
{
  chop_error_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  err = chop_store_delete_block (smart->backend, key);

  return err;
}

static chop_error_t
chop_smart_block_store_first_block (chop_block_store_t *store,
				    chop_block_iterator_t *it)
{
  chop_error_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  err = chop_store_first_block (smart->backend, it);

  return err;
}


static chop_error_t
chop_smart_block_store_sync (chop_block_store_t *store)
{
  chop_error_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  err = chop_store_sync (smart->backend);

  return err;
}

static chop_error_t
chop_smart_block_store_close (chop_block_store_t *store)
{
  chop_error_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *) store;

  if (smart->backend_ps == CHOP_PROXY_EVENTUALLY_CLOSE
      && smart->backend != NULL)
    err = chop_store_close (smart->backend);
  else
    err = 0;

  return err;
}


chop_error_t
chop_smart_block_store_open (chop_block_store_t *backend,
			     chop_proxy_semantics_t bps,
			     chop_block_store_t *store)
{
  chop_error_t err;
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  if (!backend)
    return CHOP_INVALID_ARG;

  chop_object_initialize ((chop_object_t *) store,
			  &chop_smart_block_store_class);

  err = chop_log_init ("smart-block-store", &smart->log);
  if (err)
    return err;

  store->iterator_class = chop_store_iterator_class (store);
  store->blocks_exist = chop_smart_block_store_blocks_exist;
  store->read_block = chop_smart_block_store_read_block;
  store->write_block = chop_smart_block_store_write_block;
  store->delete_block = chop_smart_block_store_delete_block;
  store->first_block = chop_smart_block_store_first_block;
  store->close = chop_smart_block_store_close;
  store->sync = chop_smart_block_store_sync;

  smart->backend = backend;
  smart->backend_ps = bps;

  return 0;
}

chop_log_t *
chop_smart_block_store_log (chop_block_store_t *store)
{
  chop_smart_block_store_t *smart =
    (chop_smart_block_store_t *)store;

  return (&smart->log);
}
