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


/* Server-side stubs.  */

#include <chop/chop-config.h>

#include <chop/block-server.h>

#include "block_rstore.h"

CHOP_RPC_HANDLER (int, char *,
		  chop_block_server_say_hello_handler) = NULL;
CHOP_RPC_HANDLER (chop_rbooleans_t, chop_rblock_keys_t,
		  chop_block_server_blocks_exist_handler) = NULL;
CHOP_RPC_HANDLER (int, block_store_write_block_args,
		  chop_block_server_write_block_handler) = NULL;
CHOP_RPC_HANDLER (block_store_read_block_ret, chop_rblock_key_t,
		  chop_block_server_read_block_handler) = NULL;
CHOP_RPC_HANDLER (int, void,
		  chop_block_server_sync_handler) = NULL;
CHOP_RPC_HANDLER (int, void,
		  chop_block_server_close_handler) = NULL;


/* Rewrite the generated code so that we can pass pointers to the handler
   functions as we want, avoiding name clashes, etc.  */
#define block_store_program_1 chop_block_server_process_request
#define say_hello_1_svc       chop_block_server_say_hello_handler
#define blocks_exist_1_svc    chop_block_server_blocks_exist_handler
#define write_block_1_svc     chop_block_server_write_block_handler
#define read_block_1_svc      chop_block_server_read_block_handler
#define sync_1_svc            chop_block_server_sync_handler
#define close_1_svc           chop_block_server_close_handler

#include "block_rstore_svc.c"


#if 0
void
chop_block_server_wait_for_requests (SVCXPRT *transport,
				     unsigned timeout_usec)
{
  int err;
  struct timeval timeout;
  fd_set fds, zero_fds;

  FD_ZERO (&fds);
  FD_SET (transport->xp_sock, &fds);
  FD_ZERO (&zero_fds);

  timeout.tv_usec = timeout_usec % 1000000;
  timeout.tv_sec  = timeout_usec / 1000000;
  err = select (transport->xp_sock, &zero_fds, &zero_fds, &timeout);
}
#endif


/* Service publishers.  */

static chop_error_t
publisher_ctor (chop_object_t *object,
		const chop_class_t *class)
{
  chop_store_publisher_t *publisher;

  publisher = (chop_store_publisher_t *) object;

  publisher->service_name = publisher->host = NULL;
  publisher->port = 0;
  publisher->hash_spec.spec_type = CHOP_HASH_SPEC_NONE;
  publisher->hash_spec.method = CHOP_HASH_NONE;
  publisher->use_tls = 0;
  publisher->openpgp_fingerprint = NULL;
  publisher->openpgp_fingerprint_size = 0;

  return 0;
}

static void
publisher_dtor (chop_object_t *object)
{
  chop_store_publisher_t *publisher;

  publisher = (chop_store_publisher_t *) object;

#define FREE_FIELD(f)						\
  if (publisher-> f != NULL)					\
    {								\
      chop_free (publisher-> f, &chop_store_publisher_class);	\
      publisher-> f = NULL;					\
    }

  FREE_FIELD (service_name);
  FREE_FIELD (host);
  FREE_FIELD (openpgp_fingerprint);

#undef FREE_FIELD

  publisher->openpgp_fingerprint_size = publisher->port = 0;
}

CHOP_DEFINE_RT_CLASS (store_publisher, object,
		      publisher_ctor, publisher_dtor,
		      NULL, NULL, /* copy/equal */
		      NULL, NULL  /* serial/deserial */);


chop_error_t
chop_store_publisher_iterate (chop_store_publisher_t *publisher,
			      unsigned timeout)
{
  if (publisher->iterate)
    return publisher->iterate (publisher, timeout);

  return CHOP_ERR_NOT_IMPL;
}

chop_error_t
chop_store_publisher_loop (chop_store_publisher_t *publisher)
{
  if (publisher->loop)
    return publisher->loop (publisher);

  return CHOP_ERR_NOT_IMPL;
}
