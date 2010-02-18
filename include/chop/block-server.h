/* libchop -- a utility library for distributed storage
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

#ifndef __CHOP_BLOCK_SERVER_H__
#define __CHOP_BLOCK_SERVER_H__

/* RPC block server stubs.  This is provided as part of
   `libchop-block-server'.  */

#include <chop/logs.h>
#include <chop/objects.h>
#include <chop/store-browsers.h> /* for `chop_hash_method_spec_t' */

#include <rpc/svc.h>
#include <chop/block_rstore.h>


/* The rpcgen-generated server-side stub.  This is the function that should
   be passed to SVC_REGITER.  */
extern void chop_block_server_process_request (struct svc_req *rqstp,
					       register SVCXPRT *transp);


/* Define an RPC handler hook (function pointer).  */
#define CHOP_RPC_HANDLER(_ret, _arg, _name) \
  _ret *(* _name) (_arg *, struct svc_req *)

/* These are the RPC handlers that shall be defined by the user before
   SVC_REGISTER is called.  */
extern CHOP_RPC_HANDLER (int, char *, chop_block_server_say_hello_handler);
extern CHOP_RPC_HANDLER (int, chop_rblock_key_t,
			 chop_block_server_block_exists_handler);
extern CHOP_RPC_HANDLER (int, block_store_write_block_args,
			 chop_block_server_write_block_handler);
extern CHOP_RPC_HANDLER (block_store_read_block_ret, chop_rblock_key_t,
			 chop_block_server_read_block_handler);
extern CHOP_RPC_HANDLER (int, void,
			 chop_block_server_sync_handler);
extern CHOP_RPC_HANDLER (int, void,
			 chop_block_server_close_handler);



/* Service publication interface.  */

CHOP_DECLARE_RT_CLASS (store_publisher, object,
		       char *service_name;
		       char *host;
		       unsigned int port;
		       chop_hash_method_spec_t hash_spec;
		       int   use_tls;
		       char *openpgp_fingerprint;
		       size_t openpgp_fingerprint_size;

		       chop_error_t (* iterate) (struct chop_store_publisher *,
						 unsigned timeout);
		       chop_error_t (* loop) (struct chop_store_publisher *););


/* Have PUBLISHER iterate for at most TIMEOUT msec.  */
extern chop_error_t
chop_store_publisher_iterate (chop_store_publisher_t *publisher,
			      unsigned timeout);

/* Have PUBLISHER run in loop.  This function only returns when an error
   occurs.  */
extern chop_error_t
chop_store_publisher_loop (chop_store_publisher_t *publisher);



/* Implementations.  */

extern const chop_class_t chop_avahi_store_publisher_class;

/* Return an Avahi-based publisher that will publish information about the
   specified block store server over the LAN.  SERVICE_NAME should be a
   service name that will be visible by service browsers; HOST and PORT
   should specify how to reach the service (over TCP).  If HOST is NULL, then
   a default value will be used.  */
extern chop_error_t
chop_avahi_store_publisher_open (const char *service_name,
				 const char *host, unsigned int port,
				 chop_hash_method_spec_t spec,
				 int use_tls,
				 const char *openpgp_fpr,
				 size_t openpgp_fpr_size,
				 chop_store_publisher_t *publisher);

/* Return the log attached to PUBLISHER, an Avahi store publisher.  */
extern chop_log_t *
chop_avahi_store_publisher_log (chop_store_publisher_t *publisher);

#endif
