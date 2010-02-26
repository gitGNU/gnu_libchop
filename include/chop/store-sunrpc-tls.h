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

#ifndef CHOP_STORE_SUNRPC_TLS_H
#define CHOP_STORE_SUNRPC_TLS_H

/* SunRPC over TLS remote block stores.
   Only available when GnuTLS is available.  */

#include <chop/stores.h>

#include <gnutls/gnutls.h>


_CHOP_BEGIN_DECLS

typedef chop_error_t (* chop_tls_session_initializer_t) (gnutls_session_t,
							 void *);

/* Open remote block store on port PORT of HOST using SunRPC over TLS.  INIT
   will be invoked, before the function returns, to initialize the GnuTLS
   session and will be passed CLOSURE as its second argument.  Upon success
   zero is returned and STORE is initialized.  */
extern chop_error_t
chop_sunrpc_tls_block_store_open (const char *host, unsigned port,
				  chop_tls_session_initializer_t init,
				  void *closure,
				  chop_block_store_t *store);


_CHOP_END_DECLS

#endif

/* arch-tag: b03ff898-e9a8-4bc6-8f9d-6f6cc974a3e8
 */
