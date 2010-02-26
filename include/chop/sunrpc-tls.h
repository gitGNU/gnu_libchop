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

#ifndef SUNRPC_TLS_H
#define SUNRPC_TLS_H

/* Support for Sun/ONC RPC over TLS.  This module is actually independent of
   the rest of libchop.  It relies on the RPC code written by Sun and found
   in the GNU C library (e.g., version 2.3.5), as well as on GNUtls
   1.4.1.  */

#include <rpc/svc.h>
#include <rpc/clnt.h>
#include <gnutls/gnutls.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int (* svctls_session_initializer_t) (gnutls_session_t *, void *);
typedef void (* svctls_session_finalizer_t) (gnutls_session_t, void *);
typedef int (* svctls_authorizer_t) (gnutls_session_t, void *);

/* Initialize server-side RPC/TLS code.  */
extern void svctls_init_if_needed (void);

/* Initialize client-side RPC/TLS code.  */
extern void clnttls_init_if_needed (void);


/* Create a handle to a RPC/TLS server listening on SOCK.  Whenever a new
   connection is accepted, MAKE_SESSION is invoked an passed INIT_DATA; on
   success, it should return 0 and a new session ready for handshaking; on
   failure, it should return non-zero.  If non-NULL, FINALIZER may be invoked
   anytime a session is to be finalized.  It should at least invoke
   `gnutls_deinit ()' on the given session.  It can be invoked just after a
   handshake failure (i.e., before the session has actually been used).  */
extern SVCXPRT *svctls_create (svctls_session_initializer_t make_session,
			       void *init_data,
			       svctls_session_finalizer_t finalizer,
			       void *finalizer_data,
			       svctls_authorizer_t authorizer,
			       void *auth_data,
			       int sock,
			       u_int sendsize, u_int recvsize);

/* Create a client handle to a TLS connection.  We assume that SESSION is
   already usable, i.e., that all its parameters have been set and that the
   handshake has already been performed successfully.  */
extern CLIENT *clnttls_create (gnutls_session_t session,
			       u_long prog, u_long vers,
			       u_int sendsz, u_int recvsz);


/* On success, zero is returned and *SESSION contains the GnuTLS session
   attached to XPRT.  Otherwise, an error is returned.  */
extern int svctls_getsession (SVCXPRT *xprt, gnutls_session_t *session);


#ifdef __cplusplus
}
#endif

#endif

/* arch-tag: 7e50f691-e8b6-4c99-ba53-d6be218c654e
 */
