#ifndef __SUNRPC_TLS_H__
#define __SUNRPC_TLS_H__

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
typedef int (* svctls_authorizer_t) (gnutls_session_t, void *);

/* Initialize server-side RPC/TLS code.  */
extern void svctls_init_if_needed (void);

/* Initialize client-side RPC/TLS code.  */
extern void clnttls_init_if_needed (void);


/* Create a handle to a RPC/TLS server listening on SOCK.  Whenever a new
   connection is accepted, MAKE_SESSION is invoked an passed INIT_DATA; on
   success, it should return 0 and a new session ready for handshaking; on
   failure, it should return non-zero.  */
extern SVCXPRT *svctls_create (svctls_session_initializer_t make_session,
			       void *init_data,
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



#ifdef __cplusplus
}
#endif

#endif

/* arch-tag: 7e50f691-e8b6-4c99-ba53-d6be218c654e
 */
