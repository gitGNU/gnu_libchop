#ifndef __CHOP_STORE_SUNRPC_TLS_H__
#define __CHOP_STORE_SUNRPC_TLS_H__

/* SunRPC over TLS remote block stores.
   Only available when GnuTLS is available.  */

#include <chop/stores.h>

#include <gnutls/gnutls.h>


_CHOP_BEGIN_DECLS

typedef errcode_t (* chop_tls_session_initializer_t) (gnutls_session_t,
						      void *);

/* Open remote block store on port PORT of HOST using SunRPC over TLS.  INIT
   will be invoked, before the function returns, to initialize the GnuTLS
   session and will be passed CLOSURE as its second argument.  Upon success
   zero is returned and STORE is initialized.  */
extern errcode_t
chop_sunrpc_tls_block_store_open (const char *host, unsigned port,
				  chop_tls_session_initializer_t init,
				  void *closure,
				  chop_block_store_t *store);


_CHOP_END_DECLS

#endif

/* arch-tag: b03ff898-e9a8-4bc6-8f9d-6f6cc974a3e8
 */
