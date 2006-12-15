/* Helper to store and retrieve GNUtls parameters to/from disk.  This module
   is meant to be linked with stand-alone executables such as
   `chop-block-server'.  */

#ifndef __CHOP_GNUTLS_HELPER_H__
#define __CHOP_GNUTLS_HELPER_H__

#include <chop/chop.h>
#include <gnutls/gnutls.h>

_CHOP_BEGIN_DECLS

extern errcode_t chop_tls_initialize_dh_params (gnutls_dh_params_t *,
						const char *config_dir,
						const char *filename);

extern errcode_t chop_tls_initialize_rsa_params (gnutls_rsa_params_t *,
						 const char *config_dir,
						 const char *filename);

_CHOP_END_DECLS

#endif

/* arch-tag: 233efab3-9454-41b2-8289-e3fe37ff9e1b
 */
