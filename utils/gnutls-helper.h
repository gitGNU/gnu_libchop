/* libchop -- a utility library for distributed storage and data backup
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
