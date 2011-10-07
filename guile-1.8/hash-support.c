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

/* Support routines for (chop hash).  */

#include <libguile.h>

/* Directly return a ready-to-use Scheme u8vector.  */
static inline SCM
chop_hash_buffer_alloc (chop_hash_method_t method,
			const char *buffer, size_t size)
{
  SCM u8vector;
  char *hash;
  size_t hash_size;

  hash_size = chop_hash_size (method);
  if (!hash_size)
    return SCM_BOOL_F;

  hash = malloc (size);
  if (!hash)
    return SCM_BOOL_F;

  chop_hash_buffer (method, buffer, size, hash);

  /* Make HASH a u8vector.  */
  u8vector = scm_take_u8vector ((unsigned char *)hash, hash_size);

  return (u8vector);
}

