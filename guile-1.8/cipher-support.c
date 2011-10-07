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

/* Support for the wrapped interface of ciphers.  */

#ifdef DEBUG
# include <stdio.h>
#endif

static size_t
chop_cipher_handle_cleanup (void *wcp)
{
  /* Remember that `chop_cipher_handle_t' is itself a pointer type.  */
  chop_cipher_handle_t cipher;

  cipher = (chop_cipher_handle_t)wcp;

#ifdef DEBUG
  fprintf (stderr, "%s: freeing cipher @%p\n",
	   __FUNCTION__, wcp);
#endif

  /* This will actually free the memory pointer to by CIPHER.  */
  chop_cipher_close (cipher);

  return 0;
}
