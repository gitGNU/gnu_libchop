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

#include <alloca.h>

#include "gnutls-helper.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <errno.h>
#include <progname.h>



/* File system helpers.  */

static chop_error_t
open_config_file (const char *dir, const char *name,
		  int flags, int *file)
{
  static char *home = NULL;

  char *path;
  struct stat dir_st;

  if (!home)
    {
      home = getenv ("HOME");
      if (!home)
	home = "";
    }

  path = (char *) alloca (strlen (home) + strlen (dir)
			  + strlen (name) + 3);
  strcpy (path, home);
  strcat (path, "/");
  strcat (path, dir);
  if (stat (path, &dir_st))
    {
      switch (errno)
	{
	case ENOENT:
	  if (mkdir (path, 0700))
	    return errno;
	  break;

	default:
	  return errno;
	}
    }

  strcat (path, "/");
  strcat (path, name);

  *file = open (path, flags, S_IRUSR | S_IWUSR);
  if (*file == -1)
    return errno;

  return 0;
}

static chop_error_t
dump_to_file (int fd, const char *buf, size_t size)
{
  const char *p;
  ssize_t written;

  for (p = buf; size > 0; p += written, size -= written)
    {
      written = write (fd, p, size);
      if (written == -1)
	{
	  int err = errno;

	  switch (err)
	    {
	    case EINTR:
	      written = 0;
	      break;

	    default:
	      return err;
	    }
	}
    }

  return 0;
}

static chop_error_t
file_content (int fd, void **content, size_t *size)
{
  struct stat st;

  if (fstat (fd, &st))
    return errno;

  *size = st.st_size;
  *content = mmap (NULL, *size, PROT_READ, MAP_SHARED, fd, 0);
  if (!*content)
    return errno;

  return 0;
}

static void
free_file_content (void *content, size_t size)
{
  (void) munmap (content, size);
}



/* Generating/storing/retrieving TLS parameters.  */

/* Generate Diffie Hellman parameters---for use with DHE kx algorithms.
   These should be discarded and regenerated once a day, once a week or once
   a month, depending on the security requirements.  */
#define PARAMS_KIND       dh
#define PARAMS_T          gnutls_dh_params_t
#define PARAMS_INIT       gnutls_dh_params_init
#define GENERATE_PARAMS   gnutls_dh_params_generate2
#define PARAMS_BITS       1024
#define PARAMS_EXPORT     gnutls_dh_params_export_pkcs3
#define PARAMS_IMPORT     gnutls_dh_params_import_pkcs3

#include "gnutls-init-params.c"

#undef PARAMS_IMPORT
#undef PARAMS_EXPORT
#undef PARAMS_BITS
#undef GENERATE_PARAMS
#undef PARAMS_INIT
#undef PARAMS_T
#undef PARAMS_KIND

/* Generate or retrieve from disk RSA parameters.  */
#define PARAMS_KIND       rsa
#define PARAMS_T          gnutls_rsa_params_t
#define PARAMS_INIT       gnutls_rsa_params_init
#define GENERATE_PARAMS   gnutls_rsa_params_generate2
#define PARAMS_BITS       1024
#define PARAMS_EXPORT     gnutls_rsa_params_export_pkcs1
#define PARAMS_IMPORT     gnutls_rsa_params_import_pkcs1

#include "gnutls-init-params.c"

#undef PARAMS_IMPORT
#undef PARAMS_EXPORT
#undef PARAMS_BITS
#undef GENERATE_PARAMS
#undef PARAMS_INIT
#undef PARAMS_T
#undef PARAMS_KIND



/* arch-tag: 4bd4de22-bb18-4ef3-9866-4d28e83d1f92
 */
