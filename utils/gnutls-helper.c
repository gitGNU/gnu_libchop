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

/* We assume the availability of this variable once linked.  */
extern const char *program_name;



/* File system helpers.  */

static errcode_t
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

static errcode_t
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

static errcode_t
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
