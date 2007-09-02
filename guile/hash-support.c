/* Support routines for (chop hash).  */

#include <chop/chop-config.h>

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

