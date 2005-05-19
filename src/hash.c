/* Hash/message digest utilities.  */

#include <chop/chop.h>
#include <chop/hash.h>

/* libgcrypt */
#include <gcrypt.h>


typedef struct
{
  chop_hash_method_t chop_name;
  int gcrypt_name;
  const char *name;
  size_t size;
} _chop_hash_method_info;

#define _STRINGIFY(_x) #_x
#define STRINGIFY(_z)  _STRINGIFY(_z)

#define _HASH_METHOD_INFO(_name, _size) \
{ CHOP_HASH_ ## _name, GCRY_MD_ ## _name, STRINGIFY (_name), _size }

/* List of available hash methods.  Note that we must respect the order of
   the `chop_hash_method_t' enum here (for O(1) lookup)!  */
static _chop_hash_method_info hash_methods[] =
  {
    _HASH_METHOD_INFO (NONE, 0),
    _HASH_METHOD_INFO (SHA1, 20),
    _HASH_METHOD_INFO (RMD160, 60),
    _HASH_METHOD_INFO (MD5, 16),
    _HASH_METHOD_INFO (MD4, 16),
    _HASH_METHOD_INFO (MD2, 0),
    _HASH_METHOD_INFO (TIGER, 24),
    _HASH_METHOD_INFO (HAVAL, 20),
    _HASH_METHOD_INFO (SHA256, 32),
    _HASH_METHOD_INFO (SHA384, 48),
    _HASH_METHOD_INFO (SHA512, 64),
    { 0, 0, 0, }
  };

/* Number of available hash methods.  */
#define HASH_METHOD_COUNT ((int)CHOP_HASH_SHA512 + 1)

/* Return true (non-zero) if METHOD is a valid hash method.  */
#define VALID_HASH_METHOD(_method) \
  ((int)(_method) < HASH_METHOD_COUNT)


size_t
chop_hash_size (chop_hash_method_t method)
{
  if (!VALID_HASH_METHOD (method))
    return 0;

  return (hash_methods[(int)method].size);
}

#include "gcrypt-enum-mapping.h"

MAKE_ENUM_MAPPING_FUNCTIONS (hash_method, CHOP_HASH_SHA512,
			     hash_methods, _chop_hash_method_info);


void
chop_hash_buffer (chop_hash_method_t method,
		  const char *buffer, size_t size,
		  char *digest)
{
  if (!VALID_HASH_METHOD (method))
    return;

  return (gcry_md_hash_buffer (hash_methods[(int)method].gcrypt_name,
			       digest, buffer, size));
}
