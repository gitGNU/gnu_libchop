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

/* List of available hash methods */
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

size_t
chop_hash_size (chop_hash_method_t method)
{
  const _chop_hash_method_info *p;
  for (p = hash_methods; p->name != NULL; p++)
    {
      if (p->chop_name == method)
	break;
    }
  return (p->size);
}

const char *
chop_hash_name (chop_hash_method_t method)
{
  const _chop_hash_method_info *p;
  for (p = hash_methods; p->name != NULL; p++)
    {
      if (p->chop_name == method)
	break;
    }
  return (p->name);
}

int
chop_hash_gcrypt_name (chop_hash_method_t method)
{
  const _chop_hash_method_info *p;
  for (p = hash_methods; p->name != NULL; p++)
    {
      if (p->chop_name == method)
	break;
    }
  return (p->gcrypt_name);
}

errcode_t
chop_hash_lookup (const char *name, chop_hash_method_t *method)
{
  const _chop_hash_method_info *p;
  for (p = hash_methods; p->name != NULL; p++)
    {
      if (!strcasecmp (p->name, name))
	break;
    }

  if (p->name)
    {
      *method = p->chop_name;
      return 0;
    }

  return CHOP_ERR_NOT_FOUND;
}
