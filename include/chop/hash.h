
#ifndef __CHOP_HASH_H__
#define __CHOP_HASH_H__

#include <chop/chop.h>

enum chop_hash_method
  {
    CHOP_HASH_NONE = 0,
    CHOP_HASH_SHA1,
    CHOP_HASH_RMD160,
    CHOP_HASH_MD5,
    CHOP_HASH_MD4,
    CHOP_HASH_MD2,
    CHOP_HASH_TIGER,
    CHOP_HASH_HAVAL,
    CHOP_HASH_SHA256,
    CHOP_HASH_SHA384,
    CHOP_HASH_SHA512
  };

/* Return the size (in bytes) of the digest yielded by hash method
   METHOD.  */
extern size_t chop_hash_size (chop_hash_method_t method);

/* Return a string representing the name of hash method METHOD.  */
extern const char *chop_hash_name (chop_hash_method_t method);

/* Return the libgcrypt name (an integer) for hash method METHOD.  */
extern int chop_hash_gcrypt_name (chop_hash_method_t method);

#endif
