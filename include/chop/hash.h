
#ifndef __CHOP_HASH_H__
#define __CHOP_HASH_H__

#include <chop/chop.h>

_CHOP_BEGIN_DECLS

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
extern size_t chop_hash_size (chop_hash_method_t method)
     _CHOP_PURE_FUNC;

/* Return a string representing the name of hash method METHOD.  */
extern const char *chop_hash_method_name (chop_hash_method_t method)
     _CHOP_PURE_FUNC;

/* Return the libgcrypt name (an integer) for hash method METHOD.  */
extern int chop_hash_method_gcrypt_name (chop_hash_method_t method)
     _CHOP_PURE_FUNC;

/* Return the hash method whose name is NAME (case-insensitive).  On error,
   CHOP_ERR_NOT_FOUND is returned and METHOD is kept unmodified.  */
extern errcode_t chop_hash_method_lookup (const char *name,
					  chop_hash_method_t *method);

/* Compute the hash of BUFFER (of size SIZE) using the algorithm METHOD.
   Store the result in DIGEST which must be large enough to hold a digest
   computed with METHOD.  */
extern void chop_hash_buffer (chop_hash_method_t method,
			      const char *buffer, size_t size,
			      char *digest);

_CHOP_END_DECLS

#endif
