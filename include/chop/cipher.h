#ifndef __CHOP_CIPHER_H__
#define __CHOP_CIPHER_H__

#include <chop/chop.h>

typedef void *chop_cipher_handle_t;

enum chop_cipher_algo
  {
    CHOP_CIPHER_NONE = 0,
    CHOP_CIPHER_IDEA,
    CHOP_CIPHER_3DES,
    CHOP_CIPHER_CAST5,
    CHOP_CIPHER_BLOWFISH,
    CHOP_CIPHER_SAFER_SK128,
    CHOP_CIPHER_DES_SK,
    CHOP_CIPHER_AES,
    CHOP_CIPHER_AES192,
    CHOP_CIPHER_AES256,
    CHOP_CIPHER_TWOFISH,

    CHOP_CIPHER_ARCFOUR,
    CHOP_CIPHER_DES
  };

enum chop_cipher_mode
  {
    CHOP_CIPHER_MODE_NONE = 0,
    CHOP_CIPHER_MODE_ECB,     /* Electronic codebook. */
    CHOP_CIPHER_MODE_CFB,     /* Cipher feedback. */
    CHOP_CIPHER_MODE_CBC,     /* Cipher block chaining. */
    CHOP_CIPHER_MODE_STREAM,  /* Used with stream ciphers. */
    CHOP_CIPHER_MODE_OFB      /* Outer feedback. */
  };

typedef enum chop_cipher_algo chop_cipher_algo_t;
typedef enum chop_cipher_mode chop_cipher_mode_t;


/* Return a string representing the name of hash method ALGO.  */
extern const char *chop_cipher_algo_name (chop_cipher_algo_t algo);

/* Return the libgcrypt name (an integer) for hash method ALGO.  */
extern int chop_cipher_algo_gcrypt_name (chop_cipher_algo_t algo);

/* Return the hash method whose name is NAME (case-insensitive).  On error,
   CHOP_ERR_NOT_FOUND is returned and ALGO is kept unmodified.  */
extern errcode_t chop_cipher_algo_lookup (const char *name,
					  chop_cipher_algo_t *algo);


/* Return a string representing the name of hash method MODE.  */
extern const char *chop_cipher_mode_name (chop_cipher_mode_t mode);

/* Return the libgcrypt name (an integer) for hash method MODE.  */
extern int chop_cipher_mode_gcrypt_name (chop_cipher_mode_t mode);

/* Return the hash method whose name is NAME (case-insensitive).  On error,
   CHOP_ERR_NOT_FOUND is returned and MODE is kept unmodified.  */
extern errcode_t chop_cipher_mode_lookup (const char *name,
					  chop_cipher_mode_t *mode);

#define CHOP_CIPHER_HANDLE_NIL  ((void *)0)

/* Return a handle to the ciphering algorithm represented by ALGO, in mode
   MODE, using the KEY_SIZE bytes pointed to by KEY as the ciphering key.  */ 
extern chop_cipher_handle_t
chop_cipher_open (chop_cipher_algo_t algo, chop_cipher_mode_t mode);

extern void chop_cipher_set_key (chop_cipher_handle_t handle,
				 void *key, size_t key_size);

extern errcode_t (* chop_cipher_encrypt) (chop_cipher_handle_t handle,
					  char *out, size_t out_size,
					  const char *in, size_t in_size);

extern errcode_t (* chop_cipher_decrypt) (chop_cipher_handle_t handle,
					  char *out, size_t out_size,
					  const char *in, size_t in_size);

extern void (* chop_cipher_close) (chop_cipher_handle_t handle);


#endif
