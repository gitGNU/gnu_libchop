/* Ciphering functions.  */

#include <chop/chop.h>
#include <chop/cipher.h>

/* libgcrypt */
#include <gcrypt.h>

#include <stdlib.h>



/* This log is initialized by `chop_init ()'.  */
chop_log_t chop_cipher_log;


typedef struct
{
  chop_cipher_algo_t chop_name;
  int gcrypt_name;
  const char *name;
} _chop_enum_mapping;

#ifndef STRINGIFY
# define _STRINGIFY(_x) #_x
# define STRINGIFY(_z)  _STRINGIFY(_z)
#endif

#define _CIPHER_ALGO_INFO(_name) \
{ CHOP_CIPHER_ ## _name, GCRY_CIPHER_ ## _name, STRINGIFY (_name) }

/* List of available hash methods.  Note that we must respect the order of
   the `chop_cipher_algo_t' enum here (for O(1) lookup)!  */
static const _chop_enum_mapping cipher_algos[] =
  {
    _CIPHER_ALGO_INFO (NONE),
    _CIPHER_ALGO_INFO (IDEA),
    _CIPHER_ALGO_INFO (3DES),
    _CIPHER_ALGO_INFO (CAST5),
    _CIPHER_ALGO_INFO (BLOWFISH),
    _CIPHER_ALGO_INFO (SAFER_SK128),
    _CIPHER_ALGO_INFO (DES_SK),
    _CIPHER_ALGO_INFO (AES),
    _CIPHER_ALGO_INFO (AES192),
    _CIPHER_ALGO_INFO (AES256),
    _CIPHER_ALGO_INFO (TWOFISH),
    _CIPHER_ALGO_INFO (TWOFISH128),

    _CIPHER_ALGO_INFO (ARCFOUR),
    _CIPHER_ALGO_INFO (DES),

    { 0, 0, 0, }
  };


#define _CIPHER_MODE_INFO(_name) \
{ CHOP_CIPHER_MODE_ ## _name, GCRY_CIPHER_MODE_ ## _name, STRINGIFY (_name) }

static const _chop_enum_mapping cipher_modes[] =
  {
    _CIPHER_MODE_INFO (NONE),
    _CIPHER_MODE_INFO (ECB),     /* Electronic codebook. */
    _CIPHER_MODE_INFO (CFB),     /* Cipher feedback. */
    _CIPHER_MODE_INFO (CBC),     /* Cipher block chaining. */
    _CIPHER_MODE_INFO (STREAM),  /* Used with stream ciphers. */
    _CIPHER_MODE_INFO (OFB),     /* Outer feedback. */

    { 0, 0, 0 }
  };


#include "gcrypt-enum-mapping.h"

MAKE_ENUM_MAPPING_FUNCTIONS (cipher_algo, CHOP_CIPHER_DES,
			     cipher_algos, _chop_enum_mapping);

MAKE_ENUM_MAPPING_FUNCTIONS (cipher_mode, CHOP_CIPHER_MODE_OFB,
			     cipher_modes, _chop_enum_mapping);

#define VALID_CIPHER_ALGO(_algo) \
  ((int)(_algo) <= CHOP_CIPHER_DES)

#define VALID_CIPHER_MODE(_mode) \
  ((int)(_mode) <= CHOP_CIPHER_MODE_OFB)



struct chop_cipher_handle
{
  /* The actual handle */
  gcry_cipher_hd_t gcry_handle;

  /* Bits of information not provided by libgcrypt */
  chop_cipher_algo_t algo;
  chop_cipher_mode_t mode;
};


chop_cipher_handle_t
chop_cipher_open (chop_cipher_algo_t algo, chop_cipher_mode_t mode)
		  /* We don't care about libgcrypt's FLAGS arg.  */
{
  gcry_error_t gerr;
  gcry_cipher_hd_t ghandle;
  chop_cipher_handle_t handle;

  if ((!VALID_CIPHER_ALGO (algo)) || (!VALID_CIPHER_MODE (mode)))
    return NULL;

  gerr = gcry_cipher_open (&ghandle,
			   cipher_algos[(int)algo].gcrypt_name,
			   cipher_modes[(int)mode].gcrypt_name,
			   0);
  if (gerr)
    return NULL;

  /* XXX:  All this overhead is libgcrypt's fault!  */
  handle = malloc (sizeof (struct chop_cipher_handle));
  if (!handle)
    return NULL;

  handle->gcry_handle = ghandle;
  handle->algo = algo;
  handle->mode = mode;

  return (handle);
}

chop_cipher_handle_t
chop_cipher_copy (chop_cipher_handle_t handle)
{
  /* XXX:  Gcrypt has no `cipher_copy' method but everything should work fine
     this way.  */
  return (chop_cipher_open (handle->algo, handle->mode));
}

chop_cipher_algo_t
chop_cipher_algorithm (chop_cipher_handle_t handle)
{
  return (handle->algo);
}

chop_cipher_mode_t
chop_cipher_mode (chop_cipher_handle_t handle)
{
  return (handle->mode);
}

size_t
chop_cipher_algo_key_size (chop_cipher_algo_t algo)
{
  if (!VALID_CIPHER_ALGO (algo))
    return 0;

  return (gcry_cipher_get_algo_keylen (cipher_algos[(int)algo].gcrypt_name));
}

size_t
chop_cipher_algo_block_size (chop_cipher_algo_t algo)
{
  if (!VALID_CIPHER_ALGO (algo))
    return 0;

  return (gcry_cipher_get_algo_blklen (cipher_algos[(int)algo].gcrypt_name));
}

errcode_t
chop_cipher_set_key (chop_cipher_handle_t handle,
		     const void *key, size_t key_size)
{
  gcry_error_t gerr;

  gerr = gcry_cipher_setkey (handle->gcry_handle, key, key_size);
  if (gerr)
    chop_log_printf (&chop_cipher_log,
		     "chop_cipher_set_key (%u bytes): %s [src: %s]",
		     key_size,
		     gcry_strerror (gerr), gcry_strsource (gerr));

  if (gcry_err_code (gerr) == GPG_ERR_WEAK_KEY)
    return CHOP_CIPHER_WEAK_KEY;

  return (gerr ? CHOP_OUT_OF_RANGE_ARG : 0);
}

errcode_t
chop_cipher_set_iv (chop_cipher_handle_t handle,
		    const void *iv, size_t iv_size)
{
  gcry_error_t gerr;

  gerr = gcry_cipher_setiv (handle->gcry_handle, iv, iv_size);

  return (gerr ? CHOP_OUT_OF_RANGE_ARG : 0);
}

void
chop_cipher_reset (chop_cipher_handle_t handle)
{
  (void)gcry_cipher_reset (handle->gcry_handle);
}


#if 0
/* Try to limit the overhead for the other functions...  */
typedef errcode_t (* _crypt_func_t) (chop_cipher_handle_t,
				     char *, size_t,
				     const char *, size_t);

_crypt_func_t chop_cipher_encrypt = (_crypt_func_t)gcry_cipher_encrypt;
_crypt_func_t chop_cipher_decrypt = (_crypt_func_t)gcry_cipher_decrypt;

void (* chop_cipher_close) (chop_cipher_handle_t handle) =
  (void (*) (chop_cipher_handle_t)) gcry_cipher_close;
#endif

errcode_t
chop_cipher_encrypt (chop_cipher_handle_t cipher,
		     char *out, size_t out_size,
		     const char *in, size_t in_size)
{
  gcry_error_t gerr;

  gerr = gcry_cipher_encrypt (cipher->gcry_handle, out, out_size,
			      in, in_size);
  if (gerr)
    chop_log_printf (&chop_cipher_log,
		     "chop_cipher_encrypt (in: %u bytes; out: %u bytes): "
		     "%s [src: %s]\n",
		     in_size, out_size,
		     gcry_strerror (gerr), gcry_strsource (gerr));

  return (gerr ? CHOP_INVALID_ARG : 0);
}

errcode_t
chop_cipher_decrypt (chop_cipher_handle_t cipher,
		     char *out, size_t out_size,
		     const char *in, size_t in_size)
{
  gcry_error_t gerr;

  gerr = gcry_cipher_decrypt (cipher->gcry_handle, out, out_size,
			      in, in_size);

  return (gerr ? CHOP_INVALID_ARG : 0);
}

void
chop_cipher_close (chop_cipher_handle_t cipher)
{
  gcry_cipher_close (cipher->gcry_handle);

  free (cipher);
}


void
chop_randomize (char *buffer, size_t size)
{
  gcry_randomize (buffer, size, GCRY_STRONG_RANDOM);
}


#if 0
errcode_t
chop_cipher_hash_encrypt (chop_cipher_handle_t cipher_handle,
			  chop_hash_method_t hash_method,
			  char *hash_key,
			  char *out, size_t out_size,
			  const char *in, size_t in_size)
{
  int err;
  size_t hash_size, key_size;

  gcry_md_hash_buffer (chop_hash_method_gcrypt_name (hash_method),
		       hash_key, in, in_size);

  if (cipher_handle == CHOP_CIPHER_HANDLE_NIL)
    return 0;

  hash_size = chop_hash_size (hash_method);
  gcry_cipher_setkey (cipher_handle, hash_key, hash_size);

  err = gcry_cipher_encrypt (cipher_handle, out, out_size, in, in_size);
  if (!err)
    {
      gcry_md_hash_buffer (chop_hash_method_gcrypt_name (hash_method),
			   hash_key, out, in_size /* not a mistake */);
    }

  return (err ? CHOP_INVALID_ARG : 0);  /* FIXME */
}
#endif
