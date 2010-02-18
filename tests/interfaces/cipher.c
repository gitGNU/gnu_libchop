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

/* Test libchop's ciphering API using various symmetric ciphering
   algorithms.  */

#include <chop/chop.h>
#include <chop/cipher.h>

#include <testsuite.h>

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>


#define SIZE_OF_INPUT  (4096)

static char *program_name = NULL;

/* The initialization vector.  */
static const char iv[400] = { 0, };

/* The clear text.  */
static char random_clear_text[SIZE_OF_INPUT];

/* The symmetric ciphering algorithms that are to be tested.  */
static const chop_cipher_algo_t the_algorithms[] =
  {
    CHOP_CIPHER_BLOWFISH,
    CHOP_CIPHER_3DES,
    CHOP_CIPHER_CAST5,
    CHOP_CIPHER_AES,
    CHOP_CIPHER_AES192,
    CHOP_CIPHER_AES256,
    CHOP_CIPHER_TWOFISH,
    CHOP_CIPHER_TWOFISH128,
    CHOP_CIPHER_DES,

    CHOP_CIPHER_NONE
  };


static inline chop_cipher_handle_t
open_cipher_handle (chop_cipher_algo_t algo,
		    const char *the_key)
{
  chop_error_t err;
  size_t key_size;
  chop_cipher_handle_t cipher_handle;

  cipher_handle = chop_cipher_open (algo, CHOP_CIPHER_MODE_ECB);
  assert (cipher_handle != CHOP_CIPHER_HANDLE_NIL);

  assert (chop_cipher_algo_block_size (algo) <= sizeof (iv));
  err = chop_cipher_set_iv (cipher_handle, iv,
			    chop_cipher_algo_block_size (algo));
  if (err)
    {
      chop_error (err, "while setting a %lu-byte init. vector", sizeof (iv));
      exit (1);
    }

  key_size = chop_cipher_algo_key_size (algo);
  test_debug ("algorithm `%s' expects keys of %zu bytes",
	      chop_cipher_algo_name (algo), key_size);

  err = chop_cipher_set_key (cipher_handle, the_key, key_size);
  if (err)
    {
      chop_error (err, "while setting a %zu-byte key", key_size);
      exit (1);
    }

  return (cipher_handle);
}


int
main (int argc, char *argv[])
{
  chop_error_t err;
  size_t algo_count, failed = 0;
  const chop_cipher_algo_t *algo;
  chop_cipher_handle_t cipher_handle;
  char the_clear_text[SIZE_OF_INPUT], the_cipher_text[SIZE_OF_INPUT];
  char the_key[40];

#define DID_FAIL()				\
  failed++, test_stage_result (0);


  /* Initialize.  */
  program_name = argv[0];
  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  for (algo = the_algorithms, algo_count = 0;
       *algo != CHOP_CIPHER_NONE;
       algo++, algo_count++)
    {
      unsigned i;

      test_stage ("algorithm `%s'", chop_cipher_algo_name (*algo));

      for (i = 0; i < sizeof (random_clear_text); i++)
	random_clear_text[i] = rand ();
      memcpy (the_clear_text, random_clear_text, sizeof (random_clear_text));

      for (i = 0; i < sizeof (the_key); i++)
	the_key[i] = rand ();

      /* Open the ciphering algorithm.  */
      cipher_handle = open_cipher_handle (*algo, the_key);
      test_assert (cipher_handle != CHOP_CIPHER_HANDLE_NIL);

      test_stage_intermediate ("encrypt");
      err = chop_cipher_encrypt (cipher_handle,
				 the_cipher_text, sizeof (the_cipher_text),
				 the_clear_text, sizeof (the_clear_text));
      if (err)
	{
	  chop_error (err, "while encrypting %zu bytes",
		      sizeof (the_clear_text));
	  DID_FAIL ();
	  continue;
	}

      if (!memcmp (the_cipher_text, the_clear_text, sizeof (the_clear_text)))
	{
	  printf ("%s: cipher text and clear text are identical\n",
		  chop_cipher_algo_name (*algo));
	  DID_FAIL ();
	  continue;
	}

      /* Close the cipher handle and start again.  */
      chop_cipher_close (cipher_handle);
      cipher_handle = open_cipher_handle (*algo, the_key);
      test_assert (cipher_handle != CHOP_CIPHER_HANDLE_NIL);

      /* Destroy the clear text.  */
      for (i = 0; i < sizeof (the_clear_text); i++)
	the_clear_text[i] = rand ();

      test_stage_intermediate ("decrypt");
      err = chop_cipher_decrypt (cipher_handle,
				 the_clear_text, sizeof (the_clear_text),
				 the_cipher_text, sizeof (the_cipher_text));
      if (err)
	{
	  chop_error (err, "while decrypting %zu bytes",
		      sizeof (the_cipher_text));
	  DID_FAIL ();
	  continue;
	}

      if (memcmp (the_clear_text, random_clear_text,
		  sizeof (random_clear_text)))
	{
	  printf ("%s: decryption result differs from original clear text\n",
		  chop_cipher_algo_name (*algo));
	  DID_FAIL ();
	  continue;
	}

      test_stage_result (1);
    }

  return (failed ? 1 : 0);
}
