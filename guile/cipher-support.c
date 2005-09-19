/* Support for the wrapped interface of ciphers.  */

#ifdef DEBUG
# include <stdio.h>
#endif

static size_t
chop_cipher_handle_cleanup (SCM s_cipher)
{
  /* Remember that `chop_cipher_handle_t' is itself a pointer type.  */
  chop_cipher_handle_t cipher;

  cipher = (chop_cipher_handle_t)gw_wcp_get_ptr (s_cipher);

#ifdef DEBUG
  fprintf (stderr, "%s: freeing cipher @%p [SCM: %p]\n",
	   __FUNCTION__, cipher, (void *)s_cipher);
#endif

  /* This will actually free the memory pointer to by CIPHER.  */
  chop_cipher_close (cipher);

  return 0;
}
