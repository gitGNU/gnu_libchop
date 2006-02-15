/* Support for the wrapped interface of ciphers.  */

#ifdef DEBUG
# include <stdio.h>
#endif

static size_t
chop_cipher_handle_cleanup (void *wcp)
{
  /* Remember that `chop_cipher_handle_t' is itself a pointer type.  */
  chop_cipher_handle_t cipher;

  cipher = (chop_cipher_handle_t)wcp;

#ifdef DEBUG
  fprintf (stderr, "%s: freeing cipher @%p\n",
	   __FUNCTION__, wcp);
#endif

  /* This will actually free the memory pointer to by CIPHER.  */
  chop_cipher_close (cipher);

  return 0;
}
