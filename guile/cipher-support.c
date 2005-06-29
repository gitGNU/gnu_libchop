/* Support for the wrapped interface of ciphers.  */

static void
chop_cipher_handle_close_dealloc (chop_cipher_handle_t cipher)
{
  chop_cipher_close (cipher);
}
