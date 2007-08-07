/* Display information about OpenPGP keys.  This is primarily meant to be
   useful in debugging ciphersuite negotiation issues.  */

#include <chop/chop.h>
#include <chop/chop-config.h>

#include <gnutls/gnutls.h>
#include <gnutls/extra.h>
#include <gnutls/openpgp.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <sys/mman.h>

#include <stdlib.h>
#include <stdio.h>

#include <argp.h>


const char *argp_program_version = "chop-openpgp-tool " PACKAGE_VERSION;
const char *argp_program_bug_address = PACKAGE_BUGREPORT;

static char doc[] =
"chop-openpgp-tool -- display information about OpenPGP keys\n";

static struct argp_option options[] =
  {
    { "verbose", 'v', 0, 0,        "Produce verbose output" },
    { "openpgp-pubkey", 'o', "PUBKEY-FILE", 0,
      "Use PUBKEY-FILE as the OpenPGP key to be used during TLS "
      "authentication" },
    { "openpgp-privkey", 'O', "PRIVKEY-FILE", 0,
      "Use PRIVKEY-FILE as the OpenPGP key to be used during TLS "
      "authentication" },
    { "raw", 'r', NULL, 0,
      "Import keys in \"raw\" binary format rather than ASCII-armored" },

    /* What should be displayed.  */
    { "all", 'a', NULL, 0,
      "Show all possible information for the given key pair" },
    { "id", 'i', NULL, 0,
      "Show public key ID" },
    { "fingerprint", 'f', NULL, 0,
      "Show fingerprint of the given public key" },
    { "names", 'n', NULL, 0,
      "Show names of the given public key" },
    { "pk-algo", 'p', NULL, 0,
      "Show the public key algorithm of the given keys" },
    { "key-usage", 'u', NULL, 0,
      "Show the intended key usage" },

    { 0, 0, 0, 0, 0 }
  };

static int verbose = 0;

/* OpenPGP key pair for OpenPGP authentication.  */
static char *openpgp_pubkey_file = NULL;
static char *openpgp_privkey_file = NULL;
static gnutls_openpgp_key_fmt_t openpgp_key_format = GNUTLS_OPENPGP_FMT_BASE64;

/* What to show.  */
static int show_all = 0;
static int show_id = 0;
static int show_fingerprint = 0;
static int show_names = 0;
static int show_pk_algorithm = 0;
static int show_key_usage = 0;



/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case 'v':
      verbose = 1;
      break;
    case 'o':
      openpgp_pubkey_file = arg;
      break;
    case 'O':
      openpgp_privkey_file = arg;
      break;
    case 'r':
      openpgp_key_format = GNUTLS_OPENPGP_FMT_RAW;
      break;
    case 'a':
      show_all = 1;
      break;
    case 'i':
      show_id = 1;
      break;
    case 'f':
      show_fingerprint = 1;
      break;
    case 'n':
      show_names = 1;
      break;
    case 'p':
      show_pk_algorithm = 1;
      break;
    case 'u':
      show_key_usage = 1;
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

/* Argp argument parsing.  */
static struct argp argp = { options, parse_opt, 0, doc };


/* Importing keys from files.  */

static int
load_file (const char *file, gnutls_datum_t *content)
{
  int err, fd = -1;
  struct stat stat;
  void *mapped_content = MAP_FAILED;

  fd = open (file, O_RDONLY);
  if (fd == -1)
    goto failed;

  err = fstat (fd, &stat);
  if (err == -1)
    goto failed;

  mapped_content = mmap (NULL, stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (mapped_content == MAP_FAILED)
    goto failed;

  content->data = (unsigned char *)mapped_content;
  content->size = (unsigned int)stat.st_size;

  return 0;

 failed:
  err = errno;
  if (mapped_content != MAP_FAILED)
    munmap (mapped_content, stat.st_size);
  if (fd >= 0)
    close (fd);

  return err;
}

static void
unload_file (gnutls_datum_t *content)
{
  munmap (content->data, (size_t)content->size);
  content->data = NULL;
  content->size = 0;
}

static int
import_openpgp_keys (const char *pubkey_file,
		     gnutls_openpgp_key_t pubkey,
		     const char *privkey_file,
		     gnutls_openpgp_privkey_t privkey,
		     gnutls_openpgp_key_fmt_t format)
{
  int err;
  gnutls_datum_t key_content = { NULL, 0 };

  err = load_file (pubkey_file, &key_content);
  if (err)
    goto failed;

  err = gnutls_openpgp_key_import (pubkey, &key_content, format);
  if (err)
    goto failed;

  unload_file (&key_content);

  if (privkey_file)
    {
      err = load_file (privkey_file, &key_content);
      if (err)
	goto failed;

      err = gnutls_openpgp_privkey_import (privkey, &key_content,
					   format,
					   "" /* FIXME: password */,
					   0 /* flags? */);
      if (err)
	goto failed;

      unload_file (&key_content);
    }

  return 0;

 failed:
  if (key_content.data)
    unload_file (&key_content);

  return err;
}


/* Displaying information about keys.  */

static int
show_key_id (gnutls_openpgp_key_t key)
{
  int err;
  unsigned char id[8];

  err = gnutls_openpgp_key_get_id (key, id);
  if (!err)
    {
      char ascii[17];

      chop_buffer_to_hex_string ((char *)id, sizeof (id), ascii);
      printf ("ID:\t%s\n", ascii);
    }

  return err;
}

static int
show_key_fingerprint (gnutls_openpgp_key_t key)
{
  int err;
  char fpr[4096];
  size_t fprlen = 0;/*  = sizeof (fpr); */

  err = gnutls_openpgp_key_get_fingerprint (key, fpr, &fprlen);
  if (!err)
    {
      char fpr_ascii[8193];
      if (fprlen > sizeof (fpr))
	abort ();

      chop_buffer_to_hex_string (fpr, fprlen, fpr_ascii);
      printf ("fingerprint:\t%s\n", fpr_ascii);
    }

  return err;
}

static int
show_pubkey_pk_algorithm (gnutls_openpgp_key_t pubkey)
{
  gnutls_pk_algorithm_t algo;
  unsigned int bits;

  algo = gnutls_openpgp_key_get_pk_algorithm (pubkey, &bits);

  printf ("pubkey PK algorithm:\t%s\t%u bits\n",
	  gnutls_pk_algorithm_get_name (algo), bits);

  return 0;
}

static int
show_privkey_pk_algorithm (gnutls_openpgp_privkey_t pubkey)
{
  gnutls_pk_algorithm_t algo;
  unsigned int bits;

  algo = gnutls_openpgp_privkey_get_pk_algorithm (pubkey, &bits);

  printf ("privkey PK algorithm:\t%s\t%u bits\n",
	  gnutls_pk_algorithm_get_name (algo), bits);

  return 0;
}

static int
show_key_names (gnutls_openpgp_key_t pubkey)
{
  int err, i;
  char name[4096];
  size_t name_len;

  for (i = 0; 1; i++)
    {
      name_len = sizeof (name);

      err = gnutls_openpgp_key_get_name (pubkey, i, name, &name_len);
      if (err)
	break;

      if (name_len > sizeof (name))
	abort ();

      printf ("name %u:\t%s\n", i, name);
    }

  return (i == 0) ? err : 0;
}

static int
show_pubkey_usage (gnutls_openpgp_key_t pubkey)
{
  int err;
  unsigned int key_usage;

  err = gnutls_openpgp_key_get_key_usage (pubkey, &key_usage);
  if (!err)
    {
      printf ("key usage:");

      if (key_usage & GNUTLS_KEY_DIGITAL_SIGNATURE)
	printf ("\tdigital-signature");
      if (key_usage & GNUTLS_KEY_NON_REPUDIATION)
	printf ("\tnon-repudation");
      if (key_usage & GNUTLS_KEY_KEY_ENCIPHERMENT)
	printf ("\tkey-encipherment");
      if (key_usage & GNUTLS_KEY_DATA_ENCIPHERMENT)
	printf ("\tdata-encipherment");
      if (key_usage & GNUTLS_KEY_KEY_AGREEMENT)
	printf ("\tkey-agreement");
      if (key_usage & GNUTLS_KEY_KEY_CERT_SIGN)
	printf ("\tcertificate-signature");
      if (key_usage & GNUTLS_KEY_CRL_SIGN)
	printf ("\tcrl-signature");
      if (key_usage & GNUTLS_KEY_ENCIPHER_ONLY)
	printf ("\tencipher-only");

      printf ("\n");
    }

  return err;
}



/* The program.  */
int
main (int argc, char *argv[])
{
#define handle_error(err, action)		\
  if (err)					\
    {						\
      fprintf (stderr, "%s while " action "\n",	\
	       gnutls_strerror (err));		\
      return 1;					\
    }

  int err;
  errcode_t cerr;
  gnutls_openpgp_key_t pubkey;
  gnutls_openpgp_privkey_t privkey;

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, 0, 0);

  if (!openpgp_pubkey_file)
    {
      fprintf (stderr, "You must specify at least a public key "
	       "certificate, with `-o'.\n");
      return 1;
    }

  cerr = chop_init ();
  if (cerr)
    {
      com_err (argv[0], cerr, "while initializing libchop");
      return 1;
    }

  err = gnutls_global_init ();
  handle_error (err, "initializing GnuTLS");
  err = gnutls_global_init_extra ();
  handle_error (err, "initializing GnuTLS-extra");

  err = gnutls_openpgp_key_init (&pubkey);
  handle_error (err, "initializing public key");
  err = gnutls_openpgp_privkey_init (&privkey);
  handle_error (err, "initializing private key");

  err = import_openpgp_keys (openpgp_pubkey_file, pubkey,
			     openpgp_privkey_file, privkey,
			     openpgp_key_format);
  handle_error (err, "importing keys");

  if (show_all || show_id)
    {
      err = show_key_id (pubkey);
      handle_error (err, "retrieving public key ID");
    }

  if (show_all || show_fingerprint)
    {
      err = show_key_fingerprint (pubkey);
      handle_error (err, "retrieving key fingerprint");
    }

  if (show_all || show_names)
    {
      err = show_key_names (pubkey);
      handle_error (err, "retrieving key names");
    }

  if (show_all || show_pk_algorithm)
    {
      err = show_pubkey_pk_algorithm (pubkey);
      handle_error (err, "retrieving public key PK algorithm");

      if (openpgp_privkey_file)
	{
	  err = show_privkey_pk_algorithm (privkey);
	  handle_error (err, "retrieving private key PK algorithm");
	}
    }

  if (show_all || show_key_usage)
    {
      err = show_pubkey_usage (pubkey);
      handle_error (err, "retrieving private key PK algorithm");
    }

  gnutls_openpgp_key_deinit (pubkey);
  gnutls_openpgp_privkey_deinit (privkey);

  return 0;

#undef handle_error
}

/* arch-tag: fbc435b5-5b14-4af2-86a9-f574a83cab2a
 */
