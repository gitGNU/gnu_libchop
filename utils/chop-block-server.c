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

/* A simple block server over SunRPCs.  It actually proxies a local block
   store, e.g. a GDBM block store, and serves it remotely.  A lot of code is
   borrowed from `chop-archiver.c'.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/hash.h>
#include <chop/stores.h>
#include <chop/filters.h>
#include <chop/block-server.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <rpc/pmap_clnt.h>
#include <string.h>
#include <memory.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/poll.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <progname.h>
#include <errno.h>
#include <assert.h>

#ifdef HAVE_GNUTLS
# include <gnutls/gnutls.h>
# include <gnutls/extra.h>
# include <gnutls/openpgp.h>

# include <chop/sunrpc-tls.h>

# include "gnutls-helper.h"
#endif

#if (defined HAVE_PTHREAD_H) && (defined HAVE_AVAHI)
# define USE_AVAHI 1
#endif

#ifndef SIG_PF
#define SIG_PF void(*)(int)
#endif

/* Whether to be verbose */
static int verbose = 0;

/* Name of the directory for configuration files under `$HOME'.  */
#define CONFIG_DIRECTORY ".chop-block-server"

/* Configuration file names.  */
#define CONFIG_TLS_RSA_PARAMS  "tls-rsa-params"
#define CONFIG_TLS_DH_PARAMS   "tls-dh-params"

/* The local block store being proxied.  */
static chop_block_store_t *local_store = NULL;

/* The local store file name.  */
static char *local_store_file_name = NULL;

/* The protocol underlying SunRPC: UDP or TCP.  */
static long protocol_type = IPPROTO_TCP;

#ifdef HAVE_GNUTLS
/* Whether to use RPC over TLS.  */
static int use_tls = 0;

/* An OpenPGP key pair for OpenPGP-based authentication.  */
static char *tls_openpgp_pubkey_file = NULL;
static char *tls_openpgp_privkey_file = NULL;
#define tls_use_openpgp_authentication				\
  ((tls_openpgp_pubkey_file) && (tls_openpgp_privkey_file))

#endif

/* The name of the content hash algorithm enforced (if any).  */
static chop_hash_method_t content_hash_enforced = CHOP_HASH_NONE;

/* Whether block/key collision checking should turned off.  */
static int no_collision_check = 0;

/* Address we should bind to and listen from.  */
static char *binding_address = NULL;

/* Port the service is to be attached to.  */
static unsigned service_port = 0;

#ifdef USE_AVAHI
/* Whether to publish the service locally using Avahi.  */
static int no_service_publication = 0;

/* Service name.  */
static char *service_name = NULL;
#endif


static void info (const char *, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 1, 2)))
#endif
     ;

#ifdef USE_AVAHI
# include <pthread.h>
#endif



/* Output.  */

static void
info (const char *fmt, ...)
{
  va_list args;
  char *newfmt;

  newfmt = (char *)alloca (strlen (program_name) + strlen (fmt) + 10);
  strcpy (newfmt, program_name);
  strcat (newfmt, ": ");
  strcat (newfmt, fmt);
  strcat (newfmt, "\n");

  va_start (args, fmt);
  vfprintf (stderr, newfmt, args);
  va_end (args);
}


/* Return true (non-zero) if KEY is a hash of BUFFER using hash method
   METHOD.  */
static int
is_valid_hash_key (const chop_block_key_t *key,
		   const char *buffer, size_t size,
		   chop_hash_method_t method)
{
  char *hash;
  size_t hash_size;

  if (method == CHOP_HASH_NONE)
    return 1;

  hash_size = chop_hash_size (method);
  if (hash_size != chop_block_key_size (key))
    return 0;

  hash = alloca (hash_size);
  chop_hash_buffer (method, buffer, size, hash);
  return (!memcmp (chop_block_key_buffer (key), hash, hash_size));
}

#define VALIDATE_HASH_KEY(key, argp)					\
  if (!is_valid_hash_key (&key,						\
			  argp->block.chop_rblock_content_t_val,	\
			  argp->block.chop_rblock_content_t_len,	\
			  content_hash_enforced))			\
    {									\
      char *hex_key;							\
									\
      hex_key = alloca (chop_block_key_size (&key) * 2 + 1);		\
      chop_buffer_to_hex_string (chop_block_key_buffer (&key),		\
				 chop_block_key_size (&key),		\
				 hex_key);				\
      info ("%s: key %s: "						\
	    "violating %s content-hashing, rejected",			\
	    __FUNCTION__,						\
	    hex_key, chop_hash_method_name (content_hash_enforced));	\
      result = -1;							\
      return &result;							\
    }


/* The RPC handlers.  */

static inline void
display_request_info (const char *name, struct svc_req *req)
{
  if (verbose)
    {
      struct sockaddr_in *caller;
      unsigned long ip_as_int;
      char caller_ip[200];
      SVCXPRT *xprt = req->rq_xprt;

      caller = svc_getcaller (xprt);
      assert (caller != NULL);

      ip_as_int = ntohl (caller->sin_addr.s_addr);
      snprintf (caller_ip, sizeof (caller_ip), "%lu.%lu.%lu.%lu",
		(ip_as_int & 0xff000000UL) >> 24,
		(ip_as_int & 0x00ff0000UL) >> 16,
		(ip_as_int & 0x0000ff00UL) >>  8,
		(ip_as_int & 0x000000ffUL));

#ifdef HAVE_GNUTLS
      {
	int err;
	unsigned char caller_key_id[8], caller_key_id_ascii[17];
	gnutls_openpgp_key_t peer_key;
	gnutls_session_t session;

	err = svctls_getsession (req->rq_xprt, &session);
	if (!err)
	  {
	    peer_key = gnutls_session_get_ptr (session);

	    /* Show the peer's key fingerprint.  */
	    err = gnutls_openpgp_key_get_id (peer_key, caller_key_id);
	    if (err)
	      strcpy ((char *) caller_key_id_ascii, "???");
	    else
	      chop_buffer_to_hex_string ((char *) caller_key_id,
					 sizeof (caller_key_id),
					 (char *) caller_key_id_ascii);
	  }
	else
	  strcpy ((char *) caller_key_id_ascii, "???");

	info ("request `%s' from peer ID %s (IP %s)",
	      name, caller_key_id_ascii, caller_ip);
      }
#else
      info ("request `%s' from IP %s", name, caller_ip);
#endif
    }
}

static int *
handle_say_hello (char **argp, struct svc_req *req)
{
  static int result = 1;

  display_request_info ("say_hello", req);

  return &result;
}

static int *
handle_block_exists (chop_rblock_key_t *argp, struct svc_req *req)
{
  static int result = 0;
  chop_error_t err;
  int exists;
  chop_block_key_t key;

  display_request_info ("block_exists", req);

  chop_block_key_init (&key, argp->chop_rblock_key_t_val,
		       argp->chop_rblock_key_t_len, NULL, NULL);
  err = chop_store_block_exists (local_store, &key, &exists);
  if (err)
    result = 0;
  else
    result = exists;

  return &result;
}

static int *
handle_write_block (block_store_write_block_args *argp, struct svc_req *req)
{
  static int result;
  chop_error_t err;
  chop_block_key_t key;

  display_request_info ("write_block", req);

  chop_block_key_init (&key, argp->key.chop_rblock_key_t_val,
		       argp->key.chop_rblock_key_t_len, NULL, NULL);
  VALIDATE_HASH_KEY (key, argp);

  if (!no_collision_check)
    {
      chop_buffer_t buffer;
      int leave = 0;
      size_t read;

      chop_buffer_init (&buffer, 0);
      err = chop_store_read_block (local_store, &key, &buffer, &read);
      switch (err)
	{
	case CHOP_STORE_BLOCK_UNAVAIL:
	  /* Nothing is available under KEY.  */
	  result = 0;
	  break;

	case 0:
	  /* Check whether the block currently stored under KEY is the same
	     as the one passed by the caller.  */
	  if ((argp->block.chop_rblock_content_t_len
	       != chop_buffer_size (&buffer))
	      || (memcmp (chop_buffer_content (&buffer),
			  argp->block.chop_rblock_content_t_val,
			  chop_buffer_size (&buffer))))
	    {
	      char *hex_key;
	      hex_key = alloca (chop_block_key_size (&key) * 2 + 1);
	      chop_buffer_to_hex_string (chop_block_key_buffer (&key),
					 chop_block_key_size (&key),
					 hex_key);
	      info ("key %s: collision detected (and rejected)", hex_key);
	      result = -3;
	    }
	  else
	    /* No collision detected, nothing to write.  */
	    result = 0;

	  /* At any rate, we can leave here.  */
	  leave = 1;
	  break;

	default:
	  info ("underlying store returned unexpectedly (%i: %s)",
		(int) err, chop_error_message (err));

	  /* Ignore the problem and try to write to LOCAL_STORE.  This makes
	     sense since, for instance, the dummy block store can return
	     `CHOP_ERR_NOT_IMPL' on `read_block' requests.  */
	  result = 0;
	}

      chop_buffer_return (&buffer);
      if (leave)
	return &result;
    }

  err = chop_store_write_block (local_store, &key,
				argp->block.chop_rblock_content_t_val,
				argp->block.chop_rblock_content_t_len);
  if (err)
    result = -1;
  else
    result = 0;

  return &result;
}

static block_store_read_block_ret *
handle_read_block (chop_rblock_key_t *argp, struct svc_req *req)
{
  static block_store_read_block_ret result;
  chop_error_t err;
  size_t read;
  chop_block_key_t key;
  chop_buffer_t buffer;

  display_request_info ("read_block", req);

#define DO_FAIL(_ret)					\
  do							\
  {							\
    result.status = (_ret);				\
    result.block.chop_rblock_content_t_len = 0;		\
    result.block.chop_rblock_content_t_val = NULL;	\
  }							\
  while (0)

  chop_block_key_init (&key, argp->chop_rblock_key_t_val,
		       argp->chop_rblock_key_t_len, NULL, NULL);
  chop_buffer_init (&buffer, 1024);
  err = chop_store_read_block (local_store, &key, &buffer, &read);
  if (err)
    DO_FAIL (err);
  else
    {
      result.status = 0;
      if (read > 0)
	{
	  /* FIXME:  When does it get freed?  */
	  result.block.chop_rblock_content_t_val =
	    malloc (read);
	  memcpy (result.block.chop_rblock_content_t_val,
		  chop_buffer_content (&buffer), read);
	  result.block.chop_rblock_content_t_len = read;
	}
      else
	result.block.chop_rblock_content_t_val = NULL;
    }

#undef DO_FAIL

  chop_buffer_return (&buffer);

  return &result;
}

static int *
handle_sync (void *unused, struct svc_req *req)
{
  static int result;
  chop_error_t err;

  display_request_info ("sync", req);

  err = chop_store_sync (local_store);
  result = err ? -1 : 0;

  return &result;
}

static int *
handle_close (void *unused, struct svc_req *req)
{
  static int result = 0;
  chop_error_t err;

  display_request_info ("close", req);

  /* Never actually close the local store.  */
  err = chop_store_sync (local_store);
  result = err ? -1 : 0;

  return &result;
}


/* Local store management.  */
static chop_error_t
open_db_store (const chop_file_based_store_class_t *class,
	       const char *file, chop_block_store_t *store)
{
  chop_error_t err;

  err = chop_file_based_store_open (class, file,
				    O_RDWR | O_CREAT, S_IRUSR | S_IWUSR,
				    store);
  if (err)
    chop_error (err, "while opening `%s' data file \"%s\"",
		chop_class_name ((chop_class_t *) class), file);

  return err;
}


/* RPC initialization.  */


/* Bind to ADDRESS on port PORT and listen there.  Return the corresponding
   socket or -1 on failure.  */
static int
listen_there (const char *address, unsigned port, int socket_type)
{
  int sock = -1;
  struct hostent *he;
  struct sockaddr_in addr;
  socklen_t addr_len;

  do
    {
      he = gethostbyname (binding_address);
    }
  while ((!he) && (h_errno == TRY_AGAIN));

  if (!he)
    {
      info ("gethostbyname: %s", strerror (errno));
      goto fail;
    }

  /* XXX: We don't support IPv6 currently.  */
  assert (he->h_addrtype == AF_INET);
  assert (he->h_length == sizeof (struct in_addr));

  addr_len = he->h_length;
  addr.sin_family = AF_INET;
  addr.sin_port = htons (service_port);

  addr.sin_addr = *(struct in_addr *)he->h_addr;

  sock = socket (he->h_addrtype, socket_type, 0);
  if (sock < 0)
    {
      info ("socket: %s", strerror (errno));
      goto fail;
    }

  if (bind (sock, (struct sockaddr *)&addr, sizeof (addr)))
    {
      info ("bind: %s", strerror (errno));
      goto fail;
    }

  if (socket_type == SOCK_STREAM)
    {
      if (listen (sock, 0))
	{
	  info ("listen: %s", strerror (errno));
	  goto fail;
	}
    }

  return sock;

 fail:
  if (sock >= 0)
    close (sock);

  return -1;
}



#ifdef HAVE_GNUTLS

/* TLS static parameters (i.e., re-used accross connections).  */

/* Anonymous authentication.  */
static gnutls_anon_server_credentials_t server_anoncred;
static gnutls_dh_params_t               server_dh_params;
#define DH_BITS 1024

/* OpenPGP authentication.  */
static gnutls_certificate_credentials_t server_certcred;
static gnutls_rsa_params_t              server_rsa_params;

static int
make_tls_session (gnutls_session *session, void *closure)
{
  /* We pretty much always want message authentication.  */
  static const int mac_prio[] =
    { GNUTLS_MAC_SHA1, GNUTLS_MAC_RMD160, GNUTLS_MAC_MD5, 0 };

  /* Often, we won't need encryption at all because the data being stored is
     already encrypted.  However, there is no anonymous authentication
     ciphersuite that supports `NULL' encryption.  */
  static const int cipher_prio[] =
    { GNUTLS_CIPHER_NULL, GNUTLS_CIPHER_ARCFOUR_128,
      GNUTLS_CIPHER_AES_128_CBC, GNUTLS_CIPHER_AES_256_CBC, 0 };

  /* Likewise, we will rarely need compression.  */
  static const int compression_prio[] =
    { GNUTLS_COMP_NULL, GNUTLS_COMP_DEFLATE, 0 };

  int err;

  info ("preparing new TLS session for incoming connection");
  err = gnutls_init (session, GNUTLS_SERVER);
  if (err)
    return -1;

  gnutls_set_default_priority (*session);

  if (tls_use_openpgp_authentication)
    {
      /* OpenPGP authentication.  */
      static const int cert_type_priority[2] = { GNUTLS_CRT_OPENPGP, 0 };
      static const int kx_prio[] =
	{ GNUTLS_KX_RSA, GNUTLS_KX_RSA_EXPORT, GNUTLS_KX_DHE_RSA,
	  GNUTLS_KX_DHE_DSS, 0 };

      /* Require OpenPGP authentication.  */
      gnutls_certificate_type_set_priority (*session, cert_type_priority);

      gnutls_credentials_set (*session, GNUTLS_CRD_CERTIFICATE,
			      server_certcred);

      gnutls_kx_set_priority (*session, kx_prio);

      /* Request client certificate if any.  */
      gnutls_certificate_server_set_request (*session, GNUTLS_CERT_REQUEST);
    }
  else
    {
      /* Anonymous authentication.  */
      static const int kx_prio[] = { GNUTLS_KX_ANON_DH, 0 };

      gnutls_credentials_set (*session, GNUTLS_CRD_ANON, server_anoncred);
      gnutls_kx_set_priority (*session, kx_prio);
    }

  gnutls_mac_set_priority (*session, mac_prio);
  gnutls_cipher_set_priority (*session, cipher_prio);
  gnutls_compression_set_priority (*session, compression_prio);

  gnutls_dh_set_prime_bits (*session, DH_BITS);

  return 0;
}

static void
finalize_tls_session (gnutls_session_t session, void *closure)
{
  if (tls_use_openpgp_authentication)
    {
      int err;
      unsigned char caller_key_id[8], caller_key_id_ascii[17];
      gnutls_openpgp_key_t peer_key;

      peer_key = gnutls_session_get_ptr (session);

      if (peer_key != NULL)
	{
	  /* Show the peer's key fingerprint.  */
	  err = gnutls_openpgp_key_get_id (peer_key, caller_key_id);
	  if (err)
	    strcpy ((char *) caller_key_id_ascii, "???");
	  else
	    chop_buffer_to_hex_string ((char *) caller_key_id,
				       sizeof (caller_key_id),
				       (char *) caller_key_id_ascii);

	  /* Finalize the previously imported key.  */
	  gnutls_openpgp_key_deinit (peer_key);
	}
      else
	strcpy ((char *) caller_key_id_ascii, "???");

      info ("finalizing session for peer ID %s", caller_key_id_ascii);
    }
  else
    info ("finalizing session");

  gnutls_deinit (session);
}

static void
initialize_tls_parameters (void)
{
  chop_error_t err;

  err = chop_tls_initialize_dh_params (&server_dh_params,
				       CONFIG_DIRECTORY,
				       CONFIG_TLS_DH_PARAMS);
  if (err)
    exit (1);

  gnutls_anon_allocate_server_credentials (&server_anoncred);
  gnutls_anon_set_server_dh_params (server_anoncred, server_dh_params);

  if (tls_use_openpgp_authentication)
    {
      err = gnutls_global_init_extra ();
      if (err)
	{
	  info ("failed to initialize GNUtls extra: %s",
		gnutls_strerror (err));
	  exit (1);
	}

      err = gnutls_certificate_allocate_credentials (&server_certcred);
      if (err)
	{
	  info ("failed to allocate certificate credentials: %s",
		gnutls_strerror (err));
	  exit (1);
	}

      err = gnutls_certificate_set_openpgp_key_file (server_certcred,
						     tls_openpgp_pubkey_file,
						     tls_openpgp_privkey_file,
						     GNUTLS_OPENPGP_FMT_RAW);
      if (err)
	{
	  info ("failed to use OpenPGP key pair: %s", gnutls_strerror (err));
	  exit (1);
	}

      err = chop_tls_initialize_rsa_params (&server_rsa_params,
					    CONFIG_DIRECTORY,
					    CONFIG_TLS_RSA_PARAMS);
      if (err)
	exit (1);

      gnutls_certificate_set_dh_params (server_certcred, server_dh_params);
      gnutls_certificate_set_rsa_export_params (server_certcred,
						server_rsa_params);

      info ("using TLS OpenPGP authentication");
    }
  else
    info ("using TLS anonymous authentication");

}

/* Decide whether or not to continue SESSION, based on who the peer is.
   Return non-zero if authorization is granted, zero otherwise.  */
static int
tls_authorizer (gnutls_session_t session, void *unused)
{
  int err;
  const gnutls_datum_t *peer_cert;
  unsigned int peer_cert_len;
  gnutls_openpgp_key_t peer_key;
  char fpr[4096], fpr_ascii[8193];
  size_t fpr_len = 0;

  if (!tls_use_openpgp_authentication)
    /* When using anonymous authentication, authorize anyone to use the
       service.  */
    return 1;

  peer_cert = gnutls_certificate_get_peers (session, &peer_cert_len);
  if (!peer_cert)
    {
      info ("client doesn't have an OpenPGP certificate, rejected");
      return 0;
    }

  assert (peer_cert_len == 1);

  err = gnutls_openpgp_key_init (&peer_key);
  if (err)
    goto failed;

  err = gnutls_openpgp_key_import (peer_key, peer_cert,
				   GNUTLS_OPENPGP_FMT_RAW);
  if (err)
    goto handle_error;

  /* Store the native key object for future reference.  */
  gnutls_session_set_ptr (session, peer_key);

  if (verbose)
    {
      /* Show cipher suite.  */
      const char *csuite;
      char name[2048];
      size_t name_len;
      gnutls_kx_algorithm_t kx = gnutls_kx_get (session);
      gnutls_cipher_algorithm_t cipher = gnutls_cipher_get (session);
      gnutls_mac_algorithm_t mac = gnutls_mac_get (session);

      csuite = gnutls_cipher_suite_get_name (kx, cipher, mac);
      assert (csuite != NULL);

      info ("cipher suite: %s", csuite);

      /* Show the peer's name.  */
      name_len = sizeof (name);
      err = gnutls_openpgp_key_get_name (peer_key, 0, name, &name_len);
      if (!err)
	info ("peer key first name: %s", name);
    }

  /* Show the peer's key fingerprint.  */
  err = gnutls_openpgp_key_get_fingerprint (peer_key, fpr, &fpr_len);
  if (err)
    goto handle_error;

  chop_buffer_to_hex_string (fpr, fpr_len, fpr_ascii);
  info ("peer key fingerprint: %s", fpr_ascii);

  /* FIXME: No actual attempt is made to verity whether the peer is
     authorized to use the service.  Eventually, we'd like to verify in a
     keyring whether the peer is trusted/authorized, etc.  */

  return 1;

 handle_error:
  info ("in %s: GNUtls error: %s", __FUNCTION__,
	gnutls_strerror (err));
  gnutls_session_set_ptr (session, NULL);
  gnutls_openpgp_key_deinit (peer_key);

 failed:

  return 0;
}

#endif /* HAVE_GNUTLS */


static SVCXPRT *
register_rpc_handlers (void)
{
  /* By default, we'll let `svcXXX_create ()' choose whatever port to listen
     from and whatever address to bind to.  */
  int listening_sock = RPC_ANYSOCK;
  long proto = protocol_type;
  SVCXPRT *transp;

  /* Register the handlers themselves.  */
  chop_block_server_say_hello_handler = handle_say_hello;
  chop_block_server_block_exists_handler = handle_block_exists;
  chop_block_server_write_block_handler = handle_write_block;
  chop_block_server_read_block_handler = handle_read_block;
  chop_block_server_sync_handler = handle_sync;
  chop_block_server_close_handler = handle_close;

  if ((binding_address != NULL) || (service_port != 0))
    {
      if (!binding_address)
	binding_address = "127.0.0.1";

      listening_sock = listen_there (binding_address, service_port,
				     (proto == IPPROTO_UDP)
				     ? SOCK_DGRAM : SOCK_STREAM);
      if (listening_sock < 0)
	{
	  info ("failed to bind and listen on %s:%u",
		binding_address, service_port);
	  exit (1);
	}
    }

/*   pmap_unset (BLOCK_STORE_PROGRAM, BLOCK_STORE_VERSION); */

#ifdef HAVE_GNUTLS
  if (use_tls)
    {
      svctls_init_if_needed ();

      initialize_tls_parameters ();
      transp = svctls_create (make_tls_session, NULL,
			      finalize_tls_session, NULL,
			      tls_authorizer, NULL,
			      listening_sock, 0, 0);
    }
  else
#endif
  if (proto == IPPROTO_TCP)
    transp = svctcp_create (listening_sock, 0, 0);
  else
    transp = svcudp_create (listening_sock);

  if (transp == NULL)
    {
      info ("cannot create RPC service");
      exit (1);
    }

  if (!service_port)
    /* Update the service port so that the service publication via Avahi gets
       it right.  */
    service_port = transp->xp_port;

  if (!svc_register (transp, BLOCK_STORE_PROGRAM, BLOCK_STORE_VERSION,
		     chop_block_server_process_request,
		     0 /* don't register with portmap */))
    {
      info ("unable to register service");
      exit (1);
    }

  return transp;
}



/* Option parsing.  */

#include <argp.h>

const char *argp_program_version = "chop-block-server (" PACKAGE_NAME ") " PACKAGE_VERSION;
const char *argp_program_bug_address = PACKAGE_BUGREPORT;

static const char doc[] =
"chop-block-server -- serves block store RPCs\
\v\
This program serves block store RPCs by proxying LOCAL-BLOCK-STORE, a local \
file-based block store.";

static char args_doc[] = "LOCAL-BLOCK-STORE";

/* Use the dummy store for debugging purposes.  */
static int debugging = 0;

/* The zip/unzip filter classes to be used.  */
static const chop_zip_filter_class_t   *zip_filter_class = NULL;
static const chop_unzip_filter_class_t *unzip_filter_class = NULL;


#ifdef HAVE_GPERF
static char *file_based_store_class_name = "gdbm_block_store";
#endif

static struct argp_option options[] =
  {
    { "verbose", 'v', 0, 0,        "Produce verbose output" },
    { "debug",   'd', 0, 0,
      "Produce debugging output and use a dummy block store (i.e. a block "
      "store that does nothing but print messages)" },
    { "zip",     'z', "ZIP-TYPE", OPTION_ARG_OPTIONAL,
      "Pass data through a ZIP-TYPE filter to compress (resp. decompress) "
      "data when writing (resp. reading) to (resp. from) the archive.  "
      "ZIP-TYPE may be one of `zlib', `bzip2' or `lzo', for instance." },
#ifdef HAVE_GPERF
    { "store",   'S', "CLASS", 0,
      "Use CLASS as the underlying file-based block store" },
#endif
    /* Content hashing.  */
    { "enforce-hash", 'H', "ALGO", 0,
      "Enforce content-hash algorithm ALGO" },
    { "no-collision-check", 'C', 0, 0,
      "Turn off block/key collision checks" },
#ifdef USE_AVAHI
    { "no-publication", 'n', 0, 0,
      "Turn off service publication on the LAN via Avahi" },
    { "service-name", 's', "NAME", 0,
      "Choose NAME as the service name to be published" },
#endif

    /* Network.  */
    { "restrict",'R', "HOSTS", 0,
      "Restrict access to HOSTS, a comma-separated list of hostnames." },
    { "protocol",'P', "PROTO", 0,
      "Serve RPCs over PROTO, either \"tcp\" or \"udp\"" },
    { "port", 'p', "PORT", 0,
      "Run the service on port PORT" },
    { "address", 'a', "ADDRESS", 0,
      "Bind to the address (and port) specified by ADDRESS" },
#ifdef HAVE_GNUTLS
    { "tls", 't', 0, 0,
      "Use RPCs over TLS (recommended)" },
    { "openpgp-pubkey", 'o', "PUBKEY-FILE", 0,
      "Use the OpenPGP public key from PUBKEY-FILE for authentication." },
    { "openpgp-privkey", 'O', "PRIVKEY-FILE", 0,
      "Use the OpenPGP private key from PRIVKEY-FILE for authentication." },
#endif

    { 0, 0, 0, 0, 0 }
  };


/* Dealing with zip/unzip filter classes.  */
#include "zip-helper.c"

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case 'v':
      verbose = 1;
      break;
    case 'd':
      debugging = 1;
      break;
    case 'z':
      get_zip_filter_classes (arg, &zip_filter_class, &unzip_filter_class);
      break;

    case 'R':
      /* FIXME:  Do it */
      abort ();
      break;

    case 'P':
      if (!strcasecmp (arg, "tcp"))
	protocol_type = IPPROTO_TCP;
      else if (!strcasecmp (arg, "udp"))
	protocol_type = IPPROTO_UDP;
      else
	{
	  info ("%s: unknown protocol", arg);
	  exit (1);
	}

      break;

#ifdef HAVE_GNUTLS
    case 't':
      use_tls = 1;
      break;
    case 'o':
      use_tls = 1, tls_openpgp_pubkey_file = arg;
      break;
    case 'O':
      use_tls = 1, tls_openpgp_privkey_file = arg;
      break;
#endif

    case 'a':
      binding_address = arg;
      break;

    case 'p':
      service_port = atoi (arg);
      break;

#ifdef HAVE_GPERF
    case 'S':
      file_based_store_class_name = arg;
      break;
#endif

    case 'C':
      no_collision_check = 1;
      break;
    case 'H':
      if (chop_hash_method_lookup (arg, &content_hash_enforced))
	{
	  info ("%s: unknown hash method name", arg);
	  exit (1);
	}
      break;

#ifdef USE_AVAHI
    case 'n':
      no_service_publication = 1;
      break;
    case 's':
      service_name = strdup (arg);
      break;
#endif

    case ARGP_KEY_ARG:
      if (state->arg_num >= 1)
	/* Too many arguments. */
	argp_usage (state);

      local_store_file_name = arg;
      break;

    case ARGP_KEY_END:
      if (state->arg_num < 1)
	/* Not enough arguments. */
	argp_usage (state);
      break;


    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

/* Argp argument parsing.  */
static struct argp argp = { options, parse_opt, args_doc, doc };


#ifdef HAVE_AVAHI
/* Service publication.  */

static void *
publishing_thread_entry_point (void *pub)
{
  chop_store_publisher_loop ((chop_store_publisher_t *) pub);

  free (pub);

  return NULL;
}

static chop_error_t
publish_service (void)
{
  chop_error_t err;
  chop_hash_method_spec_t hash_spec;
  chop_store_publisher_t *publisher;

  publisher = (chop_store_publisher_t *)
    malloc (chop_class_instance_size (&chop_avahi_store_publisher_class));

  if (!publisher)
    return ENOMEM;

  if (!service_name)
    {
      /* Choose a service name of the form `user@host'.  */
      char *name, *host;
      size_t total_size;
#ifdef HAVE_CUSERID
      char *at;

      total_size = L_cuserid + 1024 + 2;
      name = (char *)alloca (total_size);
      cuserid (name);
      at = name + strlen (name);
      host = at + 1;

      *at = '@';
#else
      host = name = (char *)alloca (1024);
      total_size = 1024;
#endif

      if (gethostname (host, total_size -1 - ((size_t)(host - name))))
	strcpy  (host, "anonymous-block-server");
      else
	/* Just to make sure...  */
	name[total_size - 1] = '\0';

      service_name = strdup (name);
    }

  hash_spec.method = content_hash_enforced;
  if (content_hash_enforced != CHOP_HASH_NONE)
    hash_spec.spec_type = CHOP_HASH_SPEC_SPECIFIED;
  else
    hash_spec.spec_type = CHOP_HASH_SPEC_ANY;

  err = chop_avahi_store_publisher_open (service_name, NULL /* host */,
					 service_port, hash_spec,
#ifdef HAVE_GNUTLS
					 use_tls,
					 /* FIXME: Publish the fingerprint */
					 NULL, 0,
#else
					 0, NULL, 0,
#endif /* HAVE_GNUTLS */
					 publisher);
  if (!err)
    {
      /* Spawn a publishing thread.  */
      pthread_t publishing_thread;

      if (verbose)
	chop_log_attach (chop_avahi_store_publisher_log (publisher),
			 2, 0);

      err = pthread_create (&publishing_thread, NULL,
			    publishing_thread_entry_point,
			    publisher);
      if (!err)
	info ("publishing service `%s'", service_name);
    }


  return err;
}

#endif /* HAVE_AVAHI */


/* The program.  */

int
main (int argc, char *argv[])
{
  chop_error_t err;
  chop_filter_t *input_filter = NULL, *output_filter = NULL;
  SVCXPRT *transp;

  set_program_name (argv[0]);

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, 0, 0);

  err = chop_init ();
  if (err)
    {
      chop_error (err, "while initializing libchop");
      return 1;
    }

  if (!debugging)
    {
      /* Use two GDBM stores.  */
      const chop_file_based_store_class_t *db_store_class;
#ifdef HAVE_GPERF
      db_store_class = (chop_file_based_store_class_t *)
	chop_class_lookup (file_based_store_class_name);
      if (!db_store_class)
	{
	  info ("class `%s' not found", file_based_store_class_name);
	  exit (1);
	}
      if (chop_object_get_class ((chop_object_t *)db_store_class)
	  != &chop_file_based_store_class_class)
	{
	  info ("class `%s' is not a file-based store class",
		file_based_store_class_name);
	  exit (1);
	}
#else
      db_store_class = &chop_gdbm_block_store_class;
#endif

      local_store = (chop_block_store_t *)
	chop_class_alloca_instance ((chop_class_t *)db_store_class);

      err = open_db_store (db_store_class, local_store_file_name,
			   local_store);
      if (err)
	exit (3);
    }
  else
    {
      /* Use a dummy block store for debugging purposes */
      local_store = (chop_block_store_t *)
	chop_class_alloca_instance (&chop_dummy_block_store_class);

      chop_dummy_block_store_open ("data", local_store);
      chop_log_attach (chop_dummy_block_store_log (local_store), 2, 0);
    }

  if (zip_filter_class)
    {
      /* Create a filtered store that uses zip filters and proxies the
	 block store for data (not metadata).  */
      chop_block_store_t *raw_store = local_store;

      local_store =
	chop_class_alloca_instance (&chop_filtered_block_store_class);
      input_filter = chop_class_alloca_instance ((chop_class_t *)
						 zip_filter_class);
      output_filter = chop_class_alloca_instance ((chop_class_t *)
						  unzip_filter_class);

      err = chop_zip_filter_generic_open (zip_filter_class,
					  CHOP_ZIP_FILTER_DEFAULT_COMPRESSION,
					  0, input_filter);
      if (!err)
	err = chop_unzip_filter_generic_open (unzip_filter_class,
					      0, output_filter);

      if (err)
	{
	  chop_error (err, "while initializing zip/unzip filters");
	  exit (4);
	}

      if (verbose)
	{
	  /* Dump the zip filters' logs to `stderr'.  */
	  chop_log_t *log;
	  log = chop_filter_log (input_filter);
	  chop_log_attach (log, 2, 0);
	  log = chop_filter_log (output_filter);
	  chop_log_attach (log, 2, 0);
	}

      err = chop_filtered_store_open (input_filter, output_filter,
				      raw_store,
				      CHOP_PROXY_EVENTUALLY_DESTROY,
				      local_store);
      if (err)
	{
	  chop_error (err, "while initializing filtered store");
	  exit (5);
	}
    }

  transp = register_rpc_handlers ();

#ifdef USE_AVAHI
  if (!no_service_publication)
    {
      err = publish_service ();
      if (err)
	{
	  chop_error (err, "while trying to publish service");
	  exit (6);
	}
    }
#endif

  /* Go ahead.  */
  info ("server is up and running");
  svc_run ();

  /* Never reached.  */
  /* XXX:  Move to a signal handler?  */
  err = chop_store_close ((chop_block_store_t *)local_store);
  if (err)
    {
      chop_error (err, "while closing output block store");
      exit (7);
    }

  return 0;
}
