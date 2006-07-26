/* A simple block server over SunRPCs.  It actually proxies a local block
   store, e.g. a GDBM block store, and serves it remotely.  A lot of code is
   borrowed from `chop-archiver.c'.  */

#include <chop/chop.h>
#include <chop/hash.h>
#include <chop/stores.h>
#include <chop/filters.h>
#include <chop/block-server.h>

#include <stdio.h>
#include <stdlib.h>
#include <rpc/pmap_clnt.h>
#include <string.h>
#include <memory.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/poll.h>
#include <netdb.h>

#include <errno.h>
#include <assert.h>

#include <chop/chop-config.h>
#ifdef HAVE_GNUTLS
# include <gnutls/gnutls.h>
# include <chop/sunrpc-tls.h>
#endif

#if (defined HAVE_PTHREAD_H) && (defined HAVE_AVAHI)
# define USE_AVAHI 1
#endif

#ifndef SIG_PF
#define SIG_PF void(*)(int)
#endif

/* The program name.  */
static char *program_name = NULL;

/* The local block store being proxied.  */
static chop_block_store_t *local_store = NULL;

/* The local store file name.  */
static char *local_store_file_name = NULL;

/* The protocol underlying SunRPC: UDP or TCP.  */
static long protocol_type = IPPROTO_TCP;

#ifdef HAVE_GNUTLS
/* Whether to use RPC over TLS.  */
static int use_tls = 0;
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
# include "avahi-publish.c"
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

static int *
handle_say_hello (char **argp, struct svc_req *req)
{
  static int result = 1;

  return &result;
}

static int *
handle_block_exists (chop_rblock_key_t *argp, struct svc_req *req)
{
  static int result = 0;
  errcode_t err;
  int exists;
  chop_block_key_t key;

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
  errcode_t err;
  chop_block_key_t key;

  chop_block_key_init (&key, argp->key.chop_rblock_key_t_val,
		       argp->key.chop_rblock_key_t_len, NULL, NULL);
  VALIDATE_HASH_KEY (key, argp);

  if (!no_collision_check)
    {
      chop_buffer_t buffer;
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
	    result = 0;
	  break;

	default:
	  result = -2;
	}

      chop_buffer_return (&buffer);
      if (result)
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
  errcode_t err;
  size_t read;
  chop_block_key_t key;
  chop_buffer_t buffer;

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
  errcode_t err;

  err = chop_store_sync (local_store);
  result = err ? -1 : 0;

  return &result;
}

static int *
handle_close (void *unused, struct svc_req *req)
{
  static int result = 0;
  errcode_t err;

  /* Never actually close the local store.  */
  err = chop_store_sync (local_store);
  result = err ? -1 : 0;

  return &result;
}


/* Local store management.  */
static errcode_t
open_db_store (const chop_file_based_store_class_t *class,
	       const char *file, chop_block_store_t *store)
{
  errcode_t err;

  err = chop_file_based_store_open (class, file,
				    O_RDWR | O_CREAT, S_IRUSR | S_IWUSR,
				    store);
  if (err)
    com_err (program_name, err, "while opening `%s' data file \"%s\"",
	     chop_class_name ((chop_class_t *)class), file);

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
static gnutls_anon_server_credentials_t server_anoncred;
static gnutls_dh_params_t               server_dh_params;
#define DH_BITS 1024

static int
make_tls_session (gnutls_session *session, void *closure)
{
  /* Need to enable anonymous KX specifically. */
  static const int kx_prio[] = { GNUTLS_KX_ANON_DH, 0 };

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

  err = gnutls_init (session, GNUTLS_SERVER);
  if (err)
    return -1;

  gnutls_set_default_priority (*session);
  gnutls_kx_set_priority (*session, kx_prio);
  gnutls_mac_set_priority (*session, mac_prio);
  gnutls_cipher_set_priority (*session, cipher_prio);
  gnutls_compression_set_priority (*session, compression_prio);

  gnutls_credentials_set (*session, GNUTLS_CRD_ANON, server_anoncred);
  gnutls_dh_set_prime_bits (*session, DH_BITS);

  return 0;
}

static void
initialize_tls_parameters (void)
{
  /* Generate Diffie Hellman parameters - for use with DHE kx
     algorithms. These should be discarded and regenerated once a day, once a
     week or once a month. Depending on the security requirements.  */
  gnutls_dh_params_init (&server_dh_params);
  gnutls_dh_params_generate2 (server_dh_params, DH_BITS);

  gnutls_anon_allocate_server_credentials (&server_anoncred);
  gnutls_anon_set_server_dh_params (server_anoncred, server_dh_params);
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

const char *argp_program_version = "chop-block-server 0.1";
const char *argp_program_bug_address = "<ludovic.courtes@laas.fr>";

static const char doc[] =
"chop-block-server -- serves block store RPCs\
\v\
This program serves block store RPCs by proxying LOCAL-BLOCK-STORE, a local \
(file-based) block store.";

static char args_doc[] = "LOCAL-BLOCK-STORE";

/* Use the dummy store for debugging purposes.  */
static int debugging = 0;

/* Whether to be verbose */
static int verbose = 0;

/* Whether to use the zlib filters.  */
static int use_zlib_filters = 0;

#ifdef HAVE_GPERF
static char *file_based_store_class_name = "gdbm_block_store";
#endif

static struct argp_option options[] =
  {
    { "verbose", 'v', 0, 0,        "Produce verbose output" },
    { "debug",   'd', 0, 0,
      "Produce debugging output and use a dummy block store (i.e. a block "
      "store that does nothing but print messages)" },
    { "zip",     'z', 0, 0,
      "Pass data through a zlib filter to compress (resp. decompress) data "
      "when writing (resp. reading) to (resp. from) the archive" },
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
#endif

    { 0, 0, 0, 0, 0 }
  };

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
      use_zlib_filters = 1;
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
      service_name = avahi_strdup (arg);
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


/* The program.  */

int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_filter_t *input_filter = NULL, *output_filter = NULL;
  SVCXPRT *transp;

  program_name = argv[0];

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, 0, 0);

  err = chop_init ();
  if (err)
    {
      com_err (argv[0], err, "while initializing libchop");
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
    }

  if (use_zlib_filters)
    {
      /* Create a filtered store that uses zlib filters and proxies the
	 block store for data (not metadata).  */
      chop_block_store_t *raw_store = local_store;

      local_store =
	chop_class_alloca_instance (&chop_filtered_block_store_class);
      input_filter = chop_class_alloca_instance (&chop_zlib_zip_filter_class);
      output_filter = chop_class_alloca_instance (&chop_zlib_unzip_filter_class);

      err = chop_zlib_zip_filter_init (-1, 0, input_filter);
      if (!err)
	err = chop_zlib_unzip_filter_init (0, output_filter);

      if (err)
	{
	  com_err (program_name, err, "while initializing zlib filters");
	  exit (4);
	}

      if (verbose)
	{
	  /* Dump the zlib filters' logs to `stderr'.  */
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
	  com_err (program_name, err, "while initializing filtered store");
	  exit (5);
	}
    }

  transp = register_rpc_handlers ();

#ifdef USE_AVAHI
  if (!no_service_publication)
    {
      pthread_t avahi_thread;

      err = pthread_create (&avahi_thread, NULL, avahi_thread_entry_point,
			    NULL);
      if (err)
	{
	  com_err (program_name, err, "while starting Avahi thread");
	  exit (6);
	}
    }
#endif

  /* Go ahead.  */
  svc_run ();

  /* Never reached.  */
  /* XXX:  Move to a signal handler?  */
  err = chop_store_close ((chop_block_store_t *)local_store);
  if (err)
    {
      com_err (argv[0], err, "while closing output block store");
      exit (7);
    }

  return 0;
}
