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

#include <errno.h>
#include <assert.h>



#ifndef SIG_PF
#define SIG_PF void(*)(int)
#endif

/* The program name.  */
static char *program_name = NULL;

/* The local block store being proxied.  */
static chop_block_store_t *local_store = NULL;

/* The local store file name.  */
static char *local_store_file_name = NULL;

/* The protocol underlying SunRPC.  */
static char *protocol_name = "tcp";

/* The name of the content hash algorithm enforced (if any).  */
static chop_hash_method_t content_hash_enforced = CHOP_HASH_NONE;

/* Whether block/key collision checking should turned off.  */
static int no_collision_check = 0;



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
      fprintf (stderr, "%s: %s: key %s: "				\
	       "violating %s content-hashing, rejected\n",		\
	       program_name, __FUNCTION__,				\
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
	      fprintf (stderr, "%s: key %s: collising detected (and rejected)\n",
		       program_name, hex_key);
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

static SVCXPRT *
register_rpc_handlers (void)
{
  long proto;
  SVCXPRT *transp;

  /* Register the handlers themselves.  */
  chop_block_server_say_hello_handler = handle_say_hello;
  chop_block_server_block_exists_handler = handle_block_exists;
  chop_block_server_write_block_handler = handle_write_block;
  chop_block_server_read_block_handler = handle_read_block;
  chop_block_server_sync_handler = handle_sync;
  chop_block_server_close_handler = handle_close;

  if (!strcasecmp (protocol_name, "tcp"))
    proto = IPPROTO_TCP;
  else if (!strcasecmp (protocol_name, "udp"))
    proto = IPPROTO_UDP;
  else
    {
      fprintf (stderr, "%s: %s: unknown protocol\n",
	       program_name, protocol_name);
      exit (1);
    }

  pmap_unset (BLOCK_STORE_PROGRAM, BLOCK_STORE_VERSION);

  if (proto == IPPROTO_TCP)
    transp = svctcp_create (RPC_ANYSOCK, 0, 0);
  else
    transp = svcudp_create (RPC_ANYSOCK);
  if (transp == NULL)
    {
      fprintf (stderr, "%s: cannot create RPC service\n",
	       program_name);
      exit (1);
    }

  if (!svc_register (transp, BLOCK_STORE_PROGRAM, BLOCK_STORE_VERSION,
		     chop_block_server_process_request, proto))
    {
      fprintf (stderr, "%s: unable to register tcp\n",
	       program_name);
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

    /* Network.  */
    { "restrict",'R', "HOSTS", 0,
      "Restrict access to HOSTS, a comma-separated list of hostnames." },
    { "protocol",'P', "PROTO", 0,
      "Serve RPCs over PROTO, either \"tcp\" or \"udp\"" },

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
      protocol_name = arg;
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
	  fprintf (stderr, "%s: %s: unknown hash method name\n",
		   program_name, arg);
	  exit (1);
	}
      break;

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
	  fprintf (stderr, "%s: class `%s' not found\n",
		   argv[0], file_based_store_class_name);
	  exit (1);
	}
      if (chop_object_get_class ((chop_object_t *)db_store_class)
	  != &chop_file_based_store_class_class)
	{
	  fprintf (stderr,
		   "%s: class `%s' is not a file-based store class\n",
		   argv[0], file_based_store_class_name);
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
