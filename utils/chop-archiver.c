#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>
#include <chop/stores.h>
#include <chop/filters.h>

#include <chop/indexers.h>

#include <chop/chop-config.h>

#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <getopt.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <argp.h>

const char *argp_program_version = "chop-archiver 0.1";
const char *argp_program_bug_address = "<ludovic.courtes@laas.fr>";

static char doc[] =
"chop-archiver -- archives and restores different versions of a file\
\v\
This program can archive a given file's revision into a GDBM block store \
and eventually restore it from them.  Subsequent revisions of a file will \
hopefully require less room than the sum of each revision's size.  When \
asked to archive a file (with `--archive') the program displays an \
\"archive handle\" in the form of a hash.  This handle must be kept and \
eventually passed to `--restore'.  Note that `chop-archiver' itself is \
somewhat dumb since it does not keep track of what handle correspond to \
what file or revision so you have to do this by yourself.\n\
\n\
Also, if you passed `--zip' at archival time, you will have to pass it \
at restoration time as well so that the archived stream gets decompressed \
on the fly.  Failing to do so, you will get the raw, zlib-compressed, file \
and won't be able to do anything with it (`gunzip' won't work).  The same \
goes for `--cipher'.";


const char *program_name = NULL;

#define DB_DATA_FILE_BASE       ".chop-archiver/archive-data"
#define DB_META_DATA_FILE_BASE  ".chop-archiver/archive-meta-data"


/* Whether archival or retrieval is to be performed.  */
static int archive_queried = 0, restore_queried = 0;

/* If ARCHIVE_QUERIED, this is the typical size of blocks that should be
   produced by the chopper.  Zero means ``chopper class preferred
   value''.  */
static size_t typical_block_size = 0;

/* The option passed to either `--archive' or `--restore'.  */
static char *option_argument = NULL;

/* Use the dummy store for debugging purposes.  */
static int debugging = 0;

/* Whether to be verbose */
static int verbose = 0;

/* Whether to use the zlib filters.  */
static int use_zlib_filters = 0;

/* The remote block store host name or NULL.  */
static char *remote_hostname = NULL;

/* The ciphering algorithm.  */
static char *cipher_algo = NULL;


#ifdef HAVE_GPERF
static char *file_based_store_class_name = "gdbm_block_store";
static char *chopper_class_name = "fixed_size_chopper";
#endif

static struct argp_option options[] =
  {
    { "verbose", 'v', 0, 0,        "Produce verbose output" },
    { "debug",   'd', 0, 0,
      "Produce debugging output and use a dummy block store (i.e. a block "
      "store that does nothing but print messages)" },
    { "block-size", 'b', "SIZE", 0,
      "Choose a typical size of SIZE bytes for the blocks produced by "
      "the chopper" },
    { "zip",     'z', 0, 0,
      "Pass data blocks through a zlib filter to compress (resp. decompress) "
      "data when writing (resp. reading) to (resp. from) the archive" },
    { "cipher",  'c', "ALGO", 0,
      "Content-hash encrypt blocks using ALGO as the ciphering algorithm" },
    { "remote",  'R', "HOST", 0,
      "Use the remote block store located at HOST (using TCP) for both "
      "data and meta-data blocks" },
#ifdef HAVE_GPERF
    { "store",   'S', "CLASS", 0,
      "Use CLASS as the underlying file-based block store" },
    { "chopper", 'C', "CHOPPER", 0,
      "Use CHOPPER as the input stream chopper" },
#endif

    /* The main functions.  */
    { "archive", 'a', "FILE",   0,
      "Archive FILE and return an archived revision handle" },
    { "restore", 'r', "HANDLE", 0,
      "Restore a file's revision from HANDLE, an archived revision handle" },

    { 0, 0, 0, 0, 0 }
  };



/* Archive STREAM onto DATA_STORE and METADATA_STORE.  Use CHOPPER and
   INDEXER in order to chop STREAM into blocks and then index blocks.  */
errcode_t
do_archive (chop_stream_t *stream, chop_block_store_t *data_store,
	    chop_block_store_t *metadata_store,
	    chop_chopper_t *chopper, chop_indexer_t *indexer)
{
  errcode_t err;
  chop_buffer_t buffer;
  chop_index_handle_t *handle;

  handle = chop_indexer_alloca_index_handle (indexer);
  err = chop_indexer_index_blocks (indexer, chopper,
				   data_store, metadata_store, handle);
  if ((err) && (err != CHOP_STREAM_END))
    {
      com_err (program_name, err, "while indexing blocks");
      return err;
    }

  if (verbose)
    {
      /* Take a look at the index handle we got */
      fprintf (stdout, "chop: done with indexing\n");
      fprintf (stdout, "chop: got a handle of class \"%s\"\n",
	       chop_class_name (chop_object_get_class ((chop_object_t *)handle)));
    }

  err = chop_buffer_init (&buffer, 400);
  if (err)
    exit (12);

  err = chop_object_serialize ((chop_object_t *)handle, CHOP_SERIAL_ASCII,
			       &buffer);
  if (err)
    {
      com_err (program_name, err, "while serializing index handle");
      exit (8);
    }

  /* Display the ASCII representation of HANDLE.  We assume that it is
     zero-terminated.  */
  if (verbose)
    fprintf (stdout, "chop: handle: %s\n", chop_buffer_content (&buffer));
  else
    /* Print the handle on a single line so that external tools can use
       it easily.  */
    fprintf (stdout, "%s\n", chop_buffer_content (&buffer));

  chop_buffer_return (&buffer);
  chop_object_destroy ((chop_object_t *)handle);

  if (verbose)
    fprintf (stdout, "chop: archive done\n");

  return 0;
}

/* Retrieve data pointed to by HANDLE from DATA_STORE and METADATA_STORE
   using INDEXER and display it.  */
errcode_t
do_retrieve (chop_index_handle_t *handle, chop_block_store_t *data_store,
	     chop_block_store_t *metadata_store, chop_indexer_t *indexer)
{
  errcode_t err;
  chop_stream_t *stream;
  char buffer[3577];  /* Dummy size chosen on purpose */

  stream = chop_indexer_alloca_stream (indexer);
  err = chop_indexer_fetch_stream (indexer, handle,
				   data_store, metadata_store,
				   stream);
  if (err)
    {
      com_err (program_name, err, "while retrieving stream");
      return err;
    }

  while (1)
    {
      size_t read = 0;
      err = chop_stream_read (stream, buffer, sizeof (buffer), &read);
      if (err)
	break;

      write (1, buffer, read);
    }

  if (err != CHOP_STREAM_END)
    {
      com_err (program_name, err, "while reading stream");
      return err;
    }

  return 0;
}

static errcode_t
process_command (const char *argument,
		 chop_block_store_t *data_store,
		 chop_block_store_t *metadata_store,
		 chop_indexer_t *indexer,
		 chop_log_t *indexer_log)
{
  errcode_t err;
  chop_block_store_t *data_proxy, *metadata_proxy;

  if (verbose)
    {
      data_proxy = (chop_block_store_t *)
	chop_class_alloca_instance (&chop_dummy_block_store_class);
      metadata_proxy = (chop_block_store_t *)
	chop_class_alloca_instance (&chop_dummy_block_store_class);

      chop_dummy_proxy_block_store_open ("data", data_store, data_proxy);
      chop_dummy_proxy_block_store_open ("meta-data", metadata_store,
					 metadata_proxy);

      /* Attach the indexer log to stderr.  */
      chop_log_attach (indexer_log, 2, 0);
    }
  else
    data_proxy = metadata_proxy = NULL;

#define THE_STORE(_what) (_what ## _proxy ? _what ## _proxy : _what ## _store)

  if (archive_queried)
    {
      chop_stream_t *stream;
      chop_chopper_class_t *chopper_class;
      chop_chopper_t *chopper;

      stream = chop_class_alloca_instance (&chop_file_stream_class);
      err = chop_file_stream_open (argument, stream);
      if (err)
	{
	  com_err (program_name, err, "while opening %s", argument);
	  exit (1);
	}

#ifdef HAVE_GPERF
      chopper_class =
	(chop_chopper_class_t *)chop_class_lookup (chopper_class_name);
      if (!chopper_class)
	{
	  fprintf (stderr, "%s: class `%s' not found\n",
		   program_name, chopper_class_name);
	  exit (1);
	}
      if (!chop_object_is_a ((chop_object_t *)chopper_class,
			     (chop_class_t *)&chop_chopper_class_class))
	{
	  fprintf (stderr, "%s: class `%s' is not a chopper class\n",
		   program_name, chopper_class_name);
	  exit (1);
	}
#else
      chopper_class = &chop_fixed_size_chopper_class;
#endif

      chopper = chop_class_alloca_instance ((chop_class_t *)chopper_class);
      err = chop_chopper_generic_open (chopper_class, stream,
				       typical_block_size, chopper);
      if (err)
	{
	  com_err (program_name, err,
		   "while initializing chopper of class `%s'",
		   chopper_class_name);
	  exit (2);
	}

      err = do_archive (stream,
			THE_STORE (data), THE_STORE (metadata),
			chopper, indexer);

      chop_stream_close ((chop_stream_t *)stream);
    }
  else if (restore_queried)
    {
      chop_index_handle_t *handle;
      const chop_class_t *handle_class;

      handle = chop_indexer_alloca_index_handle (indexer);
      handle_class = chop_indexer_index_handle_class (indexer);
      err = chop_object_deserialize ((chop_object_t *)handle,
				     handle_class, CHOP_SERIAL_ASCII,
				     argument, strlen (argument) + 1);
      if (err)
	{
	  com_err (program_name, err, "while deserializing index handle");
	  return err;
	}

      err = do_retrieve (handle,
			 THE_STORE (data), THE_STORE (metadata), indexer);
    }
#undef THE_STORE
  else
    {
      fprintf (stderr,
	       "%s: You must pass either `--archive' or `--restore'\n",
	       program_name);
      exit (1);
    }

  if (verbose)
    {
      chop_store_close (data_proxy);
      chop_store_close (metadata_proxy);
    }

  return err;
}

static errcode_t
open_db_store (const chop_file_based_store_class_t *class,
	       const char *base, chop_block_store_t *store)
{
  errcode_t err;
  char *file;
  const char *suffix, *suffix_end;
  size_t file_len, home_len, base_len, suffix_len;

  base_len = strlen (base);
  suffix = chop_class_name ((chop_class_t *)class);
  suffix_end = strchr (suffix, '_');
  suffix_len = suffix_end - suffix;
  home_len = strlen (getenv ("HOME"));
  file_len = home_len + 1 + base_len + 1 + suffix_len;
  file = alloca (file_len + 1);
  if (!file)
    return ENOMEM;

  strcpy (file, getenv ("HOME"));
  file[home_len] = '/';
  strcpy (&file[home_len + 1], base);
  strcpy (&file[home_len + 1 + base_len], ".");
  strncpy (&file[home_len + 1 + base_len + 1], suffix, suffix_len);
  file[file_len] = '\0';

  err = chop_file_based_store_open (class, file,
				    O_RDWR | O_CREAT, S_IRUSR | S_IWUSR,
				    store);
  if (err)
    com_err (program_name, err, "while opening `%s' data file \"%s\"",
	     chop_class_name ((chop_class_t *)class), file);

  return err;
}

static chop_cipher_handle_t
get_cipher_handle (const char *algoname)
{
  errcode_t err;
  chop_cipher_algo_t algo;
  chop_cipher_handle_t handle = CHOP_CIPHER_HANDLE_NIL;

  err = chop_cipher_algo_lookup (algoname, &algo);
  if (err)
    com_err (program_name, err, "%s: unknown ciphering algorithm",
	     algoname);
  else
    {
      /* The MODE argument below was chosen almost arbitrarily.  */
      handle = chop_cipher_open (algo, CHOP_CIPHER_MODE_ECB);
      if (handle == CHOP_CIPHER_HANDLE_NIL)
	com_err (program_name, CHOP_INVALID_ARG,
		 "while opening cipher algorithm `%s'", algoname);
    }

  return handle;
}


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
    case 'b':
      typical_block_size = strtoul (arg, NULL, 0);
      break;
    case 'a':
      archive_queried = 1;
      option_argument = arg;
      break;
    case 'r':
      restore_queried = 1;
      option_argument = arg;
      break;
    case 'z':
      use_zlib_filters = 1;
      break;
    case 'c':
      cipher_algo = arg;
      break;
    case 'R':
      remote_hostname = arg;
      break;
#ifdef HAVE_GPERF
    case 'S':
      file_based_store_class_name = arg;
      break;
    case 'C':
      chopper_class_name = arg;
      break;
#endif
    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

/* Argp argument parsing.  */
static struct argp argp = { options, parse_opt, 0, doc };


int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_block_store_t *store, *metastore;
  chop_indexer_t *indexer;
  chop_filter_t *input_filter = NULL, *output_filter = NULL;
  chop_cipher_handle_t cipher_handle = CHOP_CIPHER_HANDLE_NIL;

  program_name = argv[0];

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, 0, 0);

  err = chop_init ();
  if (err)
    {
      com_err (argv[0], err, "while initializing libchop");
      return 1;
    }

  if (cipher_algo)
    {
      cipher_handle = get_cipher_handle (cipher_algo);
      if (cipher_handle == CHOP_CIPHER_HANDLE_NIL)
	return 1;
    }

  indexer = chop_class_alloca_instance (&chop_hash_tree_indexer_class);
  err = chop_hash_tree_indexer_open (CHOP_HASH_SHA1, CHOP_HASH_SHA1,
				     cipher_handle,
				     12,
				     indexer);
  if (err)
    {
      com_err (program_name, err, "failed to open tree-hash indexer");
      exit (2);
    }

  if (!debugging)
    {
      if (remote_hostname)
	{
	  /* Use a remote block store for both data and metadata blocks.  */
	  store = (chop_block_store_t *)
	    chop_class_alloca_instance (&chop_remote_block_store_class);

	  err = chop_remote_block_store_open (remote_hostname, "tcp",
					      store);
	  if (err)
	    {
	      com_err (program_name, err,
		       "failed to open remote block store");
	      exit (3);
	    }

	  metastore = store;
	}
      else
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

	  store = (chop_block_store_t *)
	    chop_class_alloca_instance ((chop_class_t *)db_store_class);
	  metastore = (chop_block_store_t *)
	    chop_class_alloca_instance ((chop_class_t *)db_store_class);

	  err = open_db_store (db_store_class, DB_DATA_FILE_BASE, store);
	  if (err)
	    exit (3);

	  err = open_db_store (db_store_class, DB_META_DATA_FILE_BASE,
			       metastore);
	  if (err)
	    exit (3);
	}
    }
  else
    {
      /* Use a dummy block store for debugging purposes */
      store = (chop_block_store_t *)
	chop_class_alloca_instance (&chop_dummy_block_store_class);
      metastore = (chop_block_store_t *)
	chop_class_alloca_instance (&chop_dummy_block_store_class);

      chop_dummy_block_store_open ("data", store);
      chop_dummy_block_store_open ("meta-data", metastore);
    }

  if (use_zlib_filters)
    {
      /* Create a filtered store that uses zlib filters and proxies the
	 block store for data (not metadata).  */
      chop_block_store_t *raw_store = store;

      store = chop_class_alloca_instance (&chop_filtered_block_store_class);
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
				      raw_store, store);
      if (err)
	{
	  com_err (program_name, err, "while initializing filtered store");
	  exit (5);
	}
    }

  /* */
  err = process_command (option_argument, store, metastore,
			 indexer, chop_hash_tree_indexer_log (indexer));

  err = chop_store_close ((chop_block_store_t *)store);
  if (err)
    {
      com_err (argv[0], err, "while closing output block store");
      exit (7);
    }

  err = chop_store_close ((chop_block_store_t *)metastore);
  if (err)
    {
      com_err (argv[0], err, "while closing output meta-data block store");
      exit (7);
    }

  return 0;
}

