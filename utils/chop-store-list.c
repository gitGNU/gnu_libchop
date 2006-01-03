#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <chop/chop.h>
#include <chop/chop-config.h>
#include <chop/buffers.h>
#include <chop/objects.h>
#include <chop/stores.h>

#include <argp.h>


const char *argp_program_version = "chop-store-list 0.0";
const char *argp_program_bug_address = "<ludovic.courtes@laas.fr>";

static char doc[] =
"chop-store-list -- list entries of a keyed block store\
\v\
This program allows to list entries contained in the file-based keyed \
block store available in FILE.\n";

static struct argp_option options[] =
  {
#ifdef HAVE_GPERF
    { "store",   'S', "CLASS", 0,
      "Use CLASS as the underlying file-based block store" },
#endif
    { 0, 0, 0, 0, 0 }
  };

static char args_doc[] = "FILE";

#ifdef HAVE_GPERF
static char *file_based_store_class_name = "gdbm_block_store";
#endif

/* File name for the store being examined.  */
static char *store_name = NULL;



/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
#ifdef HAVE_GPERF
    case 'S':
      file_based_store_class_name = arg;
      break;
#endif
    case ARGP_KEY_ARG:
      if (state->arg_num >= 1)
	/* Too many arguments. */
	argp_usage (state);

      store_name = arg;
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


int
main (int argc, char *argv[])
{
  errcode_t err;
  int arg_index;
  size_t count = 0;
  const chop_class_t *db_store_class;
  chop_block_store_t *store;
  chop_block_iterator_t *it;
  int got_one_block = 0;
  chop_buffer_t buffer;

  chop_init ();

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, &arg_index, 0);


#ifdef HAVE_GPERF
  /* Lookup the user-specified store class.  */
  db_store_class = chop_class_lookup (file_based_store_class_name);
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
  /* Default:  Use the GDBM store.  */
  db_store_class = (chop_class_t *)&chop_gdbm_block_store_class;
#endif

  store = (chop_block_store_t *)
    chop_class_alloca_instance ((chop_class_t *)db_store_class);

  err = chop_file_based_store_open ((chop_file_based_store_class_t *)
				    db_store_class,
				    store_name,
				    O_RDONLY, S_IRUSR | S_IWUSR,
				    store);
  if (err)
    {
      com_err (argv[0], err, "while opening `%s' data file \"%s\"",
	       chop_class_name (db_store_class), store_name);
      return 2;
    }

  it = chop_class_alloca_instance (chop_store_iterator_class (store));

  chop_buffer_init (&buffer, 4096);

  /* Traverse STORE's blocks.  */
  for (err = chop_store_first_block (store, it);
       err == 0;
       err = chop_block_iterator_next (it))
    {
      const chop_block_key_t *key;
      char hex_key[1024], hex_content[15];
      size_t block_len, key_len;

      if ((err) || (chop_block_iterator_is_nil (it)))
	break;

      got_one_block = 1;
      key = chop_block_iterator_key (it);

      /* Hexify the key.  */
      key_len = (chop_block_key_size (key) * 2 >= sizeof (hex_key))
	? (sizeof (hex_key) / 2) - 1
	: chop_block_key_size (key);

      chop_buffer_to_hex_string (chop_block_key_buffer (key),
				 key_len, hex_key);

      /* Read the corresponding block content.  */
      chop_buffer_clear (&buffer);
      err = chop_store_read_block (store, key, &buffer, &block_len);
      assert (!err);
      assert (block_len == chop_buffer_size (&buffer));

      block_len = (chop_buffer_size (&buffer) * 2 >= sizeof (hex_content))
	? (sizeof (hex_content) / 2) - 1
	: chop_buffer_size (&buffer);
      chop_buffer_to_hex_string (chop_buffer_content (&buffer),
				 block_len, hex_content);

      fprintf (stdout, "key #%u: 0x%s %u bytes (0x%s%s)\n", count++,
	       hex_key, chop_buffer_size (&buffer), hex_content,
	       (block_len < chop_buffer_size (&buffer) ? "..." : ""));

    }

  if ((err) && (err != CHOP_STORE_END))
    {
      com_err (argv[0], err, "while traversing `%s' store \"%s\"",
	       file_based_store_class_name, store_name);
      exit (3);
    }

  if (got_one_block)
    chop_object_destroy ((chop_object_t *)it);

  chop_object_destroy ((chop_object_t *)store);

  return 0;
}
