#include <chop/chop.h>
#include <chop/chop-config.h>

#ifdef HAVE_GPERF

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <chop/buffers.h>
#include <chop/serializable.h>
#include <chop/stores.h>

#include <argp.h>


const char *argp_program_version = "chop-store-convert 0.1";
const char *argp_program_bug_address = "<ludovic.courtes@laas.fr>";

static char doc[] =
"chop-store-convert -- convert a keyed block store to another format\
\v\
This program allows to convert the contents of a file-based keyed \
block store available in SOURCE (in format SOURCE-CLASS) to another \
file-based keyed block store in format DEST-CLASS to DEST.";

static struct argp_option options[] =
  {
    { "source-class",   'S', "SOURCE-CLASS", 0,
      "Use SOURCE-CLASS as the underlying file-based block store "
      "class for SOURCE" },
    { "dest-class",     'D', "DEST-CLASS",   0,
      "Use DEST-CLASS as the underlying file-based block store "
      "class for DEST" },
    { 0, 0, 0, 0, 0 }
  };

static char args_doc[] = "SOURCE DEST";

static char *program_name = NULL;


/* Block store classes.  */
static char *source_store_class_name = NULL;
static char *dest_store_class_name = NULL;

/* File names.  */
static char *source_file_name = NULL;
static char *dest_file_name = NULL;


/* Get the class named CLASS_NAME.  */
static const chop_file_based_store_class_t *
get_store_class (const char *class_name)
{
  const chop_class_t *db_store_class;

  /* Lookup the user-specified store class.  */
  db_store_class = chop_class_lookup (class_name);
  if (!db_store_class)
    {
      fprintf (stderr, "%s: class `%s' not found\n",
	       program_name, class_name);
      exit (1);
    }
  if (chop_object_get_class ((chop_object_t *)db_store_class)
      != &chop_file_based_store_class_class)
    {
      fprintf (stderr,
	       "%s: class `%s' is not a file-based store class\n",
	       program_name, class_name);
      exit (1);
    }

  return ((chop_file_based_store_class_t *)db_store_class);
}


/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case 'S':
      source_store_class_name = arg;
      break;

    case 'D':
      dest_store_class_name = arg;
      break;

    case ARGP_KEY_ARG:
      if (state->arg_num >= 2)
	/* Too many arguments. */
	argp_usage (state);

      if (!source_file_name)
	source_file_name = arg;
      else
	dest_file_name = arg;
      break;

    case ARGP_KEY_END:
      if (state->arg_num < 2)
	/* Not enough arguments. */
	argp_usage (state);

      if ((source_store_class_name == NULL)
	  || (dest_store_class_name == NULL))
	argp_failure (state, 1, 0,
		      "You must specify the block store class of both "
		      "the source and destination store.");

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
#define PROGRESS_DISPLAY_MODULO  2000

  errcode_t err;
  int arg_index;
  size_t count;
  const chop_file_based_store_class_t *source_class, *dest_class;
  chop_block_store_t *source, *dest;
  chop_block_key_t prev_key, key;
  chop_buffer_t buffer;

  chop_init ();
  program_name = argv[0];

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, &arg_index, 0);

  /* Make the standard output unbuffered so that one can admire the progress
     animation.  */
  setvbuf (stdout, NULL, _IONBF, 0);

  source_class = get_store_class (source_store_class_name);
  dest_class = get_store_class (dest_store_class_name);
  source = (chop_block_store_t *)
    chop_class_alloca_instance ((chop_class_t *)source_class);
  dest = (chop_block_store_t *)
    chop_class_alloca_instance ((chop_class_t *)dest_class);

  err = chop_file_based_store_open (source_class,
				    source_file_name,
				    O_RDONLY, S_IRUSR | S_IWUSR,
				    source);
  if (err)
    {
      com_err (argv[0], err, "while opening `%s' data file \"%s\"",
	       chop_class_name ((chop_class_t *)source_class),
	       source_file_name);
      return 2;
    }

  err = chop_file_based_store_open (dest_class,
				    dest_file_name,
				    O_RDWR | O_CREAT,
				    S_IRUSR | S_IWUSR,
				    dest);
  if (err)
    {
      com_err (argv[0], err, "while opening `%s' data file \"%s\"",
	       chop_class_name ((chop_class_t *)dest_class),
	       dest_file_name);
      return 2;
    }

  chop_block_key_init (&key, NULL, 0, NULL, NULL);
  chop_block_key_init (&prev_key, NULL, 0, NULL, NULL);
  chop_buffer_init (&buffer, 4096);

  printf ("converting...\n");

  /* Traverse SOURCE's blocks.  */
  for (err = chop_store_first_key (source, &key), count = 0;
       err == 0;
       err = chop_store_next_key (source, &prev_key, &key), count++)
    {
      size_t block_len;

      if (err)
	break;

      /* Read the corresponding block content.  */
      chop_buffer_clear (&buffer);
      err = chop_store_read_block (source, &key, &buffer, &block_len);
      if (err)
	{
	  com_err (argv[0], err, "unexpected, while reading from `%s'",
		   source_file_name);
	  goto finish;
	}
      assert (block_len == chop_buffer_size (&buffer));

      /* Write it to DEST.  */
      err = chop_store_write_block (dest, &key,
				    chop_buffer_content (&buffer),
				    chop_buffer_size (&buffer));
      if (err)
	{
	  com_err (argv[0], err, "while writing to `%s'",
		   dest_file_name);
	  goto finish;
	}

      if (!(count++ % PROGRESS_DISPLAY_MODULO))
	{
	  /* The fancy progress animation!  */
	  static const char widgets[] = { '/', '-', '|', '-', '\\', '|' };
	  static unsigned pos = 0;

	  printf ("\b%c", widgets[pos]);
	  pos = (pos + 1) % sizeof (widgets);
	}

      if (chop_block_key_size (&prev_key))
	chop_block_key_free (&prev_key);

      prev_key = key;
    }

  if (chop_block_key_size (&key))
    chop_block_key_free (&key);

  if ((err) && (err != CHOP_STORE_END))
    com_err (argv[0], err, "while traversing `%s' store \"%s\"",
	     source_store_class_name, source_file_name);

  if (err == CHOP_STORE_END)
    err = 0;

 finish:
  chop_store_close (source);
  chop_store_close (dest);

  return (err ? 1 : 0);
}


#else /* HAVE_GPERF */

#include <stdlib.h>
#include <stdio.h>

int
main (int argc, char *argv[])
{
  fprintf (stderr, "chop-store-convert:  Sorry, you need `gperf' at "
	   "compile-time to\n");
  fprintf (stderr, "                     compile this program.\n\n");

  return 1;
}

#endif /* HAVE_GPERF */
