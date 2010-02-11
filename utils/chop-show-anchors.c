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

#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>

#include <alloca.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <argp.h>


#include <assert.h>

const char *argp_program_version = "chop-show-anchors 0.1";
const char *argp_program_bug_address = "<ludovic.courtes@laas.fr>";

static char doc[] =
"chop-show-anchors -- show the anchors\
\v\
Show the block boundaries as found using Rabin fingerprints computed \
on the contents of FILE.";

static struct argp_option options[] =
  {
    { "debug",   'd', 0, 0,
      "Turn on debugging output" },

    { "window-size", 'w', "SIZE", 0,
      "Set the size of windows used when computing fingerprints "
      "to SIZE bytes" },
    { "magic-fpr-mask", 'f', "MASK", 0,
      "Use MASK as the fingerprint mask used to determine whether a "
      "fingerprint is magic, i.e. whether it should yield a block "
      "boundary" },
    { "quiet",   'q', 0, 0,
      "Don't display FILE's contents" },
    { "stats",   's', 0, 0,
      "Output stats about the block produced" },

    { 0, 0, 0, 0, 0 }
  };

static char args_doc[] = "FILE";


/* Are we debugging?  */
static int debug = 0;

/* Whether do display FILE's contents.  */
static int quiet = 0;

/* Whether to show statistics about the blocks produced.  */
static int show_stats = 0;

/* The file we are observing.  */
static char *file_name = NULL;

/* The size of the window used when computing fingerprints.  */
static size_t window_size = 48;

/* The magic fingerprint mask.  */
static unsigned long magic_fpr_mask = 0x1fff; /* the 13 LSBs */



/* Block statistics.  */

typedef struct
{
  size_t count;
  size_t bytes;
  float  average;
  size_t max;
  size_t min;
} block_stats_t;

static void
stats_init (block_stats_t *stats)
{
  stats->count = stats->bytes = 0;
  stats->average = 0;
  stats->max = 0;
  stats->min = (size_t)-1;
}

static void
stats_update (block_stats_t *stats, size_t size)
{
  size_t prev_count = stats->count;

  stats->count++;
  stats->bytes += size;
  stats->max = (size > stats->max) ? size : stats->max;
  stats->min = (size < stats->min) ? size : stats->min;

  stats->average *= prev_count;
  stats->average += size;
  stats->average /= stats->count;
}

static void
stats_display (block_stats_t *stats)
{
  fprintf (stderr, "window size:    %7u\n", window_size);
  fprintf (stderr, "magic fpr mask: 0x%08lx\n\n", magic_fpr_mask);

  fprintf (stderr, "block count:    %7u\n", stats->count);
  fprintf (stderr, "size in bytes:  %7u\n", stats->bytes);
  fprintf (stderr, "max size:       %7u\n", stats->max);
  fprintf (stderr, "min size:       %7u\n", stats->min);
  fprintf (stderr, "avg size:       % 7.2f\n", stats->average);
}



/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case 'd':
      debug = 1;
      break;

    case 'q':
      quiet = 1;
      break;

    case 's':
      show_stats = 1;
      break;

    case 'w':
      window_size = atoi (arg);
      break;

    case 'f':
      magic_fpr_mask = strtoul (arg, NULL, 0);
      break;

    case ARGP_KEY_ARG:
      if (state->arg_num >= 1)
	/* Too many arguments. */
	argp_usage (state);

      file_name = arg;
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
  size_t source_size, chopped_size = 0;
  chop_stream_t *stream;
  chop_chopper_t *chopper;
  chop_buffer_t buffer;
  chop_log_t *chopper_log;
  block_stats_t the_stats;

  chop_init ();

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, NULL, 0);

  stream = chop_class_alloca_instance (&chop_file_stream_class);
  chopper = chop_class_alloca_instance ((chop_class_t *)&chop_anchor_based_chopper_class);

  {
    /* Get the size of the file (for debugging purposes).  */
    int e;
    struct stat st;

    e = stat (file_name, &st);
    if (e)
      {
	com_err (argv[0], errno, "%s", file_name);
	return 1;
      }

    source_size = st.st_size;
  }

  err = chop_file_stream_open (file_name, stream);
  if (err)
    {
      com_err (argv[0], err, "%s", file_name);
      return 1;
    }

  err = chop_anchor_based_chopper_init (stream, window_size, magic_fpr_mask,
					chopper);
  if (err)
    {
      com_err (argv[0], err, "anchor-based-chopper");
      return 1;
    }

  if (debug)
    {
      /* Output debugging messages to `stderr'.  */
      chopper_log = chop_anchor_based_chopper_log (chopper);
      chop_log_attach (chopper_log, 2, 0);
    }

  stats_init (&the_stats);
  chop_buffer_init (&buffer, chop_chopper_typical_block_size (chopper));

  while (1)
    {
      size_t size;
      err = chop_chopper_read_block (chopper, &buffer, &size);
      if ((err) && (err != CHOP_STREAM_END))
	{
	  com_err (argv[0], err, "while reading block");
	  return 2;
	}

      if ((!quiet) && (size))
	{
	  write (1, chop_buffer_content (&buffer), size);
	  write (1, "\n---\n", 5);
	}

      chopped_size += size;

      if ((show_stats) && (!err))
	stats_update (&the_stats, size);

      if (err == CHOP_STREAM_END)
	break;
    }

  if (show_stats)
    stats_display (&the_stats);

  assert (chopped_size == source_size);

  return 0;
}
