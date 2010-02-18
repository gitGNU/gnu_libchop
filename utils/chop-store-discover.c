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

/* Discover block stores on the network.  */

#include <chop/chop-config.h>

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/store-browsers.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <argp.h>

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif


const char *argp_program_version = "chop-store-discover 0.0";
const char *argp_program_bug_address = "<ludovic.courtes@laas.fr>";

static char doc[] =
"chop-store-discover -- discover keyed block stores over the network\
\v\
This program looks for keyed block stores available over the network, \
for instance using the service discovery facilities provided by Avahi.  \
The output contains 4 columns: the service name, its IP address and port \
number (separated by `:'), its block naming scheme \
specification in terms of a hash function, as well as the name of \
the class that implements it on the client side.";

static struct argp_option options[] =
  {
    { "debug",   'd', 0, 0,
      "Produce debugging output" },
    { "timeout", 't', "TIMEOUT", 0,
      "Wait for at most TIMEOUT msecs" },
    { "discoveries", 'D', "N", 0,
      "Quit after at least N discoveries" },
    { "domain", 'o', "DOMAIN", 0,
      "Look for block stores within domain DOMAIN, a FQDN (e.g., `.local' "
      "or `.laas.fr.')" },
    { "hash-method", 'H', "HASH", 0,
      "Only look for stores that support block naming according to hash "
      "method HASH" },
    { 0, 0, 0, 0, 0 }
  };

static char args_doc[] = "FILE";


/* Configuration.  */

static char *program_name = NULL;

/* Whether to output debugging info.  */
static int debug = 0;

/* Whether to use some timeout and if so how much.  */
static int use_timeout = 0;
static unsigned timeout = 0;

/* Minimal number of discoveries.  */
static unsigned min_discoveries = 0;

/* Specific hash method we're interested in.  */
static chop_hash_method_t hash_method = CHOP_HASH_NONE;

/* Domain where to search for block stores.  */
static char *domain_name = NULL;


/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  chop_error_t err = 0;

  switch (key)
    {
    case 'd':
      debug = 1;
      break;

    case 't':
      use_timeout = 1;
      timeout = atoi (arg);
      break;

    case 'D':
      min_discoveries = atoi (arg);
      break;

    case 'H':
      if (chop_hash_method_lookup (arg, &hash_method))
	{
	  chop_error (err, "%s: unknown hash method", arg);
	  exit (1);
	}
      break;

    case 'o':
      domain_name = strdup (arg);
      break;

    case ARGP_KEY_ARG:
      if (state->arg_num > 0)
	/* Too many arguments. */
	argp_usage (state);

      break;

    case ARGP_KEY_END:
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

/* Argp argument parsing.  */
static struct argp argp = { options, parse_opt, args_doc, doc };


/* Store discovery/removal handlers.  */

static int
handle_discovery (chop_store_browser_t *browser,
		  const char *service_name,
		  const char *host,
		  unsigned port,
		  chop_hash_method_spec_t spec,
		  const chop_class_t *client,
		  void *userdata)
{
  if ((hash_method == CHOP_HASH_NONE)
      || (chop_hash_method_compatible (spec, hash_method)))
    {
      static unsigned discoveries = 0;

      printf ("%s\t%s:%u\t%s\t%s\n",
	      service_name, host, port,
	      chop_hash_method_spec_to_string (spec),
	      client ? chop_class_name (client) : "unknown");

      if ((min_discoveries > 0)
	  && (++discoveries >= min_discoveries))
	/* Exit the main event loop.  */
	return 1;
    }

  return 0;
}

static int
handle_removal (chop_store_browser_t *browser,
		const char *service_name,
		void *userdata)
{
  fprintf (stderr, "service `%s' disappeared\n", service_name);
  return 0;
}



int
main (int argc, char *argv[])
{
  chop_error_t err;
  chop_store_browser_t *browser;

  program_name = argv[0];

  chop_init ();

  /* Parse arguments.  */
  argp_parse (&argp, argc, argv, 0, 0, 0);

  browser = chop_class_alloca_instance (&chop_avahi_store_browser_class);
  err = chop_avahi_store_browser_open (domain_name,
				       handle_discovery, NULL,
				       handle_removal, NULL,
				       browser);
  if (err)
    {
      chop_error (err, "while initializing Avahi store browser");
      exit (1);
    }

  if (debug)
    {
      chop_log_t *log;

      log = chop_avahi_store_browser_log (browser);
      assert (log);
      chop_log_attach (log, 2, 0);
    }

  if (use_timeout)
    {
      /* Iterate until we run out of time.  */
      unsigned remaining = timeout;

      /* FIXME: Move this loop to `libchop-store-browsers'.  */
      while (remaining)
	{
	  unsigned long elapsed;
	  struct timeval before, after;

	  if (gettimeofday (&before, NULL))
	    {
	      chop_error (errno, "gettimeofday");
	      exit (2);
	    }
	  err = chop_store_browser_iterate (browser, timeout);
	  if (gettimeofday (&after, NULL))
	    {
	      chop_error (errno, "gettimeofday");
	      exit (2);
	    }

	  elapsed = (after.tv_sec - before.tv_sec) * 1000;
	  if (elapsed)
	    elapsed += (1e3 - (before.tv_usec / 1000))
	      + (after.tv_usec / 1000);
	  else
	    elapsed += (after.tv_usec - before.tv_usec) / 1000;

	  remaining = (remaining > elapsed) ? (remaining - elapsed) : 0;
	}
    }
  else
    err = chop_store_browser_loop (browser);

  if (err)
    {
      chop_error (err, "while browsing stores");
      exit (2);
    }

  chop_object_destroy ((chop_object_t *)browser);

  return err;
}

/* arch-tag: 7af3b074-87f0-417b-9a84-dac18462a030
 */
