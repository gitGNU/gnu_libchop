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

/* Try out the `chop_class_lookup ()' function with a number of known
   built-in class names.  */

#include <chop/chop.h>
#include <chop/objects.h>

#include <testsuite.h>

#include <stdio.h>

int
main (int argc, char *argv[])
{
  static const char *const class_names[] =
    {
      "stream", "file_stream",
      "block_store", "gdbm_block_store", "sunrpc_block_store",
      "chopper", "fixed_size_chopper",
      "filter", "zlib_zip_filter",
      NULL
    };
  const char *const *name;
  unsigned passed = 0, failed = 0;
  errcode_t err;

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  test_stage ("%u class lookups by name",
	      (sizeof (class_names) / sizeof (*class_names)) - 1);

  for (name = class_names;
       *name != NULL;
       name++)
    {
      const chop_class_t *class;

      class = chop_class_lookup (*name);
      if (!class)
	{
	  fprintf (stderr, "failed to find class `%s'\n", *name);
	  failed++;
	}
      else
	{
	  if (strcmp (*name, chop_class_name (class)))
	    {
	      fprintf (stderr, "class name mismatch for `%s'\n", *name);
	      failed++;
	    }
	  else
	    passed++;
	}
    }

  test_stage_result (!failed);

  if (failed)
    {
      fprintf (stderr, "FAIL: %u classes found, %u classes not found\n",
	       passed, failed);
      return failed;
    }

  return 0;
}
