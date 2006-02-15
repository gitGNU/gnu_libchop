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
      "block_store", "gdbm_block_store", "sunrpc_remote_block_store",
      "chopper", "fixed_size_chopper",
      "filter", "zlib_zip_filter",
      NULL
    };
  const char *const *name;
  unsigned passed = 0, failed = 0;

  test_init (argv[0]);

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
