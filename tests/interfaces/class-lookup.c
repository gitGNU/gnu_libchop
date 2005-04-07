/* Try out the `chop_class_lookup ()' function with a number of known
   built-in class names.  */

#include <chop/chop.h>
#include <chop/serializable.h>

#include <stdio.h>

int
main (int argc, char *argv[])
{
  static const char *const class_names[] =
    {
      "stream", "file_stream",
      "block_store", "gdbm_block_store", "remote_block_store",
      "chopper", "fixed_size_chopper",
      "filter", "zlib_zip_filter",
      NULL
    };
  const char *const *name;
  unsigned passed = 0, failed = 0;

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

  if (failed)
    {
      fprintf (stderr, "FAIL: %u classes found, %u classes not found\n",
	       passed, failed);
      return failed;
    }

  fprintf (stderr, "PASS: %u classes found\n", passed);

  return 0;
}
