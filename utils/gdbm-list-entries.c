#include <gdbm.h>
#include <stdlib.h>
#include <stdio.h>

#include <chop/chop.h>

int
main (int argc, char *argv[])
{
  size_t count = 0;
  GDBM_FILE db;
  datum key;

  if (argc < 2)
    exit (1);

  db = gdbm_open (argv[1], 0, GDBM_READER, 0, 0);
  if (!db)
    {
      fprintf (stderr, "%s: %s\n", argv[1], gdbm_strerror (gdbm_errno));
      return 1;
    }

  for (key = gdbm_firstkey (db);
       key.dptr != NULL;
       key = gdbm_nextkey (db, key))
    {
      char hex_key[1024], hex_content[15];
      datum content;
      size_t len = (key.dsize * 2 >= sizeof (hex_key))
	? (sizeof (hex_key) / 2) - 1 : key.dsize;
      chop_buffer_to_hex_string (key.dptr, len, hex_key);

      content = gdbm_fetch (db, key);
      len = (content.dsize * 2 >= sizeof (hex_content))
	? (sizeof (hex_content) / 2) - 1 : content.dsize;
      chop_buffer_to_hex_string (content.dptr, len, hex_content);

      fprintf (stdout, "key #%u: 0x%s %u bytes (0x%s%s)\n", count++,
	       hex_key, content.dsize, hex_content,
	       (len < content.dsize ? "..." : ""));
    }

  gdbm_close (db);

  return 0;
}
