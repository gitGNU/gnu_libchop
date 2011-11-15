/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2011  Ludovic Courtès <ludo@gnu.org>

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

/* Test the compliance of various deserializers.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/block-indexers.h>

#include <testsuite.h>
#include <stdlib.h>
#include <string.h>

struct pair
{
  const char *const class_name;
  const char *const serial;
};

static const struct pair ascii_serials[] =
  {
    {
      "chk_index_handle",
      "zqoimseyv5r4cpj3ab64o6y2j6cbatkz5picnvidsyafwnvtrvbq====,grqqeqcihncuevm7xnmnkfp3ukkruk2e/4a",
    },

    {
      "hash_index_handle",
      "3q2hrigwtmsmvqi64cy2yw7szh66drvf/122",
    },

    {
      "hash_block_indexer",
      "SHA1"
    },

    {
      "chk_block_indexer",
      "TWOFISH,ECB,TIGER,RMD160"
    },

    { NULL, NULL }
  };

/* Check whether we can serialize/deserialize PAIR.  */
static void
check_serial_deserial (const struct pair *pair)
{
  static const char junk[] = "012343210";

  chop_error_t err;
  chop_object_t *object;
  const chop_class_t *klass;
  chop_buffer_t buffer;
  char with_trailing_junk[strlen (pair->serial) + sizeof junk + 1];
  size_t read;

  test_stage ("instance of `%s'", pair->class_name);

  err = chop_buffer_init (&buffer, 0);
  test_check_errcode (err, "initializing buffer");

  klass = chop_class_lookup (pair->class_name);
  object = chop_class_alloca_instance (klass);

  test_stage_intermediate ("with trailing NUL");
  err = chop_object_deserialize (object, klass,
				 CHOP_SERIAL_ASCII,
				 pair->serial, strlen (pair->serial),
				 &read);

  test_check_errcode (err, "deserializing");
  test_assert (read == strlen (pair->serial));

  err = chop_object_serialize (object, CHOP_SERIAL_ASCII, &buffer);
  test_check_errcode (err, "serializing");

  test_assert (!strncmp (chop_buffer_content (&buffer), pair->serial,
			 strlen (pair->serial)));

  chop_object_destroy (object);
  chop_buffer_clear (&buffer);

  /* Now, add some trailing junk to see if the SIZE parameter is honored.  */

  test_stage_intermediate ("with trailing junk");
  strcpy (with_trailing_junk, pair->serial);
  strcat (with_trailing_junk, junk);

  err = chop_object_deserialize (object, klass,
				 CHOP_SERIAL_ASCII,
				 with_trailing_junk, strlen (pair->serial),
				 &read);

  test_check_errcode (err, "deserializing with trailing junk");
  test_assert (read == strlen (pair->serial));

  err = chop_object_serialize (object, CHOP_SERIAL_ASCII, &buffer);
  test_check_errcode (err, "serializing (trailing junk)");

  test_assert (!strncmp (chop_buffer_content (&buffer), pair->serial,
			 strlen (pair->serial)));

  test_stage_result (1);
}


int
main (int argc, char *argv[])
{
  chop_error_t err;
  const struct pair *pair;

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  for (pair = ascii_serials; pair->class_name != NULL; pair++)
    check_serial_deserial (pair);

  return EXIT_SUCCESS;
}
