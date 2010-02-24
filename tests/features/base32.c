/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2010  Ludovic Courtès <ludo@gnu.org>

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

/* Base32 tests as found in RFC 4648.  */

#include <chop/chop.h>

#include <testsuite.h>

#include <stdlib.h>

int
main (int argc, char *argv[])
{
  chop_error_t err;
  char base32[1024];

  test_init (argv[0]);

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  test_stage ("encoding");

  chop_buffer_to_base32_string ("", 0, base32);
  test_assert (!strcmp (base32, ""));

  chop_buffer_to_base32_string ("f", 1, base32);
  test_assert (!strcmp (base32, "my======"));

  chop_buffer_to_base32_string ("fo", 2, base32);
  test_assert (!strcmp (base32, "mzxq===="));

  chop_buffer_to_base32_string ("foo", 3, base32);
  test_assert (!strcmp (base32, "mzxw6==="));

  chop_buffer_to_base32_string ("foob", 4, base32);
  test_assert (!strcmp (base32, "mzxw6yq="));

  chop_buffer_to_base32_string ("fooba", 5, base32);
  test_assert (!strcmp (base32, "mzxw6ytb"));

  chop_buffer_to_base32_string ("foobar", 6, base32);
  test_assert (!strcmp (base32, "mzxw6ytboi======"));

  test_stage_result (1);

  return EXIT_SUCCESS;
}
