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
#include <string.h>

struct sample
{
  const char *plaintext;
  const char *encoded;
};

/* Examples from RFC 4648.  */
static const struct sample samples[] =
  {
    { "",       "" },
    { "f",      "my======" },
    { "fo",     "mzxq====" },
    { "foo",    "mzxw6===" },
    { "foob",   "mzxw6yq=" },
    { "fooba",  "mzxw6ytb" },
    { "foobar", "mzxw6ytboi======" },
    { NULL, NULL }
  };

static void
encoding (void)
{
  char buffer[512];
  const struct sample *sample;

  test_stage ("encoding");

  for (sample = samples; sample->plaintext; sample++)
    {
      chop_buffer_to_base32_string (sample->plaintext,
				    strlen (sample->plaintext),
				    buffer);
      test_assert (!strcmp (sample->encoded, buffer));
    }

  test_stage_result (1);
}

static void
decoding (void)
{
  char buffer[512];
  const struct sample *sample;

  test_stage ("decoding");

  for (sample = samples; sample->plaintext; sample++)
    {
      const char *end;
      size_t output_len;

      output_len = chop_base32_string_to_buffer (sample->encoded,
						 strlen (sample->encoded),
						 buffer, &end);
      test_assert (end == sample->encoded + strlen (sample->encoded));
      test_assert (output_len == strlen (sample->plaintext));
      test_assert (!memcmp (buffer, sample->plaintext, output_len));
    }

  test_stage_result (1);
}

static void
decoding_non_padded_strings (void)
{
  char buffer[512];
  const struct sample *sample;

  test_stage ("decoding of non-padded strings");

  for (sample = samples; sample->plaintext; sample++)
    {
      char *input, *pad;
      const char *end;
      size_t output_len;

      input = alloca (strlen (sample->encoded) + 1);
      strcpy (input, sample->encoded);
      pad = strchr (input, '=');
      if (pad)
	{
	  /* Remove the padding and make sure it still works.  */
	  *pad = '|';
	  output_len = chop_base32_string_to_buffer (input, strlen (input),
						     buffer, &end);
	  test_assert (end == pad);
	  test_assert (output_len == strlen (sample->plaintext));
	  test_assert (!memcmp (buffer, sample->plaintext, output_len));
	}
    }

  test_stage_result (1);
}

static void
decoding_strings_with_trailing_garbage (void)
{
  char buffer[512];
  const struct sample *sample;

  test_stage ("decoding of strings with trailing garbage");

  for (sample = samples; sample->plaintext; sample++)
    {
      char *input;
      const char *end;
      size_t output_len;

      input = alloca (strlen (sample->encoded) + 7);
      strcpy (input, sample->encoded);
      strcat (input, ",hello");

      output_len = chop_base32_string_to_buffer (input, strlen (input),
						 buffer, &end);
      test_assert (end == input + strlen (sample->encoded));
      test_assert (output_len == strlen (sample->plaintext));
      test_assert (!memcmp (buffer, sample->plaintext, output_len));
    }

  test_stage_result (1);
}

static void
random_data (void)
{
  unsigned int i;
  char input[1024], b32[2048], output[1024];

  test_stage ("encoding/decoding of random data");

  for (i = 0; i < 10; i++)
    {
      const char *end;
      size_t input_len, output_len;

      input_len = sizeof (input) / 2
	+ (random () % (sizeof (input) / 2));

      test_randomize_input (input, input_len);

      chop_buffer_to_base32_string (input, input_len, b32);

      output_len = chop_base32_string_to_buffer (b32, strlen (b32),
						 output, &end);
      test_assert (end == b32 + strlen (b32));
      test_assert (output_len == input_len);
      test_assert (!memcmp (input, output, output_len));
    }

  test_stage_result (1);
}


int
main (int argc, char *argv[])
{
  chop_error_t err;

  test_init (argv[0]);
  test_init_random_seed ();

  err = chop_init ();
  test_check_errcode (err, "initializing libchop");

  encoding ();
  decoding ();
  decoding_non_padded_strings ();
  decoding_strings_with_trailing_garbage ();
  random_data ();

  return EXIT_SUCCESS;
}
