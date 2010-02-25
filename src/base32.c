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

/* Base32 encoding, as described in RFC 4648.  */

#include <chop/chop-config.h>

#include <chop/chop.h>

#include <stdint.h>
#include <ctype.h>


/* Return a mask for bits from FIRST to LAST included.  */
#define BIT_MASK(first, last)				\
  (((1 << ((last) - (first) + 1)) - 1) << (first))


/* Encoding.  */

/* The base32 alphabet.  */
static const char base32_chars[] = "abcdefghijklmnopqrstuvwxyz234567";

void
chop_buffer_to_base32_string (const char *buffer, size_t size, char *b32)
{
  const uint8_t *ubuf;
  unsigned int padding, quintets, i, j;
  uint8_t value = 0;

  if (CHOP_EXPECT_FALSE (size == 0))
    {
      *b32 = '\0';
      return;
    }

  ubuf = (const uint8_t *) buffer;

  j = quintets = (size * 8 + 4) / 5;
  i = size - 1;

  /* Below is a "Duff's device", which is essentially the unrolled loop that
     reads quintets (5-bit groups) from BUFFER and assign the corresponding
     character to B32.  BUFFER is read from right to left and B32 is filled
     from right to left.  */
  switch ((size * 8) % 5)
    {
      /* The case values below are sorted in the reverse order of modulos:
	 (unfold (lambda (k) (>= k 5)) (lambda (i) (modulo (* 8 i) 5)) 1+ 0)
	 => (0 3 1 4 2)   */

    case 0:
      do
	{
	  value = ubuf[i] & BIT_MASK (0, 4);
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i--] & BIT_MASK (5, 7)) >> 5U;

	case 2:
	  value |= (ubuf[i] & BIT_MASK (0, 1)) << 3U;
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i] & BIT_MASK (2, 6)) >> 2U;
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i--] & BIT_MASK (7, 7)) >> 7U;

	case 4:
	  value |= (ubuf[i] & BIT_MASK (0, 3)) << 1U;
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i--] & BIT_MASK (4, 7)) >> 4U;

	case 1:
	  value |= (ubuf[i] & BIT_MASK (0, 0)) << 4U;
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i] & BIT_MASK (1, 5)) >> 1U;
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i--] & BIT_MASK (6, 7)) >> 6U;

	case 3:
	  value |= (ubuf[i] & BIT_MASK (0, 2)) << 2U;
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i--] & BIT_MASK (3, 7)) >> 3U;
	  b32[--j] = base32_chars[value];
	}
      while (j >= 8);
    }

  padding = quintets % 8 ? 8 - (quintets % 8) : 0;

  for (j = 0; j < padding; j++)
    b32[quintets + j] = '=';

  b32[quintets + padding] = '\0';
}


/* Decoding.  */

static inline int
is_base32 (char c)
{
  c = toupper (c);
  return ((c >= 'A' && c <= 'Z')
	  || (c >= '2' && c <= '7'));
}

#define B32(c, v)  [ (int) c ] = (v)

static const uint8_t base32_values[] =
  {
    B32 ('A', 0), B32 ('B', 1), B32 ('C', 2), B32 ('D', 3),
    B32 ('E', 4), B32 ('F', 5), B32 ('G', 6), B32 ('H', 7),
    B32 ('I', 8), B32 ('J', 9), B32 ('K', 10), B32 ('L', 11),
    B32 ('M', 12), B32 ('N', 13), B32 ('O', 14), B32 ('P', 15),
    B32 ('Q', 16), B32 ('R', 17), B32 ('S', 18), B32 ('T', 19),
    B32 ('U', 20), B32 ('V', 21), B32 ('W', 22), B32 ('X', 23),
    B32 ('Y', 24), B32 ('Z', 25),
    B32 ('2', 26), B32 ('3', 27), B32 ('4', 28), B32 ('5', 29),
    B32 ('6', 30), B32 ('7', 31)
  };

#undef B32

static inline uint8_t
base32_value (char c)
{
  return base32_values[toupper (c)];
}

size_t
chop_base32_string_to_buffer (const char *b32, size_t size, char *buffer,
			      const char **end)
{
#define CONSUME_QUINTET(array, index)		\
  /* Input sanitizing.  */			\
  if ((array)[(index)] == '=')			\
    {						\
      i = (index);				\
      goto skip_padding;			\
    }						\
  else if (!is_base32 ((array)[(index)]))	\
    {						\
      i = (index);				\
      break;					\
    }						\
  value = base32_value ((array)[(index)])

  uint8_t value;
  size_t left, i, j;
  uint8_t *out = (uint8_t *) buffer;

  j = 0;
  for (i = 0, left = size;
       i < size && left >= 8;
       i += 8, left -= 8)
    {
      CONSUME_QUINTET (b32, i);
      out[j] = value << 3U;

      CONSUME_QUINTET (b32, i + 1);
      out[j++] |= (value & BIT_MASK (2, 4)) >> 2U;
      out[j] = (value & BIT_MASK (0, 1)) << 6U;

      CONSUME_QUINTET (b32, i + 2);
      out[j] |= value << 1U;

      CONSUME_QUINTET (b32, i + 3);
      out[j++] |= (value & BIT_MASK (4, 4)) >> 4U;
      out[j] = (value & BIT_MASK (0, 3)) << 4U;

      CONSUME_QUINTET (b32, i + 4);
      out[j++] |= (value & BIT_MASK (1, 4)) >> 1U;
      out[j] = (value & BIT_MASK (0, 0)) << 7U;

      CONSUME_QUINTET (b32, i + 5);
      out[j] |= value << 2U;

      CONSUME_QUINTET (b32, i + 6);
      out[j++] |= (value & BIT_MASK (3, 4)) >> 3U;
      out[j] = (value & BIT_MASK (0, 2)) << 5U;

      CONSUME_QUINTET (b32, i + 7);
      out[j++] |= value;
    }

  for (;
       (i % 8) && left > 0;
       i++, left--)
    {
    skip_padding:
      if (b32[i] != '=')
	/* We expected padding but got something else.  */
	break;
    }

  *end = &b32[i];

  return j;

#undef CONSUME_QUINTET
}
