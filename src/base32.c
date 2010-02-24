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

/* Return a mask for bits from FIRST to LAST included.  */
#define BIT_MASK(first, last)				\
  (((1 << ((last) - (first) + 1)) - 1) << (first))

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
	  value |= (ubuf[i] & BIT_MASK (0, 3)) << 2U;
	  b32[--j] = base32_chars[value];

	  value = (ubuf[i--] & BIT_MASK (3, 7)) >> 3U;
	  b32[--j] = base32_chars[value];
	}
      while (j > 8);
    }

  padding = quintets % 8 ? 8 - (quintets % 8) : 0;

  for (j = 0; j < padding; j++)
    b32[quintets + j] = '=';

  b32[quintets + padding] = '\0';
}
