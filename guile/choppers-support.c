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

/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <chop/chop-config.h>

#include <errno.h>
#include <assert.h>


static inline errcode_t
chop_fixed_size_chopper_open_alloc (chop_stream_t *input,
				    size_t block_size, int pad_blocks,
				    chop_chopper_t **chopper)
{
  errcode_t err;

  *chopper =
    gwrap_chop_malloc ((chop_class_t *)&chop_fixed_size_chopper_class);
  if (!*chopper)
    return ENOMEM;

  err = chop_fixed_size_chopper_init (input, block_size, pad_blocks, *chopper);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) *chopper,
	 (chop_class_t *) &chop_fixed_size_chopper_class);
      *chopper = NULL;
    }

  return err;
}

static inline errcode_t
chop_anchor_based_chopper_open_alloc (chop_stream_t *input,
				      size_t window_size,
				      unsigned long magic_fpr_mask,
				      chop_chopper_t **chopper)
{
  errcode_t err;

  *chopper =
    gwrap_chop_malloc ((chop_class_t *) &chop_anchor_based_chopper_class);
  if (!*chopper)
    return ENOMEM;

  err = chop_anchor_based_chopper_init (input, window_size, magic_fpr_mask,
					*chopper);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) *chopper,
	 (chop_class_t *) &chop_anchor_based_chopper_class);
      *chopper = NULL;
    }

  return err;
}

static errcode_t
chop_chopper_generic_open_alloc (const char *class_nickname,
				 chop_stream_t *input,
				 unsigned long typical_block_size,
				 chop_chopper_t **chopper)
{
  errcode_t err;
  char *class_realname;
  const chop_class_t *class;

  class_realname = alloca (strlen (class_nickname) + 20);
  strcpy (class_realname, class_nickname);
  strcat (class_realname, "_chopper");

  class = chop_class_lookup (class_realname);
  if (!class)
    return CHOP_ERR_NOT_FOUND;

  if (chop_object_get_class ((chop_object_t *)class)
      != &chop_chopper_class_class)
    return CHOP_INVALID_ARG;

  *chopper = gwrap_chop_malloc (class);
  if (!*chopper)
    return ENOMEM;

  err = chop_chopper_generic_open ((chop_chopper_class_t *)class, input,
				   typical_block_size, *chopper);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *chopper, class);
      *chopper = NULL;
      return err;
    }

  return err;
}


static inline errcode_t
chop_chopper_read_block_alloc_u8vector (chop_chopper_t *chopper,
					SCM *result)
{
  errcode_t err;
  size_t size;
  chop_buffer_t buffer;

  chop_buffer_init (&buffer, 0);

  err = chop_chopper_read_block (chopper, &buffer, &size);
  if (err)
    {
      chop_buffer_return (&buffer);
      *result = SCM_BOOL_F;

      return err;
    }

  assert (size == chop_buffer_size (&buffer));
  if (size)
    {
      unsigned char *block = (unsigned char *)scm_malloc (size);

      memcpy (block, chop_buffer_content (&buffer), size);
      *result = scm_take_u8vector (block, size);
    }
  else
    *result = SCM_BOOL_F;

  chop_buffer_return (&buffer);

  return err;
}
