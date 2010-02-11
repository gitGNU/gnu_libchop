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

#include <chop/chop.h>
#include <chop/choppers.h>

/* Declare `chop_whole_stream_chopper_t' which inherits from
   `chop_chopper_t'. */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (whole_stream_chopper, chopper,
				      chopper_class,  /* Metaclass */
				      /* stateless */);

/* A generic `open' method that chooses default parameters.  */
static errcode_t
wsc_generic_open (chop_stream_t *input, size_t block_size,
		  chop_chopper_t *chopper)
{
  return chop_whole_stream_chopper_open (input, chopper);
}


CHOP_DEFINE_RT_CLASS_WITH_METACLASS (whole_stream_chopper, chopper,
				     chopper_class,  /* Metaclass */

				     /* metaclass inits */
				     .generic_open = wsc_generic_open,

				     NULL, NULL, /* No ctor/dtor */
				     NULL, NULL, /* No copy/equalp */
				     NULL, NULL  /* No serial/deserial */);


static errcode_t
read_whole_stream  (chop_chopper_t *chopper,
		    chop_buffer_t *buffer, size_t *size)
{
  errcode_t err = 0;
  char local_buffer[4096];

  *size = 0;
  chop_buffer_clear (buffer);

  while (!err)
    {
      size_t amount = 0;

      err = chop_stream_read (chop_chopper_stream (chopper), local_buffer,
			      sizeof (local_buffer), &amount);
      if (((!err) || (err == CHOP_STREAM_END)) && (amount > 0))
	{
	  err = chop_buffer_append (buffer, local_buffer, amount);
	  if (err)
	    break;

	  *size += amount;
	}

      if (err)
	break;
    }

  if (err == CHOP_STREAM_END)
    {
      if (*size == 0)
	return err;

      return 0;
    }

  return err;
}

errcode_t
chop_whole_stream_chopper_open (chop_stream_t *input,
				chop_chopper_t *chopper)
{
  errcode_t err;

  err = chop_object_initialize ((chop_object_t *)chopper,
				(chop_class_t *)
				&chop_whole_stream_chopper_class);
  if (err)
    return err;

  chopper->stream = input;
  chopper->read_block = read_whole_stream;
  chopper->typical_block_size = 0;
  chopper->close = NULL;

  return 0;
}


/* arch-tag: 37d56338-ca75-44a6-984e-0bbdd9d1b20c
 */
