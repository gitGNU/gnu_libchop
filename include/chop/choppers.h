/* libchop -- a utility library for distributed storage
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

/* Stream choppers.  */

#ifndef __CHOP_CHOPPERS_H__
#define __CHOP_CHOPPERS_H__

#include <chop/chop.h>
#include <chop/buffers.h>
#include <chop/streams.h>
#include <chop/objects.h>
#include <chop/logs.h>


/* Declare `chop_chopper_t' which inherits from `chop_object_t'.  */
CHOP_DECLARE_RT_CLASS (chopper, object,

		       chop_stream_t *stream;

		       size_t typical_block_size;
		       chop_error_t (* read_block) (struct chop_chopper *,
						    chop_buffer_t *, size_t *);
		       /* The CLOSE method is optional.  */
		       void (* close) (struct chop_chopper *););

/* The `chop_chopper_class_t' metaclass which provides a generic chopper
   creation method (a "factory").  */
CHOP_DECLARE_RT_CLASS (chopper_class, class,
		       chop_error_t (* generic_open) (chop_stream_t *,
						      size_t,
						      chop_chopper_t *););

/* These classes inherit from `chop_chopper_t'.  Both have
   `chop_chopper_class_t' as their class.  */
extern const chop_chopper_class_t chop_fixed_size_chopper_class;
extern const chop_chopper_class_t chop_whole_stream_chopper_class;
extern const chop_chopper_class_t chop_anchor_based_chopper_class;



/* Note:  I should have a look at EDelta at some point,
   http://www.diku.dk/~jacobg/edelta/ .  XXX */

/* Initialize CHOPPER as an instance of the CLASS chopper class with input
   stream INPUT.  The implementation of CLASS will choose default parameters
   for the specific chopper implementation (class-specific initialization
   methods are available below for fine-tuning of parameters).  It should,
   however, make sure that the average block size will be TYPICAL_BLOCK_SIZE
   bytes.  If TYPICAL_BLOCK_SIZE is zero, then the implementation of CLASS is
   free to choose any block size.  Return zero on success.  */
static __inline__ chop_error_t
chop_chopper_generic_open (const chop_chopper_class_t *class,
			   chop_stream_t *input,
			   size_t typical_block_size,
			   chop_chopper_t *chopper)
{
  if (class->generic_open)
    return (class->generic_open (input, typical_block_size, chopper));

  return CHOP_ERR_NOT_IMPL;
}


/* Initialize CHOPPER as a fixed-size stream chopper.  It will read data from
   input stream INPUT and cut it into blocks of BLOCK_SIZE bytes.  If
   PAD_BLOCKS is non-zero, the last block before CHOP_STREAM_END will be
   padded with zeros in order to be BLOCK_SIZE long.  */
extern chop_error_t
chop_fixed_size_chopper_init (chop_stream_t *input,
			      size_t block_size,
			      int pad_blocks,
			      chop_chopper_t *chopper);

/* Initialize CHOPPER as a whole-stream chopper which fetches data from
   INPUT.  CHOPPER will simply return one single block containing the entire
   contents of INPUT.  Consequently, this is very costly in terms of memory
   consumption.  Also, CHOP_WHOLE_STREAM_CHOPPER_CLASS does not honor at all
   the TYPICAL_BLOCK_SIZE argument of `chop_chopper_generic_open ()'.  */
extern chop_error_t
chop_whole_stream_chopper_open (chop_stream_t *input,
				chop_chopper_t *chopper);

/* Initialize CHOPPER as an anchor-based stream chopper.  It will read data
   from INPUT and produce variably sized blocks.  Anchor-based choppers
   implement the algorithm described by Udi Manber in "Finding similar files
   in a large file system" (1994).  Basically, this algorithm allows to
   deterministically find anchors within a file such that identical blocks
   among similar files may be discovered.  WINDOW_SIZE is the size of the
   sliding window used to compute fingerprints.  The paper recommends 50.
   Lower values may yield smaller blocks and better similarity detection.
   The probability of finding a block boundary, and therefore the average
   size of blocks, largely depends on the value of MAGIC_FPR_MASK, the mask
   that will be applied to each fingerprint computed in order to determine
   whether it should yield a block boundary.  The more bits are set in
   MAGIC_FPR_MASK, the less likely a fingerprint will match, and the larger
   the average block size will be.  */
extern chop_error_t
chop_anchor_based_chopper_init (chop_stream_t *input, size_t window_size,
				unsigned long magic_fpr_mask,
				chop_chopper_t *chopper);

/* If CHOPPER is an anchor-based chopper, return its log.  Return NULL
   otherwise.  */
extern chop_log_t *
chop_anchor_based_chopper_log (chop_chopper_t *chopper);

/* Return the input stream attached to CHOPPER.  */
static __inline__ chop_stream_t *
chop_chopper_stream (const chop_chopper_t *__chopper)
{
  return (__chopper->stream);
}

/* Set INPUT as the input stream attached to CHOPPER.  */
static __inline__ void chop_chopper_set_stream (chop_chopper_t *__chopper,
						chop_stream_t *__input)
{
  __chopper->stream = __input;
}

/* Read a block from CHOPPER and store its contents into BLOCK.  On success,
   BLOCK contains the exact contents of the block (i.e. the contents are
   "pushed"), SIZE contains the size of the block, and zero is returned.  On
   end of stream, *SIZE is set to zero and CHOP_STREAM_END is returned.  */
static __inline__ chop_error_t
chop_chopper_read_block (chop_chopper_t *__chopper,
			 chop_buffer_t *__block,
			 size_t *__size)
{
  return (__chopper->read_block (__chopper, __block, __size));
}

/* Return the "typical" size of the blocks produced by CHOPPER.  The meaning
   of "typical" actually depends on the chopper implementation.  The value
   returned can be used as a hint for the initial size of block buffers.  */
static __inline__ size_t
chop_chopper_typical_block_size (const chop_chopper_t *__chopper)
{
  return (__chopper->typical_block_size);
}

/* Deallocate any resources associated with CHOPPER.  Once closed, CHOPPER
   becomes unusable.  */
static __inline__ void
chop_chopper_close (chop_chopper_t *__chopper)
{
  if (__chopper->close)
    __chopper->close (__chopper);
}

#endif
