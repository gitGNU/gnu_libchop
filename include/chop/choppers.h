/* Stream choppers.  */

#ifndef __CHOP_CHOPPERS_H__
#define __CHOP_CHOPPERS_H__

#include <chop/chop.h>
#include <chop/buffers.h>
#include <chop/streams.h>
#include <chop/serializable.h>
#include <chop/logs.h>


/* Declare `chop_chopper_t' which inherits from `chop_object_t'.  */
CHOP_DECLARE_RT_CLASS (chopper, object,
		       chop_stream_t *stream;

		       size_t typical_block_size;
		       errcode_t (* read_block) (struct chop_chopper *,
						 chop_buffer_t *, size_t *););

/* Declare `chop_fixed_size_chopper_t' which inherits from
   `chop_chopper_t'. */
CHOP_DECLARE_RT_CLASS (fixed_size_chopper, chopper,
		       size_t block_size;
		       int pad_blocks;);

/* This class inherits from `chop_chopper_t'.  */
extern const chop_class_t chop_anchor_based_chopper_class;



/* Initialize CHOPPER as a fixed-size stream chopper.  It will read data from
   input stream INPUT and cut it into blocks of BLOCK_SIZE bytes.  If
   PAD_BLOCKS is non-zero, the last block before CHOP_STREAM_END will be
   padded with zeros in order to be BLOCK_SIZE long.  */
extern errcode_t
chop_fixed_size_chopper_init (chop_stream_t *input,
			      size_t block_size,
			      int pad_blocks,
			      chop_fixed_size_chopper_t *chopper);

/* Initialize CHOPPER as an anchor-based stream chopper.  It will read data
   from INPUT and produce variably sized blocks.  Anchor-based choppers
   implement the algorithm described by Udi Manber in "Finding similar files
   in a large file system" (1994).  Basically, this algorithm allows to
   deterministically find anchors within a file such that identical blocks
   among similar files may be discovered.  WINDOW_SIZE is the size of the
   sliding window used to compute fingerprints.  The paper recommends 50.
   Lower values may yield smaller blocks and better similarity detection.  */
extern errcode_t
chop_anchor_based_chopper_init (chop_stream_t *input, size_t window_size,
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

/* Note: Use `GMemChunk' (slab allocator),
   http://developer.gnome.org/doc/API/glib/glib-memory-chunks.html */

/* Read a block from CHOPPER and store its contents into BLOCK.  On success,
   BLOCK contains the exact contents of the block (i.e. the contents are
   "pushed"), SIZE contains the size of the block, and zero is returned.  On
   end of stream, *SIZE is set to zero and CHOP_STREAM_END is returned.  */
static __inline__ errcode_t
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

#endif
