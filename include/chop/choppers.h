/* Stream choppers.  */

#ifndef __CHOP_CHOPPERS_H__
#define __CHOP_CHOPPERS_H__

#include <chop/chop.h>
#include <chop/buffers.h>


struct chop_chopper
{
  chop_stream_t *stream;

  errcode_t (* read_block) (struct chop_chopper *,
			    chop_buffer_t *, size_t *);
};


/* Choppers */

typedef struct chop_chopper chop_chopper_t;
typedef struct chop_fixed_size_chopper chop_fixed_size_chopper_t;


extern errcode_t chop_fixed_size_chopper_init (chop_stream_t *input,
					       size_t block_size,
					       chop_fixed_size_chopper_t *
					       chopper);

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



struct chop_fixed_size_chopper
{
  chop_chopper_t chopper;

  size_t block_size;
};

#endif
