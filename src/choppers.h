/* Stream choppers.  */

#include "chop.h"


/* Choppers */

typedef struct chop_chopper chop_chopper_t;
typedef struct chop_fixed_size_chopper chop_fixed_size_chopper_t;

typedef struct chop_pad chop_pad_t;

extern errcode_t chop_chopper_fixed_size_init (chop_stream_t *input,
					       size_t block_size,
					       chop_fixed_size_chopper_t *
					       chopper);

extern chop_stream_t *chop_chopper_stream (const chop_chopper_t *);

extern void chop_chopper_set_stream (chop_chopper_t *chopper,
				     chop_stream_t *input);

/* Note: Use `GMemChunk' (slab allocator),
   http://developer.gnome.org/doc/API/glib/glib-memory-chunks.html */

extern errcode_t chop_chopper_read_block (chop_chopper_t *chopper,
					  chop_pad_t *block,
					  size_t *size);


/* Return PAD to its owner for deallocation.  */
extern void chop_pad_return (chop_pad_t *pad);

/* Return the owner of PAD.  */
extern chop_pad_pool_t *chop_pad_pool (const chop_pad_t *pad);

extern chop_pad_t *chop_pad_new ();


struct chop_chopper
{
  chop_stream_t *stream;

  errcode_t (* read_block) (struct chop_chopper *,
			    const char **, size_t *);
};

struct chop_fixed_size_chopper
{
  chop_chopper_t chopper;

  size_t block_size;
};

struct chop_pad
{
  chop_pad_owner_t *owner;
};

