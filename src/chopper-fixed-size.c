#include <chop/chop.h>
#include <chop/choppers.h>
#include <chop/streams.h>

#include <alloca.h>
#include <errno.h>

static errcode_t chop_fixed_chopper_read_block (chop_chopper_t *,
						chop_buffer_t *block,
						size_t *);

errcode_t
chop_fixed_size_chopper_init (chop_stream_t *input,
			      size_t block_size,
			      chop_fixed_size_chopper_t *chopper)
{
  chopper->chopper.stream = input;
  chopper->chopper.read_block = chop_fixed_chopper_read_block;
  chopper->block_size = block_size;
  chopper->chopper.typical_block_size = block_size;

  return 0;
}

static errcode_t
chop_fixed_chopper_read_block (chop_chopper_t *chopper,
			       chop_buffer_t *buffer,
			       size_t *size)
{
  errcode_t err = 0;
  size_t amount;
  char *block;
  chop_fixed_size_chopper_t *fixed = (chop_fixed_size_chopper_t *)chopper;
  chop_stream_t *input = chop_chopper_stream (chopper);

  block = (char *)alloca (fixed->block_size);
  if (!block)
    return ENOMEM;

  *size = 0;
  while (*size < fixed->block_size)
    {
      amount = 0;
      err = chop_stream_read (input, &block[*size],
			      fixed->block_size, &amount);
      *size += amount;

      if (err)
	{
	  if (err == CHOP_STREAM_END)
	    break;
	  else
	    return err;
	}
    }

  if (*size == 0)
    /* Tried to read past the end of stream */
    return err;

  if (*size < fixed->block_size)
    {
      /* Reached the end of stream: pad block with zeros */
      memset (&block[*size], '0', fixed->block_size - *size);
      *size = fixed->block_size;
    }

  err = chop_buffer_push (buffer, block, fixed->block_size);

  return err;
}
