/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <errno.h>
#include <assert.h>

static void
chop_chopper_close_dealloc (chop_chopper_t *chopper)
{
  if (chopper)
    {
      chop_chopper_close (chopper);
      free (chopper);
    }
}

static __inline__ errcode_t
chop_fixed_size_chopper_open_alloc (chop_stream_t *input,
				    size_t block_size, int pad_blocks,
				    chop_chopper_t **chopper)
{
  errcode_t err;

  *chopper = malloc (chop_class_instance_size (&chop_fixed_size_chopper_class));
  if (!*chopper)
    return ENOMEM;

  err = chop_fixed_size_chopper_init (input, block_size, pad_blocks, *chopper);
  if (err)
    {
      free (*chopper);
      *chopper = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_anchor_based_chopper_open_alloc (chop_stream_t *input,
				      size_t window_size,
				      chop_chopper_t **chopper)
{
  errcode_t err;

  *chopper = malloc (chop_class_instance_size (&chop_anchor_based_chopper_class));
  if (!*chopper)
    return ENOMEM;

  err = chop_anchor_based_chopper_init (input, window_size, *chopper);
  if (err)
    {
      free (*chopper);
      *chopper = NULL;
    }

  return err;
}

static __inline__ errcode_t
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
      char *block = malloc (size);
      if (block)
	{
	  memcpy (block, chop_buffer_content (&buffer), size);
	  *result = scm_take_u8vector (block, size);
	}
      else
	{
	  err = ENOMEM;
	  *result = SCM_BOOL_F;
	}
    }
  else
    *result = SCM_BOOL_F;

  return err;
}
