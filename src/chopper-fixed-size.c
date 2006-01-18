#include <chop/chop.h>
#include <chop/choppers.h>
#include <chop/streams.h>

#include <alloca.h>
#include <errno.h>


/* The base `chop_chopper_t' definition.  */
CHOP_DEFINE_RT_CLASS (chopper, object,
		      NULL, NULL, /* No ctor/dtor */
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serial/deserial */);


/* The `chop_chopper_class_t' definition.  */
CHOP_DEFINE_RT_CLASS (chopper_class, class,
		      NULL, NULL, /* No ctor/dtor */
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serial/deserial */);



/* Class definitions.  */

static errcode_t fixed_size_chopper_ctor (chop_object_t *object,
					  const chop_class_t *class);


/* Declare `chop_fixed_size_chopper_t' which inherits from
   `chop_chopper_t'. */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (fixed_size_chopper, chopper,
				      chopper_class,  /* Metaclass */
				      size_t block_size;
				      int pad_blocks;);

/* A generic `open' method that chooses default parameters.  */
static errcode_t
chop_fs_generic_open (chop_stream_t *input, size_t block_size,
		      chop_chopper_t *chopper)
{
  if (block_size == 0)
    block_size = chop_stream_preferred_block_size (input);

  return chop_fixed_size_chopper_init (input, block_size,
				       0 /* no padding */,
				       chopper);
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (fixed_size_chopper, chopper,
				     chopper_class,  /* Metaclass */

				     /* metaclass inits */
				     .generic_open = chop_fs_generic_open,

				     fixed_size_chopper_ctor, NULL,
				     NULL, NULL, /* No copy/equalp */
				     NULL, NULL  /* No serial/deserial */);



static errcode_t chop_fixed_chopper_read_block (chop_chopper_t *,
						chop_buffer_t *block,
						size_t *);

/* The constructor.  */
static errcode_t
fixed_size_chopper_ctor (chop_object_t *object,
			 const chop_class_t *class)
{
  chop_fixed_size_chopper_t *fixed;

  fixed = (chop_fixed_size_chopper_t *)object;
  fixed->chopper.stream = NULL;
  fixed->chopper.read_block = chop_fixed_chopper_read_block;
  fixed->chopper.typical_block_size = 0;
  fixed->chopper.close = NULL;

  return 0;
}

errcode_t
chop_fixed_size_chopper_init (chop_stream_t *input,
			      size_t block_size,
			      int pad_blocks,
			      chop_chopper_t *chopper)
{
  chop_fixed_size_chopper_t *fixed;

  fixed = (chop_fixed_size_chopper_t *)chopper;
  chop_object_initialize ((chop_object_t *)chopper,
			  (chop_class_t *)&chop_fixed_size_chopper_class);

  fixed->chopper.stream = input;
  fixed->chopper.typical_block_size = block_size;
  fixed->block_size = block_size;
  fixed->pad_blocks = pad_blocks;

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

  *size = 0;
  while (*size < fixed->block_size)
    {
      amount = 0;
      err = chop_stream_read (input, &block[*size],
			      fixed->block_size - *size, &amount);
      *size += amount;

      if (CHOP_EXPECT_FALSE (err))
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

  if ((fixed->pad_blocks) && (*size < fixed->block_size))
    {
      /* Reached the end of stream: pad block with zeros */
      memset (&block[*size], '0', fixed->block_size - *size);
      *size = fixed->block_size;
    }

  err = chop_buffer_push (buffer, block, *size);

  return err;
}
