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
