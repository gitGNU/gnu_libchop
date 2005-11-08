#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/filters.h>

#include <errno.h>


/* Define the `chop_filter_t' class.  */

static errcode_t
filter_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_filter_t *filter = (chop_filter_t *)object;

  filter->input_fault_handler.handle = NULL;
  filter->input_fault_handler.data = NULL;
  filter->output_fault_handler.handle = NULL;
  filter->output_fault_handler.data = NULL;
  filter->within_fault_handler = 0;

  return 0;
}

CHOP_DEFINE_RT_CLASS (filter, object,
		      filter_ctor, NULL,
		      NULL, NULL);



/* An input buffer.  */
typedef struct
{
  const char *buffer;
  size_t size;
  size_t read;
  chop_filter_fault_handler_t prev_handler;
} input_buffer_t;

static errcode_t
handle_input_fault_from_buffer (chop_filter_t *filter, size_t amount,
				void *data)
{
  errcode_t err;
  size_t pushed;
  input_buffer_t *ibuffer = (input_buffer_t *)data;

  if (!ibuffer->size)
    /* We've read it all.  */
    return CHOP_FILTER_UNHANDLED_FAULT;

  amount = (amount > ibuffer->size) ? ibuffer->size : amount;
  pushed = 0;
  err = chop_filter_push (filter, ibuffer->buffer, amount, &pushed);

  ibuffer->buffer += pushed;
  ibuffer->size -= pushed;
  ibuffer->read += pushed;

  return err;
}

errcode_t
chop_filter_set_input_from_buffer (chop_filter_t *filter,
				   const char *input, size_t input_size)
{
  input_buffer_t *ibuffer = malloc (sizeof (input_buffer_t));
  if (!ibuffer)
    return ENOMEM;

  ibuffer->buffer = input;
  ibuffer->size = input_size;
  ibuffer->read = 0;
  ibuffer->prev_handler = chop_filter_input_fault_handler (filter);
  chop_filter_set_input_fault_handler (filter,
				       handle_input_fault_from_buffer,
				       ibuffer);

  return 0;
}

void
chop_filter_finish_input_from_buffer (chop_filter_t *filter,
				      size_t *bytes_read)
{
  chop_filter_fault_handler_t handler;
  input_buffer_t *ibuffer;

  handler = chop_filter_input_fault_handler (filter);
  if (handler.handle != handle_input_fault_from_buffer)
    /* Result unspecified.  */
    return;

  ibuffer = (input_buffer_t *)handler.data;
  *bytes_read = ibuffer->read;

  chop_filter_set_input_fault_handler (filter,
				       ibuffer->prev_handler.handle,
				       ibuffer->prev_handler.data);

  free (ibuffer);
}

errcode_t
chop_filter_through (chop_filter_t *filter,
		     const char *input, size_t input_size,
		     chop_buffer_t *output)
{
  errcode_t err;
  size_t bytes_read;
  int flush = 0;

  /* Set FILTER's input fault handler such that it will fetch data from
     INPUT.  A more efficient way would consist in avoiding the use of fault
     handlers at all (XXX).  */
  err = chop_filter_set_input_from_buffer (filter, input, input_size);
  if (err)
    return err;

  chop_buffer_clear (output);
  while (1)
    {
      char block[1024];
      size_t pulled;

      err = chop_filter_pull (filter, flush,
			      block, sizeof (block), &pulled);
      if (err)
	{
	  if (err == CHOP_FILTER_EMPTY)
	    {
	      if (!flush)
		flush = 1;
	      else
		{
		  err = 0;
		  break;
		}
	    }
	  else
	    break;
	}

      err = chop_buffer_append (output, block, pulled);
      if (err)
	break;
    }

  chop_filter_finish_input_from_buffer (filter, &bytes_read);

  return err;
}
