#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>

#include <alloca.h>

static int debug = 1;

int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_stream_t *stream;
  chop_chopper_t *chopper;
  chop_buffer_t buffer;
  chop_log_t *chopper_log;

  if (argc < 2)
    return 1;

  stream = chop_class_alloca_instance (&chop_file_stream_class);
  chopper = chop_class_alloca_instance (&chop_anchor_based_chopper_class);

  err = chop_file_stream_open (argv[1], (chop_file_stream_t *)stream);
  if (err)
    {
      com_err (argv[0], err, "%s", argv[1]);
      return 1;
    }

  err = chop_anchor_based_chopper_init (stream, 10, chopper);
  if (err)
    {
      com_err (argv[0], err, "anchor-based-chopper");
      return 1;
    }

  if (debug)
    {
      /* Output debugging messages to `stderr'.  */
      chopper_log = chop_anchor_based_chopper_log (chopper);
      chop_log_attach (chopper_log, 2, 0);
    }

  chop_buffer_init (&buffer, chop_chopper_typical_block_size (chopper));
  while (1)
    {
      size_t size;
      err = chop_chopper_read_block (chopper, &buffer, &size);
      if ((err) && (err != CHOP_STREAM_END))
	{
	  com_err (argv[0], err, "while reading block");
	  return 2;
	}

      write (1, chop_buffer_content (&buffer), size);
      write (1, "\n---\n", 5);

      if (err == CHOP_STREAM_END)
	break;
    }

  return 0;
}
