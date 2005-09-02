#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/choppers.h>

#include <alloca.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <assert.h>

static int debug = 0;

int
main (int argc, char *argv[])
{
  errcode_t err;
  size_t source_size, chopped_size = 0;
  chop_stream_t *stream;
  chop_chopper_t *chopper;
  chop_buffer_t buffer;
  chop_log_t *chopper_log;

  if (argc < 2)
    return 1;

  stream = chop_class_alloca_instance (&chop_file_stream_class);
  chopper = chop_class_alloca_instance ((chop_class_t *)&chop_anchor_based_chopper_class);

  {
    /* Get the size of the file (for debugging purposes).  */
    int e;
    struct stat st;

    e = stat (argv[1], &st);
    if (e)
      {
	com_err (argv[0], e, "%s", argv[1]);
	return 1;
      }

    source_size = st.st_size;
  }

  err = chop_file_stream_open (argv[1], stream);
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

      if (size)
	{
	  write (1, chop_buffer_content (&buffer), size);
	  write (1, "\n---\n", 5);
	  chopped_size += size;
	}

      if (err == CHOP_STREAM_END)
	break;
    }

  assert (chopped_size == source_size);

  return 0;
}
