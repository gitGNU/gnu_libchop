#include <chop.h>
#include <streams.h>

int
main (int argc, char *argv[])
{
  errcode_t err;
  chop_file_stream_t stream;

  if (argc < 2)
    return 1;

  chop_init ();

  err = chop_file_stream_open (argv[1], &stream);
  if (err)
    {
      com_err (argv[1], err, "while opening %s", argv[1]);
      exit (1);
    }

  return 0;
}

