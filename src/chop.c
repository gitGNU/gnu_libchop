#include <chop.h>
#include <streams.h>

errcode_t
chop_init (void)
{
  initialize_chop_error_table ();
}

size_t
chop_stream_preferred_block_size (const chop_stream_t *stream)
{
  return (stream->block_size (stream));
}
