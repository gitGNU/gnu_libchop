#include <chop/chop.h>
#include <chop/streams.h>

errcode_t
chop_init (void)
{
  initialize_chop_error_table ();
  return 0;
}

