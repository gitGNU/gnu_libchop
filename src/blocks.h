#include <unistd.h>

struct chop_block
{
  size_t size;
  size_t number;
  const void *content;
};

