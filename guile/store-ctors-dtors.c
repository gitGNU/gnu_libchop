/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <stdlib.h>
#include <errno.h>

static chop_block_store_t *
chop_dummy_block_store_open_alloc (const char *name)
{
  chop_block_store_t *store;

  store = malloc (chop_class_instance_size (&chop_dummy_block_store_class));
  if (!store)
    return NULL;

  chop_dummy_block_store_open (name, store);

  return store;
}

static void
chop_store_close_dealloc (chop_block_store_t *store)
{
  if (store)
    {
      chop_store_close (store);
      free (store);
    }
}
