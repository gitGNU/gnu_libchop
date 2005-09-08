#include <chop/chop.h>
#include <chop/stores.h>


static void
store_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_block_store_t *store =
    (chop_block_store_t *)object;

  /* Initialize the block store fields so that method pointers are guaranteed
     to either be NULL or point to actual methods.  */
  store->name = NULL;
  store->block_exists = NULL;
  store->read_block = NULL;
  store->write_block = NULL;
  store->delete_block = NULL;
  store->first_key = NULL;
  store->next_key = NULL;
  store->close = NULL;
  store->sync = NULL;
}

CHOP_DEFINE_RT_CLASS (block_store, object,
		      store_ctor, NULL, /* No destructor */
		      NULL, NULL  /* No serializer/deserializer */);

/* The meta-class for all file-based stores, i.e. GDBM, TDB, etc.  */
CHOP_DEFINE_RT_CLASS (file_based_store_class, class,
		      NULL, NULL,
		      NULL, NULL);


errcode_t
chop_file_based_store_open (const chop_file_based_store_class_t *db_class,
			    const char *file, int open_flags, mode_t mode,
			    chop_block_store_t *store)
{
  if (!db_class->generic_open)
    return CHOP_ERR_NOT_IMPL;

  return (db_class->generic_open ((chop_class_t *)db_class, file,
				  open_flags, mode, store));
}
