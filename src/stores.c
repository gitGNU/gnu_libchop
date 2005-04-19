#include <chop/chop.h>
#include <chop/stores.h>


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
