/* Block stores.  */

#include "chop.h"

struct chop_block_store
{
  char *name;

  errcode_t (* read_block) (struct chop_block_store *,
			    const chop_hash_t, chop_block_t *);
};


extern errcode_t chop_gdbm_store_open (const char *name);

/* extern errcode_t chop_dht_store_open (pid_t dht); */

extern errcode_t chop_store_write_blocks (chop_block_store_t *store,
					  const chop_block_t *blocks,
					  enum chop_hash_method method,
					  size_t block_count,
					  const chop_block_t *
					  (* resolve_collision)
					  (const chop_block_t *old,
					   const chop_block_t *new),
					  size_t *written);

extern errcode_t chop_store_read_block (chop_block_store_t *store,
					const chop_hash_t key,
					char *buffer);

extern errcode_t chop_store_delete_block (chop_block_store_t *store,
					  const chop_hash_t key);

extern errcode_t chop_store_sync (chop_block_store_t *store);

