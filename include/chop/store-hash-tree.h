/* The hash tree "store".  */

#ifndef __CHOP_STORE_HASH_TREE_H__
#define __CHOP_STORE_HASH_TREE_H__

typedef struct chop_hash_tree_block_store chop_hash_tree_block_store_t;

extern errcode_t
chop_hash_tree_block_store_open (chop_block_store_t *backend,
				 chop_hash_method_t hash_method,
				 chop_hash_tree_block_store_t *store);


/* Implementation details */

struct chop_hash_tree_block_store
{
  chop_block_store_t store;

  /* The underlying block store, or NULL */
  chop_block_store_t *backend;

  chop_hash_method_t hash_method;
  int gcrypt_hash_method;
  size_t block_count;
  size_t key_size;
  chop_buffer_t block_keys;
};


#endif
