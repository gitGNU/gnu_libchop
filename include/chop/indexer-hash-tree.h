
#ifndef __CHOP_STORE_HASH_TREE_H__
#define __CHOP_STORE_HASH_TREE_H__

/* The hash tree indexer.  */

#include <chop/chop.h>
#include <chop/indexers.h>
#include <chop/buffers.h>
#include <chop/hash.h>

typedef struct chop_hash_tree_indexer chop_hash_tree_indexer_t;


/* Initialize the hash tree indexer HTREE.  Blocks will be symmetrically
   ciphered using a hash produced by the CONTENT_HASH_METHOD algorithm.
   Block keys are then computed using KEY_HASH_METHOD.  */
extern errcode_t
chop_hash_tree_indexer_open (chop_hash_method_t content_hash_method,
			     chop_hash_method_t key_hash_method,
			     size_t keys_per_block,
			     chop_hash_tree_indexer_t *htree);


/* Implementation details */

struct chop_hash_tree_indexer
{
  chop_indexer_t indexer;

  /* Hash method and message digest size (in bytes) */
  chop_hash_method_t hash_method;
  int                gcrypt_hash_method;
  size_t             key_size;
  size_t             keys_per_block;
};



#endif
