/* This file implements a "hash tree" data structures for encoding the
   contents of a file.  This structure is also known as "Merkle's Hash
   Trees", named after its original author [1].

   [1]  Ralph C. Merkel, Protocols for Public Key Cryptosystems,
        IEEE Symp. on Security and Privacy, pp. 122--134, 1980.  */

#include <chop/chop.h>
#include <chop/indexers.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>

/* Glibc's obstacks */
/* #include <obstack.h> */

/* libgcrypt */
#include <gcrypt.h>

#include <chop/chop-config.h>

#if HAVE_NETINET_IN_H
/* `htons ()' and friends */
# include <netinet/in.h>
#else
# error "Where can I find `htons ()'?"
#endif


/* Define CHOP_INDEXER_CLASS.  */
CHOP_DEFINE_RT_CLASS (indexer, object,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);



/* Declare and define the `chop_hash_tree_indexer_t' class and its run-time
   representation CHOP_HASH_TREE_INDEXER_CLASS.  */
CHOP_DECLARE_RT_CLASS (hash_tree_indexer, indexer,

		       /* Hash method and message digest size (in bytes) */
		       chop_hash_method_t block_id_hash_method;
		       chop_hash_method_t hash_key_hash_method;
		       size_t             key_size;
		       size_t             keys_per_block;

		       /* Cipher handle */
		       chop_cipher_handle_t cipher_handle;

		       /* For debugging purposes */
		       chop_log_t         log;);

CHOP_DEFINE_RT_CLASS (hash_tree_indexer, indexer,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);



/* Internal class declarations.  */

/* Declare `chop_chk_index_handle_t' that inherits from the
   `chop_index_handle_t' class.  */
CHOP_DECLARE_RT_CLASS (chk_index_handle, index_handle,
		       /* The size of the indexed block, in bytes */
		       size_t block_size;

		       /* The method used to compute its identifier */
		       chop_hash_method_t block_id_hash_method;

		       /* The actual block identifier and its size */
		       char block_id[80];
		       size_t block_id_size;

		       unsigned ciphered:1;

		       /* Relevant only if CIPHERED is 1 */
		       chop_hash_method_t hash_key_hash_method;
		       char hash_key[80];
		       size_t hash_key_size;);



/* Internal exported class definitions.  */

/* Serialize OBJECT (which is assumed to be a `chop_index_handle_t' object)
   into BUFFER according to METHOD.  */
static errcode_t
chk_index_handle_serialize (const chop_object_t *object,
			    chop_serial_method_t method,
			    chop_buffer_t *buffer)
{
  chop_chk_index_handle_t *handle =
    (chop_chk_index_handle_t *)object;

  switch (method)
    {
    case CHOP_SERIAL_ASCII:
      {
	/* Return something like
	   "SHA1:eabe1ca1f3c7ca148ce2fe5954f52ef9a0f0082a/39a".  */
	char *hex;
	size_t hash_method_len;
	const char *hash_method;

	hash_method = chop_hash_method_name (handle->block_id_hash_method);

	hash_method_len = strlen (hash_method);
	hex = alloca (hash_method_len + 1 + (handle->block_id_size * 2) + 1);
	memcpy (hex, hash_method, hash_method_len);
	hex[hash_method_len] = ':';
	chop_buffer_to_hex_string (handle->block_id, handle->block_id_size,
				   &hex[hash_method_len + 1]);
	chop_buffer_push (buffer, hex, strlen (hex) /* strip trailing \0 */);

	if (handle->ciphered)
	  {
	    /* Append the block's ciphering key.  */
	    hash_method = chop_hash_method_name (handle->hash_key_hash_method);
	    hash_method_len = strlen (hash_method);
	    hex = alloca (1 + hash_method_len + 1
			  + (handle->block_id_size * 2) + 1);
	    hex[0] = ',';
	    memcpy (hex + 1, hash_method, hash_method_len);
	    hex[hash_method_len + 1] = ':';
	    chop_buffer_to_hex_string (handle->hash_key, handle->hash_key_size,
				       &hex[hash_method_len + 2]);

	    chop_buffer_append (buffer, hex, strlen (hex));
	  }

	/* Append a slash and the indexed block size.  */
	hex[0] = '/';
	sprintf (hex + 1, "%x", handle->block_size);
	chop_buffer_append (buffer, hex, strlen (hex) + 1);

	return 0;
      }

    case CHOP_SERIAL_BINARY:
      {
	unsigned char ciphered;
	unsigned char hash_method = (unsigned char)handle->block_id_hash_method;

	ciphered = (unsigned char)handle->ciphered;
	chop_buffer_push (buffer, &ciphered, 1);
	chop_buffer_append (buffer, &hash_method, 1);
	chop_buffer_append (buffer, handle->block_id, handle->block_id_size);
	if (handle->ciphered)
	  {
	    hash_method = (char)handle->hash_key_hash_method;
	    chop_buffer_append (buffer, &hash_method, 1);
	    chop_buffer_append (buffer,
				handle->hash_key, handle->hash_key_size);
	  }

	/* FIXME:  Append the block size.  */

	return 0;
      }

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return CHOP_ERR_NOT_IMPL;
}


/* Return the size of a binary serialization of INDEX.  */
static inline size_t
chop_chk_index_handle_size (const chop_chk_index_handle_t *index)
{
  size_t size;

  size = index->block_id_size;
  if (index->ciphered)
    size += index->hash_key_size;
  size += 4;  /* always store the block size on 4 bytes */

  return (size);
}

/* Read from BUFFER (SIZE-byte long) an ASCII serialization of a hash and
   return its hash method in METHOD and its content in HASH.  Return in COUNT
   the number of bytes read from BUFFER.  */
static errcode_t
read_ascii_hash (const char *buffer, size_t size,
		 chop_hash_method_t *method, size_t *hash_size,
		 char *hash, size_t *count)
{
  errcode_t err;
  char *colon, *hash_name;
  const char *end;
  size_t hash_name_len;

  if (size < 5)
    return CHOP_DESERIAL_TOO_SHORT;

  colon = strchr (buffer, ':');
  if (!colon)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  hash_name_len = colon - buffer;
  hash_name = alloca (hash_name_len + 1);

  memcpy (hash_name, buffer, hash_name_len);
  hash_name[hash_name_len] = '\0';
  err = chop_hash_method_lookup (hash_name, method);
  if (err)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  if (size < hash_name_len)
    return CHOP_DESERIAL_TOO_SHORT;

  chop_hex_string_to_buffer (buffer + hash_name_len + 1,
			     size - (hash_name_len + 1),
			     hash, &end);

  /* For hash keys, the actual size does not depend on the hash algorithm
     being used: it may be larger or smaller, depending on the ciphering
     algorithm that was used.  */
  *hash_size = end - (buffer + hash_name_len + 1);
  *hash_size >>= 1;

  *count = end - buffer;

  return 0;
}

static errcode_t
chk_index_handle_deserialize (const char *buffer, size_t size,
			      chop_serial_method_t method,
			      chop_object_t *object)
{
  errcode_t err;
  chop_chk_index_handle_t *handle =
    (chop_chk_index_handle_t *)object;

  switch (method)
    {
      case CHOP_SERIAL_ASCII:
	{
	  size_t offset = 0, count;

	  /* Read the block ID.  */
	  err = read_ascii_hash (buffer, size,
				 &handle->block_id_hash_method,
				 &handle->block_id_size,
				 handle->block_id,
				 &count);
	  offset += count;

	  /* For block IDs, the size of the ID that has been read must be
	     equal to that of a HASH produced by BLOCK_ID_HASH_METHOD.  */
	  if (handle->block_id_size !=
	      chop_hash_size (handle->block_id_hash_method))
	    return CHOP_DESERIAL_CORRUPT_INPUT;

	  if ((!err) && (buffer[offset] == ','))
	    {
	      /* Read the hash key.  */
	      offset++;
	      err = read_ascii_hash (buffer + offset,
				     size - count,
				     &handle->hash_key_hash_method,
				     &handle->hash_key_size,
				     handle->hash_key,
				     &count);
	      handle->ciphered = 1;
	      offset += count;
	    }
	  else
	    handle->ciphered = 0;

	  if (!err)
	    {
	      if (buffer[offset] != '/')
		err = CHOP_DESERIAL_CORRUPT_INPUT;
	      else
		{
		  /* Read the block size.  */
		  char *end;
		  long int block_size;

		  offset++;
		  block_size = strtol (buffer + offset, &end, 16);
		  if (end == buffer + offset)
		    err = CHOP_DESERIAL_CORRUPT_INPUT;
		  else if (block_size < 0)
		    err = CHOP_DESERIAL_CORRUPT_INPUT;
		  /* FIXME:  Check for possible overflow.  */
		  else
		    handle->block_size = block_size;
		}
	    }

	  break;
	}

    default:
      return CHOP_ERR_NOT_IMPL;
    }

  return err;
}

CHOP_DEFINE_RT_CLASS (index_handle, object,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);

CHOP_DEFINE_RT_CLASS (chk_index_handle, index_handle,
		      NULL, NULL, /* No constructor/destructor */
		      chk_index_handle_serialize,
		      chk_index_handle_deserialize);



/* Internal data structures.  */

/* This represents a key block or "i-node", i.e. a block whose content is a
   vector of keys of data blocks or key blocks.  */
typedef struct key_block
{
  /* Pointer to the key block that contains a pointer to it */
  struct key_block *parent;

  /* KEY_COUNT concatenated block keys, each of which should be KEY_SIZE
     long.  This contains a header which is KEY_BLOCK_HEADER_SIZE bytes
     long.  */
#define KEY_BLOCK_HEADER_SIZE  (10)
#define KEY_BLOCK_MAGIC_1      'H'
#define KEY_BLOCK_MAGIC_2      'T'
  char *keys;
  size_t key_count;
  size_t key_size;

  /* Depth of this block's subtree */
  size_t depth;

  /* Hash method for key block keys (copied from its indexer) */
  chop_hash_method_t block_id_hash_method;
  chop_hash_method_t hash_key_hash_method;

  /* If not NULL, the cipher handle to use when encrypting this block */
  chop_cipher_handle_t cipher_handle;

  /* Pointer to the indexer's log (for debugging purposes) */
  chop_log_t *log;
} key_block_t;

/* A tree of key blocks.  */
typedef struct key_block_tree
{
  /* The current bottom-most key block */
  key_block_t *current;

  /* Number of keys per key block */
  size_t keys_per_block;

  /* Key size and algorithm */
  size_t key_size;
  chop_hash_method_t block_id_hash_method;
#if 0
  int g_block_id_hash_method;
  int g_hash_key_hash_method;
#endif

  /* The block ciphering algorithm, or NULL */
  chop_cipher_handle_t cipher_handle;

  /* Only relevant if CIPHER_HANDLE is not NULL:  method used to compute the
     key when ciphering blocks.  */
  chop_hash_method_t   hash_key_hash_method;

  /* Meta-data block store: the block store where key blocks are flushed */
  chop_block_store_t *metadata_store;

  /* Pointer to the indexer's log (for debugging purposes) */
  chop_log_t *log;
} key_block_tree_t;


#if 0
/* A vector of zeros.  */
static char zero_vector[4000] = { 0, };
#endif


/* Initialize key block tree TREE.  */
static inline void
chop_block_tree_init (key_block_tree_t *tree, size_t keys_per_block,
		      chop_cipher_handle_t cipher_handle,
		      chop_hash_method_t hash_key_hash_method,
		      chop_hash_method_t block_id_hash_method,
		      size_t key_size,
		      chop_block_store_t *metadata_store, chop_log_t *log)
{
  tree->current = NULL;
  tree->keys_per_block = keys_per_block;
  tree->key_size = key_size;
  tree->block_id_hash_method = block_id_hash_method;
  tree->hash_key_hash_method = hash_key_hash_method;
  tree->cipher_handle = cipher_handle;
  tree->metadata_store = metadata_store;
  tree->log = log;
}

/* Fill in the KEYS field of BLOCK with a header.  This should be called by
   CHOP_KEY_BLOCK_FLUSH, right before BLOCK is actually written.  */
static inline void
chop_key_block_fill_header (key_block_t *block)
{
  unsigned char *header = (unsigned char *)block->keys;
  size_t count = block->key_count;
  size_t depth = block->depth;

  *(header++) = KEY_BLOCK_MAGIC_1;
  *(header++) = KEY_BLOCK_MAGIC_2;

  *(header++) = (count & 0xff);  count >>= 8;
  *(header++) = (count & 0xff);  count >>= 8;
  *(header++) = (count & 0xff);  count >>= 8;
  *(header++) = (count & 0xff);  count >>= 8;
  assert (!count);

  *(header++) = (depth & 0xff);  depth >>= 8;
  *(header++) = (depth & 0xff);  depth >>= 8;
  assert (!depth);

  assert (header - (unsigned char *)block->keys <= KEY_BLOCK_HEADER_SIZE);

  /* Don't leave uninitialized bytes.  */
  memset (header, 0,
	  KEY_BLOCK_HEADER_SIZE - (header - (unsigned char *)block->keys));
}


/* Make KEY point to a ciphering key of at most KEY_SIZE bytes.  Fill KEY
   with data from SOURCE, a SOURCESIZE-byte buffer, cycling if SOURCE is
   smaller than ALGO's key size.  */
static inline void
cipher_make_suitable_key (char *key, size_t key_size,
			  const char *source, size_t sourcesize)
{
  size_t source_bytes_left = (sourcesize), source_offset = 0;
  size_t key_offset = 0;

  while (key_size)
    {
      size_t amount;

      amount = (key_size > source_bytes_left)
	? source_bytes_left : key_size;

      memcpy ((key) + key_offset,
	      (source) + source_offset, amount);
      source_bytes_left -= amount;
      key_size -= amount;
      source_offset += amount;
      key_offset += amount;

      if (!source_bytes_left)
	/* Rewind.  */
	source_offset = 0, source_bytes_left = (sourcesize);
    }
}

/* Index and cipher (if needed) SIZE bytes starting at BUFFER, and store them
   into STORE according to the block ID hash method described by the other
   parameters: if CIPHER_HANDLE is not NULL, BUFFER will be encrypted using a
   key given by applying HASH_KEY_HASH_METHOD to the cleartext;
   BLOCK_ID_HASH_METHOD is the hash method used to compute the block ID.
   Update INDEX with the index of the newly indexed block.  */
static errcode_t
chop_hash_tree_index_block (chop_cipher_handle_t cipher_handle,
			    chop_hash_method_t hash_key_hash_method,
			    chop_hash_method_t block_id_hash_method,
			    chop_block_store_t *store,
			    const char *buffer,
			    size_t size,
			    chop_chk_index_handle_t *index)
{
  errcode_t err = 0;
  size_t total_size;
  chop_block_key_t key;
  char *block_content;
  int g_hash_key_hash_method, g_block_id_hash_method;

  if (cipher_handle)
    {
      /* Encrypt the block using its hash as a key.  */
      size_t hash_key_size, cipher_key_size, block_size, padding_size;
      chop_cipher_algo_t algo;
      char *hash_key;

      algo = chop_cipher_algorithm (cipher_handle);
      block_size = chop_cipher_algo_block_size (algo);
      cipher_key_size = chop_cipher_algo_key_size (algo);
      assert (block_size > 0);

      g_hash_key_hash_method =
	chop_hash_method_gcrypt_name (hash_key_hash_method);
      hash_key_size = chop_hash_size (hash_key_hash_method);
      hash_key = alloca (hash_key_size);
      gcry_md_hash_buffer (g_hash_key_hash_method,
			   hash_key, buffer, size);

      /* Most ciphering algorithm need the input size to be a multiple of
	 their ciphering block size.  */
      padding_size = (size % block_size)
	? (block_size - (size % block_size)) : 0;
      total_size = size + padding_size;
      block_content = alloca (total_size);
      if (padding_size)
	/* Pad with zeros.  The actual size of the block will be stored in
	   its index anyway.  */
	memset (block_content + size, 0, padding_size);

      /* Make INDEX->HASH_KEY a key of size CIPHER_KEY_SIZE from HASH_KEY
	 which is HASH_KEY_SIZE-byte long.  */
      cipher_make_suitable_key (index->hash_key, cipher_key_size,
				hash_key, hash_key_size);

      /* Provide exactly the right key size.  */
      err = chop_cipher_set_key (cipher_handle, index->hash_key,
				 cipher_key_size);

      while (err == CHOP_CIPHER_WEAK_KEY)
	{
	  /* The key we wanted to use was considered weak.  Hence, we use a
	     random one, hoping that it won't be considered weak...  */
	  chop_randomize (index->hash_key, cipher_key_size);
	  err = chop_cipher_set_key (cipher_handle, index->hash_key,
				     cipher_key_size);
	}

#if 0
      if (!err)
	err = chop_cipher_set_iv (cipher_handle, zero_vector,
				  block_size);
#endif

      if (!err)
	err = chop_cipher_encrypt (cipher_handle,
				   block_content, total_size,
				   buffer, total_size);


      /* Set up the index for this block.  */
      index->ciphered = 1;
      index->hash_key_size = cipher_key_size;
    }
  else
    {
      /* Leave the block unencrypted.  */
      block_content = (char *)buffer;
      total_size = size;
      index->ciphered = 0;
      index->hash_key_size = 0;
    }

  if (!err)
    {
      /* Compute the block key.  */
      size_t block_id_size;

      g_block_id_hash_method =
	chop_hash_method_gcrypt_name (block_id_hash_method);
      block_id_size = chop_hash_size (block_id_hash_method);
      gcry_md_hash_buffer (g_block_id_hash_method, index->block_id,
			   block_content, total_size);
      index->block_id_size = block_id_size;

      /* Write this key block to backing store.  */
      chop_block_key_init (&key, index->block_id, block_id_size, NULL, NULL);
      err = chop_store_write_block (store, &key, block_content, total_size);
    }

  index->block_size = err ? 0 : size;

  return err;
}

/* Flush BLOCK to METADATA and return its index in INDEX.  The memory used by
   BLOCK can now be reused.  */
static errcode_t
chop_key_block_flush (key_block_t *block, chop_block_store_t *metadata,
		      chop_chk_index_handle_t *index)
{
  errcode_t err;
  size_t block_size;

  chop_log_printf (block->log,
		   "key_block_flush: block %p with %u keys being flushed "
		   "to store %p\n",
		   block, block->key_count, metadata);

  chop_key_block_fill_header (block);

  /* FIXME:  Maybe we should pad BLOCK->KEYS with zero and create fixed-size
     blocks.  */
  block_size = KEY_BLOCK_HEADER_SIZE + (block->key_count * block->key_size);

  err = chop_hash_tree_index_block (block->cipher_handle,
				    block->hash_key_hash_method,
				    block->block_id_hash_method,
				    metadata,
				    block->keys, block_size,
				    index);
  return err;
}

/* Allocate and initialize a new key block and return it in BLOCK.  */
static inline errcode_t
chop_key_block_new (size_t keys_per_block,
		    chop_cipher_handle_t cipher_handle,
		    chop_hash_method_t hash_key_hash_method,
		    chop_hash_method_t block_id_hash_method,
		    size_t key_size,
		    chop_log_t *log,
		    key_block_t **block)
{
  *block = (key_block_t *)malloc (sizeof (key_block_t));
  if (!*block)
    return ENOMEM;

  (*block)->keys = malloc (KEY_BLOCK_HEADER_SIZE +
			   (keys_per_block * key_size));
  if (!(*block)->keys)
    {
      free (*block);
      return ENOMEM;
    }

  (*block)->parent = NULL;
  (*block)->depth = 0;
  (*block)->key_count = 0;
  (*block)->key_size = key_size;
  (*block)->block_id_hash_method = block_id_hash_method;
  (*block)->hash_key_hash_method = hash_key_hash_method;
  (*block)->cipher_handle = cipher_handle;
  (*block)->log = log;

  return 0;
}

/* Append INDEX to BLOCK which can contain at most KEYS_PER_BLOCK block keys.
   When full, BLOCK is written to METADATA and its contents are reset.  */
static errcode_t
chop_key_block_add_index (key_block_t *block, size_t keys_per_block,
			  chop_block_store_t *metadata,
			  const chop_chk_index_handle_t *index)
{
  errcode_t err;
  size_t offset;

  if (block->key_count + 1 >= keys_per_block)
    {
      /* This key block is full:
	 1.  flush it;
	 2.  add its key to its parent key block;
	 3.  reset it and append KEY to it.  */
      key_block_t *parent = block->parent;
      chop_chk_index_handle_t block_index;

      err = chop_key_block_flush (block, metadata, &block_index);
      if (err)
	return err;

      if (!parent)
	{
	  /* BLOCK is orphan: create him a parent key block.  */
	  err = chop_key_block_new (keys_per_block,
				    block->cipher_handle,
				    block->hash_key_hash_method,
				    block->block_id_hash_method,
				    block->key_size,
				    block->log, &parent);
	  if (err)
	    return err;

	  parent->depth = block->depth + 1;
	  block->parent = parent;
	}

      err = chop_key_block_add_index (parent, keys_per_block, metadata,
				      &block_index);
      if (err)
	return err;

      block->key_count = 0;
    }

  /* Append the content of INDEX (this is similar to its binary
     serialization): the block ID first, and then its hash key (if any).  */
  /* FIXME:  Should use the serialization method.  */
  offset = (block->key_size * block->key_count) + KEY_BLOCK_HEADER_SIZE;
  assert (chop_chk_index_handle_size (index) == block->key_size);

  memcpy (&block->keys[offset],
	  index->block_id,
	  index->block_id_size);
  offset += index->block_id_size;
  if (index->ciphered)
    {
      memcpy (&block->keys[offset],
	      index->hash_key,
	      index->hash_key_size);
      offset += index->hash_key_size;
    }
  {
    uint32_t net_block_size = htonl (index->block_size);
    memcpy (&block->keys[offset], &net_block_size, 4);
  }

  block->key_count++;

  return 0;
}

/* Append INDEX to TREE.  */
static errcode_t
chop_block_tree_add_index (key_block_tree_t *tree,
			   const chop_chk_index_handle_t *index)
{
  errcode_t err = 0;
  key_block_t *current;

  if (!tree->current)
    {
      /* Allocate a new block tree */
      err = chop_key_block_new (tree->keys_per_block,
				tree->cipher_handle,
				tree->hash_key_hash_method,
				tree->block_id_hash_method,
				tree->key_size,
				tree->log, &tree->current);
      if (err)
	return err;

      tree->current->depth = 0;
    }

  current = tree->current;

  err = chop_key_block_add_index (current, tree->keys_per_block,
				  tree->metadata_store, index);

  return err;
}


/* Flush all the pending key blocks of TREE and return the key of the
   top-level key block in ROOT_INDEX.  */
static errcode_t
chop_block_tree_flush (key_block_tree_t *tree,
		       chop_chk_index_handle_t *root_index)
{
  errcode_t err = 0;
  key_block_t *block;
  size_t depth = 0, last_depth = 0;

  for (block = tree->current;
       block != NULL;
       block = block->parent)
    {
      depth++;
      last_depth = block->depth;
      err = chop_key_block_flush (block, tree->metadata_store, root_index);
      if (err)
	break;

      if (block->parent)
	/* Add the key of the newly flushed block to its parent */
	err = chop_key_block_add_index (block->parent, tree->keys_per_block,
					tree->metadata_store, root_index);
    }

  if (!err)
    chop_log_printf (tree->log,
		     "block_tree_flush: hash tree depth: %u\n", depth);
  else
    chop_log_printf (tree->log,
		     "block_tree_flush: failed: %s\n",
		     error_message (err));

  if (depth)
    assert (last_depth == depth - 1);

  return err;
}

/* Free the memory associated with TREE.  */
static void
chop_block_tree_free (key_block_tree_t *tree)
{
  key_block_t *block;

  for (block = tree->current;
       block != NULL;
       block = block->parent)
    {
      /* FIXME:  Do something!  */
    }
}


/* Implementation of the indexer interface.  */


static errcode_t
chop_hash_tree_index_blocks (chop_indexer_t *indexer,
			     chop_chopper_t *input,
			     chop_block_store_t *output,
			     chop_block_store_t *metadata,
			     chop_index_handle_t *handle);

static errcode_t
chop_hash_tree_fetch_stream (struct chop_indexer *,
			     const chop_index_handle_t *,
			     chop_block_store_t *,
			     chop_block_store_t *,
			     chop_stream_t *);

static errcode_t hash_tree_stream_read (chop_stream_t *, char *,
					size_t, size_t *);

static void hash_tree_stream_close (chop_stream_t *);

extern const chop_class_t chop_hash_tree_stream_class;


errcode_t
chop_hash_tree_indexer_open (chop_hash_method_t content_hash_method,
			     chop_hash_method_t key_hash_method,
			     chop_cipher_handle_t cipher_handle,
			     size_t keys_per_block,
			     chop_indexer_t *indexer)
{
  errcode_t err;
  chop_hash_tree_indexer_t *htree;

  if (key_hash_method == CHOP_HASH_NONE)
    return CHOP_INVALID_ARG;

  if ((cipher_handle != NULL) && (content_hash_method == CHOP_HASH_NONE))
    return CHOP_INVALID_ARG;


  chop_object_initialize ((chop_object_t *)indexer,
			  &chop_hash_tree_indexer_class);

  htree = (chop_hash_tree_indexer_t *)indexer;
  err = chop_log_init ("hash-tree", &htree->log);
  if (err)
    return err;

  htree->indexer.index_blocks = chop_hash_tree_index_blocks;
  htree->indexer.fetch_stream = chop_hash_tree_fetch_stream;
  htree->indexer.stream_class = &chop_hash_tree_stream_class;
  htree->indexer.index_handle_class = &chop_chk_index_handle_class;

  htree->block_id_hash_method = key_hash_method;
  htree->hash_key_hash_method = content_hash_method;

  htree->key_size = chop_hash_size (key_hash_method);
  if (cipher_handle != NULL)
    {
      /* Note: we'll always provide a ciphering key with size equal to that
	 required by the ciphering algo, independently of the size of hashes
	 produced by KEY_HASH_METHOD (see `cipher_make_suitable_key ()').  */
      chop_cipher_algo_t algo = chop_cipher_algorithm (cipher_handle);
      htree->key_size += chop_cipher_algo_key_size (algo);
    }
  htree->key_size += 4; /* the block size is encoded on 4 bytes */

  htree->keys_per_block = keys_per_block;
  htree->cipher_handle = cipher_handle;

#if 0
#define obstack_chunk_alloc malloc
#define obstack_chunk_free  free
  obstack_alloc_failed_handler = &chop_obstack_alloc_failed_handler;
  obstack_init (&htree->key_obstack);
#endif

  return 0;
}

chop_log_t *
chop_hash_tree_indexer_log (chop_indexer_t *indexer)
{
  chop_hash_tree_indexer_t *htree;

  /* Gratuitous overhead.  */
  if (!chop_object_is_a ((chop_object_t *)indexer,
			 &chop_hash_tree_indexer_class))
    return NULL;

  htree = (chop_hash_tree_indexer_t *)indexer;

  return (&htree->log);
}


static errcode_t
chop_hash_tree_index_blocks (chop_indexer_t *indexer,
			     chop_chopper_t *input,
			     chop_block_store_t *output,
			     chop_block_store_t *metadata,
			     chop_index_handle_t *handle)
{
  errcode_t err = 0;
  chop_hash_tree_indexer_t *htree = (chop_hash_tree_indexer_t *)indexer;
  chop_chk_index_handle_t *index = (chop_chk_index_handle_t *)handle;
  size_t amount;
  chop_buffer_t buffer;
  key_block_tree_t tree;
  chop_block_key_t key;
  char *key_buf = (char *)alloca (htree->key_size);

  chop_block_tree_init (&tree, htree->keys_per_block,
			htree->cipher_handle,
			htree->hash_key_hash_method,
			htree->block_id_hash_method,
			htree->key_size,
			metadata, &htree->log);
  chop_block_key_init (&key, key_buf, htree->key_size, NULL, NULL);

  err = chop_buffer_init (&buffer,
			  chop_chopper_typical_block_size (input));
  if (err)
    return err;

  /* Initialize INDEX.  */
  chop_object_initialize ((chop_object_t *)index,
			  &chop_chk_index_handle_class);
  index->ciphered = (htree->cipher_handle == NULL) ? 0 : 1;
  index->block_id_hash_method = htree->block_id_hash_method;
  index->hash_key_hash_method = htree->hash_key_hash_method;

  /* Read blocks from INPUT until the underlying stream returns
     CHOP_STREAM_END.  Keep a copy of each block key.  */
  while (1)
    {
      chop_buffer_clear (&buffer);
      err = chop_chopper_read_block (input, &buffer, &amount);
      if (err)
	break;

      if (!amount)
	continue;

      /* Store this block and get its index */
      err = chop_hash_tree_index_block (htree->cipher_handle,
					htree->hash_key_hash_method,
					htree->block_id_hash_method,
					output,
					chop_buffer_content (&buffer),
					chop_buffer_size (&buffer),
					index);
      if (err)
	break;

      /* Add this block key to our block key tree */
      chop_block_tree_add_index (&tree, index);
    }

  if (err == CHOP_STREAM_END)
    /* Flush the key block tree and get its key.  Here, we get the
       top-level index handle.  */
    err = chop_block_tree_flush (&tree, index);

  /* Free memory associated with TREE */
  chop_block_tree_free (&tree);

  return err;
}



/* Hash tree stream implementation (for retrieval).  */


typedef struct decoded_block
{
  /* The raw buffer and its size */
  chop_buffer_t buffer;

  /* Offset within this raw buffer */
  size_t offset;

  /* Non-zero if this represents a key block (aka. "inode") */
  int is_key_block;

  /* If this is a key block, this represents its height within the tree,
     starting from the lowest key blocks (data blocks being bottommost).  */
  size_t depth;

  /* If this is a key block, this is the total number of children it has.  */
  size_t key_count;

  /* If this is a key block, this points to its current child.  We don't keep
     all blocks in memory.  */
  struct decoded_block *current_child;

  /* If this is a key block, this is the number of CURRENT_CHILD, i.e. an
     integer between zero and KEY_COUNT.  */
  size_t current_child_number;

  /* The parent block */
  struct decoded_block *parent;

  /* If this is a data block, this represents its offset within the stream
     being decoded.  */
  size_t stream_offset;

  /* Pointer to the stream's log */
  chop_log_t *log;
} decoded_block_t;

typedef struct ciphering_context
{
  /* May be CHOP_CIPHER_HANDLE_NIL, in which case no decryption will take
     place and the `hash_key*' fields will simply be ignored.  */
  chop_cipher_handle_t cipher_handle;

  chop_hash_method_t block_id_hash_method;
  chop_hash_method_t hash_key_hash_method;
  size_t block_id_size;
  size_t hash_key_size;

  size_t index_size;  /* The total index size */
} ciphering_context_t;

typedef struct decoded_block_tree
{
  chop_block_store_t *data_store;
  chop_block_store_t *metadata_store;
  chop_chk_index_handle_t *index;

  ciphering_context_t ciphering_context;

  decoded_block_t *top_level;
  size_t current_offset;
  chop_log_t *log;
} decoded_block_tree_t;




/* Hash tree stream objects are returned by CHOP_INDEXER_FETCH_STREAM when
   reading from a hash-tree-indexed stream.  */
CHOP_DECLARE_RT_CLASS (hash_tree_stream, stream,
		       decoded_block_tree_t tree;
		       chop_log_t log;);


/* Initialize INDEX as an instance of CHOP_CHK_INDEX_HANDLE_CLASS, based on
   the information in CONTEXT.  */
static inline void
chk_index_initialize_from_context (chop_chk_index_handle_t *index,
				   const ciphering_context_t *context)
{
  chop_object_initialize ((chop_object_t *)index,
			  &chop_chk_index_handle_class);

  index->ciphered = (context->cipher_handle) ? 1 : 0;
  index->block_id_hash_method = context->block_id_hash_method;
  index->hash_key_hash_method = context->hash_key_hash_method;
  index->block_id_size = context->block_id_size;
  index->hash_key_size = context->hash_key_size;
}

static inline void
chop_decoded_block_tree_init (decoded_block_tree_t *tree,
			      chop_block_store_t *data_store,
			      chop_block_store_t *metadata_store,
			      const chop_index_handle_t *handle,
			      chop_cipher_handle_t cipher_handle,
			      chop_hash_method_t block_id_hash_method,
			      chop_hash_method_t hash_key_hash_method,
			      chop_log_t *log)
{
  const chop_class_t *handle_class =
    chop_object_get_class ((chop_object_t *)handle);

  tree->data_store = data_store;
  tree->metadata_store = metadata_store;
  tree->index = malloc (chop_class_instance_size (handle_class));
  assert (tree->index);  /* XXX: This is bad */
  memcpy (tree->index, handle, chop_class_instance_size (handle_class));
  tree->top_level = NULL;
  tree->current_offset = 0;

  tree->ciphering_context.cipher_handle = cipher_handle;
  tree->ciphering_context.block_id_hash_method = block_id_hash_method;
  tree->ciphering_context.hash_key_hash_method = hash_key_hash_method;
  tree->ciphering_context.block_id_size =
    chop_hash_size (block_id_hash_method);

  if (cipher_handle)
    {
      /* For hash keys, we use exactly the key size requested by the
	 ciphering algorithm (see `cipher_make_suitable_key ()').  */
      chop_cipher_algo_t cipher_algo;

      cipher_algo = chop_cipher_algorithm (cipher_handle);
      tree->ciphering_context.hash_key_size =
	chop_cipher_algo_key_size (cipher_algo);
    }
  else
    tree->ciphering_context.hash_key_size = 0;

  tree->ciphering_context.index_size =
    chop_hash_size (block_id_hash_method);
  if (cipher_handle)
    tree->ciphering_context.index_size +=
      tree->ciphering_context.hash_key_size;

  tree->log = log;
}

/* Constructor.  */
static void
hash_tree_stream_init (chop_object_t *object, const chop_class_t *class)
{
  chop_hash_tree_stream_t *stream = (chop_hash_tree_stream_t *)object;

  stream->stream.read = hash_tree_stream_read;
  stream->stream.close = hash_tree_stream_close;

  stream->tree.index = NULL;

  chop_log_init ("hash-tree-stream", &stream->log);
}


CHOP_DEFINE_RT_CLASS (hash_tree_stream, object,
		      hash_tree_stream_init, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);


/* The entry point.  On success, return zero and set OUTPUT to a stream
   representing the contents of the hash-tree-encoded stream pointed to by
   HANDLE.  */
static errcode_t
chop_hash_tree_fetch_stream (struct chop_indexer *indexer,
			     const chop_index_handle_t *handle,
			     chop_block_store_t *input,
			     chop_block_store_t *metadata,
			     chop_stream_t *output)
{
  errcode_t err;
  chop_hash_tree_stream_t *tstream;
  const chop_class_t *handle_class;
  chop_index_handle_t *handle_copy;
  chop_hash_tree_indexer_t *htree = (chop_hash_tree_indexer_t *)indexer;

  /* Make sure HANDLE is something familiar.  */
  if (!chop_object_is_a ((chop_object_t *)handle,
			 &chop_chk_index_handle_class))
    return CHOP_INVALID_ARG;

  /* OUTPUT is assumed to be as large as `chop_hash_tree_stream_t' */
  chop_object_initialize ((chop_object_t *)output,
			  &chop_hash_tree_stream_class);
  tstream = (chop_hash_tree_stream_t *)output;

  /* Copy HANDLE */
  handle_class = chop_object_get_class ((chop_object_t *)handle);
  handle_copy = malloc (chop_class_instance_size (handle_class));
  if (!handle_copy)
    return ENOMEM;
  memcpy (handle_copy, handle, chop_class_instance_size (handle_class));

  chop_decoded_block_tree_init (&tstream->tree, input, metadata,
				handle,
				htree->cipher_handle,
				htree->block_id_hash_method,
				htree->hash_key_hash_method,
				&htree->log);

  /* Initialize the log (for debugging purposes) */
  err = chop_log_init ("hash-tree-stream", &tstream->log);
  if (err)
    return err;

  chop_log_mimic (&tstream->log, &htree->log, 0);

  chop_log_printf (&htree->log, "fetching stream");

  return err;
}



/* Decode BLOCK's header (which was created by CHOP_KEY_BLOCK_FILL_HEADER).
   BLOCK is assumed to be a key block.  This functions sets BLOCK's KEY_COUNT
   and DEPTH fields.  */
static inline errcode_t
chop_decoded_block_decode_header (decoded_block_t *block)
{
  char magic[2];
  const unsigned char *buffer = chop_buffer_content (&block->buffer);
  assert (block->is_key_block);

  magic[0] = *(buffer++);
  magic[1] = *(buffer++);

  if ((magic[0] != KEY_BLOCK_MAGIC_1) || (magic[1] != KEY_BLOCK_MAGIC_2))
    {
      chop_log_printf (block->log,
		       "invalid key block magic numbers: 0x%02x%02x",
		       magic[0], magic[1]);
      block->current_child_number = block->key_count = block->depth = 0;
      return CHOP_INDEXER_ERROR;
    }
  else
    {
      block->key_count  = ((size_t)*(buffer++));
      block->key_count |= ((size_t)*(buffer++)) <<  8;
      block->key_count |= ((size_t)*(buffer++)) << 16;
      block->key_count |= ((size_t)*(buffer++)) << 24;

      block->depth  = ((size_t)*(buffer++));
      block->depth |= ((size_t)*(buffer++)) << 8;
    }

  block->offset = KEY_BLOCK_HEADER_SIZE;
  block->current_child_number = 0;

  chop_log_printf (block->log, "decoded block: keys=%u, depth=%u",
		   block->key_count, block->depth);

  return 0;
}

static inline errcode_t
chop_decoded_block_new (decoded_block_t **block, chop_log_t *log)
{
  errcode_t err;

  *block = malloc (sizeof (decoded_block_t));
  if (!*block)
    return ENOMEM;

  (*block)->current_child = (*block)->parent = NULL;
  (*block)->current_child_number = 0;

  err = chop_buffer_init (&(*block)->buffer, 0);
  if (err)
    return err;

  (*block)->log = log;

  return 0;
}

/* Return non-zero if children of BLOCK are key blocks.  */
#define CHILDREN_ARE_KEY_BLOCKS(_block) \
  (((_block)->is_key_block && ((_block)->depth > 0)) ? 1 : 0)

/* Fetch block with index INDEX from STORE and update BLOCK accordingly.
   PARENT, if not NULL, is assumed to be the parent of the block being
   fetched.  */
static errcode_t
chop_decoded_block_fetch (chop_block_store_t *store,
			  const chop_chk_index_handle_t *index,
			  const ciphering_context_t *ciphering_context,
			  decoded_block_t *parent, decoded_block_t *block)
{
  errcode_t err;
  size_t block_size;
  int is_key_block;
  chop_block_key_t key;

  if (parent)
    is_key_block = CHILDREN_ARE_KEY_BLOCKS (parent);
  else
    is_key_block = 1;

  chop_block_key_init (&key, index->block_id, index->block_id_size,
		       NULL, NULL);

  err = chop_store_read_block (store, &key, &block->buffer, &block_size);
  if ((err) && (err != ENOMEM))
    {
      chop_buffer_return (&block->buffer);
      return err;
    }

  /* Make sure we got at least as much data as requested (ciphering sometimes
     requires padding so we may get more than needed).  */
  if (block_size < index->block_size)
    {
      char *hex_id = alloca (index->block_id_size * 2 + 1);
      chop_buffer_to_hex_string (index->block_id, index->block_id_size,
				 hex_id);
      chop_log_printf (block->log, "block_fetch: %s: got %u bytes instead "
		       "of %u", hex_id, block_size, index->block_size);
      return CHOP_INDEXER_ERROR;
    }

  if (index->ciphered)
    {
      /* Decrypt BLOCK's content using its hash key and the specified
	 ciphering algorithm.  */
      chop_cipher_handle_t cipher_handle;
      chop_cipher_algo_t cipher_algo;

#if 0
      {
	char *hash_key_hex = alloca (index->hash_key_size * 2 + 1);

	chop_buffer_to_hex_string (index->hash_key, index->hash_key_size,
				   hash_key_hex);
	chop_log_printf (block->log, "block: using hash key %s",
			 hash_key_hex);
      }
#endif

      cipher_handle = ciphering_context->cipher_handle;
      if (cipher_handle == CHOP_CIPHER_HANDLE_NIL)
	{
	  chop_log_printf (block->log, "block_fetch: "
			   "index handle refers to a ciphered block but "
			   "no cipher handle was passed");
	  return CHOP_INDEXER_ERROR;
	}
      cipher_algo = chop_cipher_algorithm (cipher_handle);

      /* We round hash keys so that their size is equal to the key size
	 requested by the algorithm (see `cipher_make_suitable_key ()').
	 Therefore, at this point, this should still be true.  */
      if (chop_cipher_algo_key_size (cipher_algo) != index->hash_key_size)
	{
	  chop_log_printf (block->log, "block_fetch: index' hash key size "
			   "is %u instead of %u", index->hash_key_size,
			   chop_cipher_algo_key_size (cipher_algo));
	  return CHOP_INDEXER_ERROR;
	}

      /* Provide exactly the right key size.  */
      err = chop_cipher_set_key (cipher_handle,
				 index->hash_key,
				 chop_cipher_algo_key_size (cipher_algo));
#if 0
      if (!err)
	err = chop_cipher_set_iv (cipher_handle, zero_vector,
				  chop_cipher_algo_block_size
				  (chop_cipher_algorithm (cipher_handle)));
#endif

      if (!err)
	{
	  char *cleartext = alloca (block_size);

	  err = chop_cipher_decrypt (cipher_handle,
				     cleartext, block_size,
				     chop_buffer_content (&block->buffer),
				     block_size);
	  if (!err)
	    /* Push only INDEX->BLOCK_SIZE bytes (the payload).  */
	    err = chop_buffer_push (&block->buffer, cleartext,
				    index->block_size);
	}

      if (err)
	{
	  char *block_id;
	  block_id = alloca (index->block_id_size * 2 + 1);
	  chop_buffer_to_hex_string (index->block_id, index->block_id_size,
				     block_id);
	  chop_log_printf (block->log, "block `%s': decryption error: %s",
			   block_id, error_message (err));
	  return err;
	}
    }

  block->is_key_block = is_key_block;
  block->offset = 0;
  if (is_key_block)
    err = chop_decoded_block_decode_header (block);

  block->parent = parent;

  /* Keep the CURRENT_CHILD pointer as is in order to be able to reuse
     `decoded_block_t' objects accross calls to
     CHOP_DECODED_BLOCK_NEXT_CHILD.  */
/*   block->current_child = NULL; */

  return err;
}

/* Update BLOCK's CURRENT_CHILD pointer to its next block if any.
   CHOP_STREAM_END is returned if BLOCK and his parents don't have any
   further block.  This propagates the request recursively to BLOCK's
   parents.  However, no new block object is allocated: they are simply
   reused.  */
static errcode_t
chop_decoded_block_next_child (decoded_block_t *block,
			       const ciphering_context_t *ciphering_context,
			       chop_block_store_t *metadata_store,
			       chop_block_store_t *data_store)
{
  errcode_t err;
  chop_log_t *log = block->log;

 start:
  if ((block->current_child_number >= block->key_count)
      || (block->offset >= chop_buffer_size (&block->buffer)))
    {
      /* We're done with this block so let's reuse it with the next block */
      if (!block->parent)
	{
	  /* BLOCK is the top-level key block and there's nothing left in
	     it.  */
	  chop_log_printf (log, "root block: end of stream (offset: %u/%u)",
			   block->offset, chop_buffer_size (&block->buffer));
	  return CHOP_STREAM_END;
	}
      else
	{
	  /* Propagate this request to BLOCK's parent */
	  decoded_block_t *parent = block->parent;
	  err = chop_decoded_block_next_child (block->parent,
					       ciphering_context,
					       metadata_store, data_store);
	  if (err)
	    return err;

	  /* At this point, the contents of BLOCK have changed, i.e. BLOCK
	     has been re-used to represent the new child of its parent.  The
	     CURRENT_CHILD pointer of PARENT should still point to BLOCK and
	     vice-versa.  */
	  assert (parent->current_child == block);
	  assert (block->parent == parent);
	  block = parent->current_child;

	  /* Restart the procedure with the new content of BLOCK.  */
	  goto start;
	}
    }
  else
    {
      /* Read a block index and update BLOCK's CURRENT_CHILD pointer.  */
      size_t index_size;
      chop_block_store_t *the_store;
      chop_chk_index_handle_t index;
      char *pos;

      pos = (char *)chop_buffer_content (&block->buffer) + block->offset;

      /* Sanity check:  do we have a complete index available?  */
      index_size = ciphering_context->index_size;
      assert (block->offset + index_size <= chop_buffer_size (&block->buffer));

      chk_index_initialize_from_context (&index, ciphering_context);

      /* FIXME:  We should use the serialization method.  */
      memcpy (index.block_id, pos,
	      ciphering_context->block_id_size);
      block->offset += ciphering_context->block_id_size;
      pos += ciphering_context->block_id_size;
      if (index.ciphered)
	{
	  /* INDEX points to an encrypted block.  */
	  memcpy (index.hash_key, pos,
		  ciphering_context->hash_key_size);
	  block->offset += ciphering_context->hash_key_size;
	  pos += ciphering_context->hash_key_size;
	}
      {
	uint32_t net_block_size;
	memcpy (&net_block_size, pos, 4);
	index.block_size = ntohl (net_block_size);
	block->offset += 4, pos += 4;
      }

      if (!block->current_child)
	{
	  err = chop_decoded_block_new (&block->current_child, block->log);
	  if (err)
	    return err;
	}

      /* Pick up the right block store */
      if (CHILDREN_ARE_KEY_BLOCKS (block))
	the_store = metadata_store;
      else
	the_store = data_store;

      chop_log_printf (log, "fetching new child block (current depth: %u)",
		       block->is_key_block ? block->depth : 0);
      err = chop_decoded_block_fetch (the_store, &index,
				      ciphering_context,
				      block, block->current_child);
      if (err)
	return err;

      block->current_child_number++;
    }

  return 0;
}


/* Read at most SIZE bytes from BLOCK.  If BLOCK is a key block, pass this
   request to its current child block.  If BLOCK is a data block, read as
   much as possible from it and ask its parents to jump to the next data
   block (without allocating new block objects) if BLOCK has been read
   entirely.  */
static errcode_t
chop_decoded_block_read (decoded_block_t *block,
			 chop_block_store_t *metadata_store,
			 chop_block_store_t *data_store,
			 const ciphering_context_t *ciphering_context,
			 char *buffer, size_t size, size_t *read)
{
  errcode_t err = 0;

  if (block->is_key_block)
    {
      decoded_block_t *child;
      if (!block->current_child)
	{
	  /* Fetch this block's first child */
	  err = chop_decoded_block_next_child (block, ciphering_context,
					       metadata_store, data_store);
	  if (err)
	    return err;

	  block->current_child->current_child = NULL;
	}

      /* Pass this request to BLOCK's current child */
      child = block->current_child;
      err = chop_decoded_block_read (child, metadata_store, data_store,
				     ciphering_context, buffer, size, read);
    }
  else
    {
      /* BLOCK is a data block */
      assert (block->parent);

      *read = 0;
      while (*read < size)
	{
	  const char *block_buf;
	  size_t amount, available, to_read = size - *read;

	  if (block->offset >= chop_buffer_size (&block->buffer))
	    {
	      /* We're donc with this block.  Re-use it for the next block.  */
	      err = chop_decoded_block_next_child (block->parent,
						   ciphering_context,
						   metadata_store, data_store);
	      if (err)
		{
		  if (err == CHOP_STREAM_END)
		    {
		      if (*read == 0)
			/* Only return CHOP_STREAM_END if really nothing was
			   read; otherwise, wait till the next request.  */
			return err;
		    }
		  else
		    return err;
		}
	    }

	  block_buf = chop_buffer_content (&block->buffer) + block->offset;
	  available = chop_buffer_size (&block->buffer) - block->offset;
	  amount = (available > to_read) ? to_read : available;

	  memcpy (buffer + *read, block_buf, amount);
	  block->offset += amount;
	  *read += amount;

	  if (err == CHOP_STREAM_END)
	    {
	      /* We'll return CHOP_STREAM_END next time */
	      err = 0;
	      break;
	    }
	}
    }

  return err;
}

/* Read at most SIZE bytes from TREE's underlying metadata block store.  On
   success, zero is returned and READ is set to the number of bytes actually
   read.  CHOP_STREAM_END is returned on end-of-stream.  */
static errcode_t
chop_decoded_block_tree_read (decoded_block_tree_t *tree,
			      char *buffer, size_t size,
			      size_t *read)
{
  errcode_t err;

  chop_log_printf (tree->log, "reading %u bytes", size);
  if (!tree->top_level)
    {
      /* Fetch the top-level key block (or "inode").  */
      chop_chk_index_handle_t *hhandle;

      err = chop_decoded_block_new (&tree->top_level, tree->log);
      if (err)
	return err;

      if (!chop_object_is_a ((chop_object_t *)tree->index,
			     &chop_chk_index_handle_class))
	return CHOP_INVALID_ARG;

      hhandle = (chop_chk_index_handle_t *)tree->index;

      err = chop_decoded_block_fetch (tree->metadata_store, hhandle,
				      &tree->ciphering_context,
				      NULL /* No parent block */,
				      tree->top_level);
      if (err)
	return err;
    }

  *read = 0;
  err = chop_decoded_block_read (tree->top_level,
				 tree->metadata_store, tree->data_store,
				 &tree->ciphering_context,
				 buffer, size, read);
  tree->current_offset += *read;

  return err;
}

/* Free resources associated with TREE.  */
static void
chop_decoded_block_tree_free (decoded_block_tree_t *tree)
{
  decoded_block_t *block, *next;

  for (block = tree->top_level;
       block != NULL;
       block = next)
    {
      next = block->current_child;
      free (block);
    }

  tree->top_level = NULL;
}


/* The stream `read' method for hash tree streams.  */
static errcode_t
hash_tree_stream_read (chop_stream_t *stream, char *buffer, size_t size,
		       size_t *read)
{
  chop_hash_tree_stream_t *tstream = (chop_hash_tree_stream_t *)stream;

  assert (tstream->tree.index);

  return (chop_decoded_block_tree_read (&tstream->tree, buffer, size, read));
}

/* The stream `close' method for hash tree streams.  */
static void
hash_tree_stream_close (chop_stream_t *stream)
{
  chop_hash_tree_stream_t *tstream = (chop_hash_tree_stream_t *)stream;

  chop_decoded_block_tree_free (&tstream->tree);
  chop_log_close (&tstream->log);
}

