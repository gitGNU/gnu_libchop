#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/store-hash-tree.h>
#include <chop/buffers.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

/* Glibc's obstacks */
#include <obstack.h>

/* libgcrypt */
#include <gcrypt.h>


typedef struct
{
  chop_hash_method_t chop_name;
  int gcrypt_name;
  const char *name;
  size_t size;
} _chop_hash_method_info;

#define _STRINGIFY(_x) #_x
#define STRINGIFY(_z)  _STRINGIFY(_z)

#define _HASH_METHOD_INFO(_name, _size) \
{ CHOP_HASH_ ## _name, GCRY_MD_ ## _name, STRINGIFY (_name), _size }

static _chop_hash_method_info hash_methods[] =
  {
    _HASH_METHOD_INFO (NONE, 0),
    _HASH_METHOD_INFO (SHA1, 20),
    _HASH_METHOD_INFO (RMD160, 60),
    _HASH_METHOD_INFO (MD5, 16),
    _HASH_METHOD_INFO (MD4, 16),
    _HASH_METHOD_INFO (MD2, 0),
    _HASH_METHOD_INFO (TIGER, 24),
    _HASH_METHOD_INFO (HAVAL, 20),
    _HASH_METHOD_INFO (SHA256, 32),
    _HASH_METHOD_INFO (SHA384, 48),
    _HASH_METHOD_INFO (SHA512, 64),
    { 0, 0, 0, }
  };

size_t
chop_hash_size (chop_hash_method_t method)
{
  const _chop_hash_method_info *p;
  for (p = hash_methods; p->name != NULL; p++)
    {
      if (p->chop_name == method)
	break;
    }
  return (p->size);
}

const char *
chop_hash_name (chop_hash_method_t method)
{
  const _chop_hash_method_info *p;
  for (p = hash_methods; p->name != NULL; p++)
    {
      if (p->chop_name == method)
	break;
    }
  return (p->name);
}

int
chop_hash_gcrypt_name (chop_hash_method_t method)
{
  const _chop_hash_method_info *p;
  for (p = hash_methods; p->name != NULL; p++)
    {
      if (p->chop_name == method)
	break;
    }
  return (p->gcrypt_name);
}


static errcode_t
chop_hash_tree_write_block (chop_block_store_t *,
			    const chop_block_key_t *,
			    const char *,
			    size_t);

static errcode_t
chop_hash_tree_read_block (chop_block_store_t *,
			   const chop_block_key_t *,
			   chop_buffer_t *,
			   size_t *);

static errcode_t chop_hash_tree_sync (chop_block_store_t *);

static errcode_t chop_hash_tree_close (chop_block_store_t *);


static void
chop_obstack_alloc_failed_handler (void)
{
  exit (77);
}

errcode_t
chop_hash_tree_block_store_open (chop_block_store_t *backend,
				 chop_hash_method_t hash_method,
				 chop_hash_tree_block_store_t *store)
{
  store->store.read_block = chop_hash_tree_read_block;
  store->store.write_block = chop_hash_tree_write_block;
  store->store.sync = chop_hash_tree_sync;
  store->store.close = chop_hash_tree_close;

  store->backend = backend;
  store->hash_method = hash_method;
  store->gcrypt_hash_method = chop_hash_gcrypt_name (hash_method);
  store->key_size = chop_hash_size (hash_method);
  store->block_count = 0;

#if 0
#define obstack_chunk_alloc malloc
#define obstack_chunk_free  free
  obstack_alloc_failed_handler = &chop_obstack_alloc_failed_handler;
  obstack_init (&store->key_obstack);
#endif

  chop_buffer_init (&store->block_keys, 32);

  return 0;
}

static void
dispose_key_copy (char *key_content, void *unused)
{
  free (key_content);
}

static errcode_t
chop_hash_tree_write_block (chop_block_store_t *store,
			    const chop_block_key_t *key,
			    const char *buffer,
			    size_t size)
{
  errcode_t err;
  chop_hash_tree_block_store_t *htree =
    (chop_hash_tree_block_store_t *)store;

  /* Copy KEY and store it in our block key vector STORE->BLOCK_KEYS */
  chop_block_key_t *key_copy;
  char *hash = calloc (1, chop_block_key_size (key));
  key_copy = calloc (1, sizeof (chop_block_key_t));
  if ((!hash) || (!key_copy))
    return ENOMEM;

  memcpy (hash, chop_block_key_buffer (key), chop_block_key_size (key));
  chop_block_key_init (key_copy, hash,
		       chop_block_key_size (key),
		       &dispose_key_copy, NULL);

#if 0
  gcry_md_hash_buffer (store->gcrypt_hash_method, hash,
		       buffer, size);
#endif

  /* Add this block key to the key vector */
  htree->block_count++;
  err = chop_buffer_append (&htree->block_keys,
			    (const char *)&key_copy,
			    sizeof (chop_block_key_t *));

  if ((!err) && (htree->backend))
    /* Store this block in the underlying block store */
    err = chop_store_write_block (htree->backend, key, buffer, size);

  return err;
}

static errcode_t
chop_hash_tree_read_block (chop_block_store_t *store,
			   const chop_block_key_t *key,
			   chop_buffer_t *buffer, size_t *size)
{
  return CHOP_ERR_NOT_IMPL;
}

/* Convert BUFFER which is SIZE byte long into its hexadecimal representation
   and store the result in HEX.  HEX must be at least (SIZE*2 + 1) byte
   long.  */
static void
buffer_to_hex_string (const char *buffer, size_t size, char *hex)
{
  const char *p, *end = buffer + size;
  for (p = buffer; p < end; p++)
    {
      sprintf (hex, "%02x", *p);
      hex += 2;
    }

  *hex = '\0';
}

static __inline__ void
show_hash_tree (chop_hash_tree_block_store_t *htree)
{
  chop_block_key_t **block_keys;
  char hex[1024];
  unsigned block_num;

  block_keys = (chop_block_key_t **)chop_buffer_content (&htree->block_keys);
  printf ("store-hash-tree:  stored %u blocks\n",
	  htree->block_count);
  for (block_num = 0; block_num < htree->block_count; block_num++)
    {
      chop_block_key_t *key = block_keys[block_num];
      buffer_to_hex_string (chop_block_key_buffer (key),
			    chop_block_key_size (key), hex);
      printf ("block 0x%04x: %s\n", block_num, hex);
    }
}

static errcode_t
chop_hash_tree_sync (chop_block_store_t *store)
{
  chop_hash_tree_block_store_t *htree =
    (chop_hash_tree_block_store_t *)store;

  if (!htree->backend)
    /* Show debugging output */
    show_hash_tree (htree);
  else
    return (chop_store_sync (htree->backend));

  return 0;
}

static inline void
hash_tree_free (chop_hash_tree_block_store_t *htree)
{
  unsigned block_num;
  chop_block_key_t **block_keys =
    (chop_block_key_t **)chop_buffer_content (&htree->block_keys);

  for (block_num = 0; block_num < htree->block_count; block_num++)
    {
      chop_block_key_free (block_keys[block_num]);
      free (block_keys[block_num]);
    }

  chop_buffer_return (&htree->block_keys);
}

static errcode_t
chop_hash_tree_close (chop_block_store_t *store)
{
  errcode_t err;

  err = chop_hash_tree_sync (store);
  if (err)
    return err;

  hash_tree_free ((chop_hash_tree_block_store_t *)store);

  return 0;
}
