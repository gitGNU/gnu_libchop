
#ifndef __CHOP_STORES_H__
#define __CHOP_STORES_H__

/* Block stores.  */

#include <chop/chop.h>
#include <chop/buffers.h>


struct chop_block_store
{
  char *name;

  errcode_t (* read_block) (struct chop_block_store *,
			    const chop_block_key_t *,
			    chop_buffer_t *, size_t *);
  errcode_t (* write_block) (struct chop_block_store *,
			     const chop_block_key_t *,
			     const char *, size_t);
  errcode_t (* close) (struct chop_block_store *);
  errcode_t (* sync) (struct chop_block_store *);
};

struct chop_block_key
{
  char  *key;
  size_t size;
  void (* dispose) (char *, void *);
  void *owner;
};

typedef struct chop_gdbm_block_store chop_gdbm_block_store_t;
typedef chop_block_store_t chop_dummy_block_store_t;



/* Block keys.  */

static __inline__ void chop_block_key_init (chop_block_key_t *__key,
					    char *__key_contents,
					    size_t __key_size,
					    void (* __dispose_func) (char *,
								     void *),
					    void *__user_data)
{
  __key->key = __key_contents;
  __key->size = __key_size;
  __key->dispose = __dispose_func;
  __key->owner = __user_data;
}

static __inline__ void chop_block_key_free (chop_block_key_t *__key)
{
  if (__key->dispose)
    __key->dispose (__key->key, __key->owner);
  __key->key = NULL;
  __key->size = 0;
  __key->dispose = NULL;
  __key->owner = NULL;
}

static __inline__ size_t chop_block_key_size (const chop_block_key_t *__key)
{
  return (__key->size);
}

static __inline__ const char *
chop_block_key_buffer (const chop_block_key_t *__key)
{
  return (__key->key);
}

/* Convert key KEY into an hexadecimal representation stored in HEX.  HEX has
   to be at least twice as long as KEY's buffer plus one byte.  */
static __inline__ void
chop_block_key_to_hex_string (const chop_block_key_t *__key,
			      char *hex)
{
  chop_buffer_to_hex_string (chop_block_key_buffer (__key),
			     chop_block_key_size (__key),
			     hex);
}


/* Initialize STORE as a "dummy" block store that does nothing but display
   what goes on.  Only useful for debugging purposes.  */
extern void chop_dummy_block_store_open (chop_dummy_block_store_t *store);

/* Open GDBM database file NAME, with mode MODE (same as for open(2) and
   chmod(2)).  If BLOCK_SIZE is lower than 512, the use the filesystem block
   size as the GDBM size for block transferts, otherwise use the value of
   BLOCK_SIZE.  FATAL_FUNC will be called by GDBM whenever a fatal error
   occurs; if NULL, a built-in GDBM function will be called.  On success,
   return zero and initialize the object pointed to by STORE.  */
extern errcode_t chop_gdbm_store_open (const char *name, size_t block_size,
				       int mode,
				       void (* fatal_func) (const char *),
				       chop_gdbm_block_store_t *store);

/* extern errcode_t chop_dht_store_open (pid_t dht); */

static __inline__ errcode_t
chop_store_write_block (chop_block_store_t *__store,
			const chop_block_key_t *__key,
			const char *__block,
			size_t __size)
{
  return (__store->write_block (__store, __key, __block, __size));
}

static __inline__ errcode_t
chop_store_read_block (chop_block_store_t *__store,
		       const chop_block_key_t *__key,
		       chop_buffer_t *__buffer, size_t *__size)
{
  return (__store->read_block (__store, __key, __buffer, __size));
}


/* extern errcode_t chop_store_write_blocks (chop_block_store_t *store, */
/* 					  const chop_block_t *blocks, */
/* 					  enum chop_hash_method method, */
/* 					  size_t block_count, */
/* 					  const chop_block_t * */
/* 					  (* resolve_collision) */
/* 					  (const chop_block_t *old, */
/* 					   const chop_block_t *new), */
/* 					  size_t *written); */

extern errcode_t chop_store_delete_block (chop_block_store_t *store,
					  const chop_block_key_t key);

static __inline__ errcode_t chop_store_sync (chop_block_store_t *__store)
{
  return (__store->sync (__store));
}

static __inline__ errcode_t chop_store_close (chop_block_store_t *__store)
{
  return (__store->close (__store));
}

#endif
