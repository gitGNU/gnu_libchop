
#ifndef __CHOP_STORES_H__
#define __CHOP_STORES_H__

/* Block stores.  */

#include <chop/chop.h>
#include <chop/buffers.h>
#include <chop/serializable.h>
#include <chop/logs.h>

/* Declare `chop_block_store_t' (represented at run-time by
   CHOP_BLOCK_STORE_CLASS) as inheriting from `chop_object_t'.  */
CHOP_DECLARE_RT_CLASS (block_store, object,
		       char *name;

		       errcode_t (* read_block) (struct chop_block_store *,
						 const chop_block_key_t *,
						 chop_buffer_t *, size_t *);
		       errcode_t (* write_block) (struct chop_block_store *,
						  const chop_block_key_t *,
						  const char *, size_t);
		       errcode_t (* close) (struct chop_block_store *);
		       errcode_t (* sync) (struct chop_block_store *););




/* Block keys.  */

struct chop_block_key
{
  char  *key;
  size_t size;
  void (* dispose) (char *, void *);
  void *owner;
};


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


/* Implementations of the block store interface.  */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* The class of all the file-based block store classes (i.e. GDBM, TDB,
   etc.).  Provides a generic method for opening such a store that is called
   by `chop_file_based_store_open ()'.  This declares
   CHOP_FILE_BASED_STORE_CLASS_CLASS and `chop_file_based_store_class_t'.  */
CHOP_DECLARE_RT_CLASS (file_based_store_class, class,
		       errcode_t (* generic_open) (const chop_class_t *class,
						   const char *file,
						   int open_flags,
						   mode_t mode,
						   chop_block_store_t *store););


extern const chop_class_t chop_dummy_block_store_class;
extern const chop_file_based_store_class_t chop_gdbm_block_store_class;
extern const chop_file_based_store_class_t chop_tdb_block_store_class;
extern const chop_class_t chop_remote_block_store_class;


/* Initialize STORE as a "dummy" block store that does nothing but display
   what goes on.  Only useful for debugging purposes.  STORE must point to
   enough memory to store an instance of type
   CHOP_DUMMY_BLOCK_STORE_CLASS.  */
extern void chop_dummy_block_store_open (const char *name,
					 chop_block_store_t *store);

/* Open a "dummy" block store that acts as a proxy of BACKEND.  This allows
   to display accesses to BACKEND.  All operations but CHOP_STORE_CLOSE are
   forwarded to BACKEND.  */
extern void
chop_dummy_proxy_block_store_open (const char *name,
				   chop_block_store_t *backend,
				   chop_block_store_t *store);

/* Return the log of STORE.  This may be used to change the way the dummy
   block store logs its data (by default, its log is attached to the standard
   error output).  */
extern chop_log_t *chop_dummy_block_store_log (chop_block_store_t *store);

/* Open GDBM database file NAME, with mode MODE (same as for open(2) and
   chmod(2)). OPEN_FLAGS are flags as those passed to open(2).  If BLOCK_SIZE
   is lower than 512, the use the filesystem block size as the GDBM size for
   block transferts, otherwise use the value of BLOCK_SIZE.  FATAL_FUNC will
   be called by GDBM whenever a fatal error occurs; if NULL, a built-in GDBM
   function will be called.  On success, return zero and initialize the
   object pointed to by STORE.  STORE must point to enough memory to store an
   instance of type CHOP_GDBM_BLOCK_STORE_CLASS.  */
extern errcode_t chop_gdbm_store_open (const char *name, size_t block_size,
				       int open_flags, mode_t mode,
				       void (* fatal_func) (const char *),
				       chop_block_store_t *store);

/* Open TDB (the Trivial Database) file NAME, etc, etc.  Availability of this
   function depends on whether you had TDB installed at compilation time.
   TDB databases are usually slightly smaller than GDBM ones.  */
extern errcode_t chop_tdb_store_open (const char *name,
				      int hash_size, int tdb_flags,
				      int open_flags, mode_t mode,
				      chop_block_store_t *store);

/* This function is a simple version of the GDBM/TDB block store open
   functions which it just calls.  The first argument gives the pointer to
   one of the database-based block store classes.  */
extern errcode_t
chop_file_based_store_open (const chop_file_based_store_class_t *class,
			    const char *file, int open_flags, mode_t mode,
			    chop_block_store_t *store);

/* Open a remote block store located at HOST with protocol PROTOCOL.
   PROTOCOL may be either "udp" or "tcp".  On success return 0.  */
extern errcode_t
chop_remote_block_store_open (const char *host, const char *protocol,
			      chop_block_store_t *store);

/* XXX:  We might want to have a look at Berkeley DB and the Trivial DB
   (`libdb3' and `libtdb1'), or even the TDB Replication System
   (http://tdbrepl.inodes.org/) or a DHT.  */



/* The block store interface.  */

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
