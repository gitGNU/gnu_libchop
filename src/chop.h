/* Chop! */

#include <chop-errors.h>
#include <unistd.h>

typedef struct chop_stream chop_stream_t;
typedef struct chop_block chop_block_t;
typedef void *chop_hash_t;

enum chop_hash_method
  {
    CHOP_HASH_NONE,
    CHOP_HASH_SHA1,
    CHOP_HASH_RMD160,
    CHOP_HASH_MD5,
    CHOP_HASH_MD4,
    CHOP_HASH_MD2,
    CHOP_HASH_TIGER,
    CHOP_HASH_HAVAL,
    CHOP_HASH_SHA256,
    CHOP_HASH_SHA384,
    CHOP_HASH_SHA512
  };

typedef struct chop_owner chop_owner_t;
typedef struct chop_block_store chop_block_store_t;


extern errcode_t chop_init (void);


/* Stream methods */

extern size_t chop_stream_preferred_block_size (const chop_stream_t *stream);

extern errcode_t chop_stream_read (chop_stream_t *stream,
				   char *buffer,
				   size_t size,
				   size_t *read);



/* Note: Use `GMemChunk' (slab allocator),
   http://developer.gnome.org/doc/API/glib/glib-memory-chunks.html */

extern const char *chop_block_content (const chop_stream_t *stream,
				       size_t *block_size);

/* Use either GNet or Gcrypt here, `gcry_md_hash_buffer'.  */
extern errcode_t chop_hash (void *buffer, size_t size,
			      enum chop_hash_method method,
			      chop_hash_t hash);

extern errcode_t chop_block_hash (const chop_block_t *block,
				  enum chop_hash_method method,
				  chop_hash_t hash);

extern size_t chop_hash_size (enum chop_hash_method);

extern errcode_t chop_block_owner (const chop_block_t *block,
				   chop_owner_t *owner);

extern size_t chop_block_ref_count (const chop_block_t *block);

extern void chop_block_ref (chop_block_t *block);

extern void chop_block_unref (chop_block_t *block);
