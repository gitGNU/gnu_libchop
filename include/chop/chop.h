/* Chop! */

#ifndef __CHOP_H__
#define __CHOP_H__

#include <chop-errors.h>
#include <unistd.h>

typedef struct chop_stream chop_stream_t;
typedef struct chop_block chop_block_t;
typedef struct chop_block_key chop_block_key_t;
typedef enum chop_hash_method chop_hash_method_t;

typedef struct chop_owner chop_owner_t;
typedef struct chop_block_store chop_block_store_t;


/* Initialize the Chop library.  */
extern errcode_t chop_init (void);


/* Miscellaneous helper functions.  */

/* Convert BUFFER which is SIZE byte long into its hexadecimal representation
   and store the result in HEX.  HEX must be at least (SIZE*2 + 1) byte
   long.  */
extern void chop_buffer_to_hex_string (const char *buffer, size_t size,
				       char *hex);



/* JUNK AHEAD */

/* Note: Use `GMemChunk' (slab allocator),
   http://developer.gnome.org/doc/API/glib/glib-memory-chunks.html */

extern const char *chop_block_content (const chop_stream_t *stream,
				       size_t *block_size);

/* Use either GNet or Gcrypt here, `gcry_md_hash_buffer'.  */
/* extern errcode_t chop_hash (void *buffer, size_t size, */
/* 			    enum chop_hash_method method, */
/* 			    chop_hash_t hash); */

extern errcode_t chop_block_owner (const chop_block_t *block,
				   chop_owner_t *owner);

extern size_t chop_block_ref_count (const chop_block_t *block);

extern void chop_block_ref (chop_block_t *block);

extern void chop_block_unref (chop_block_t *block);

#endif
