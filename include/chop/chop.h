/* Chop! */

#ifndef __CHOP_H__
#define __CHOP_H__

#include <chop/chop-errors.h>
#include <unistd.h>

typedef struct chop_stream chop_stream_t;
typedef struct chop_block chop_block_t;
typedef struct chop_block_key chop_block_key_t;
typedef enum chop_hash_method chop_hash_method_t;

typedef struct chop_owner chop_owner_t;
typedef struct chop_block_store chop_block_store_t;


/* Initialize the Chop library.  This function must be called before using
   the library.  */
extern errcode_t chop_init (void);


/* Miscellaneous helper functions.  */

/* Convert BUFFER which is SIZE byte long into its hexadecimal representation
   and store the result in HEX.  HEX must be at least (SIZE*2 + 1) byte
   long.  */
extern void chop_buffer_to_hex_string (const char *buffer, size_t size,
				       char *hex);


#endif
