/* Chop! */

#ifndef __CHOP_H__
#define __CHOP_H__

#include <chop/chop-errors.h>
#include <unistd.h>

typedef struct chop_block chop_block_t;
typedef struct chop_block_key chop_block_key_t;
typedef enum chop_hash_method chop_hash_method_t;

typedef struct chop_owner chop_owner_t;


/* Initialize the Chop library.  This function must be called before using
   the library.  */
extern errcode_t chop_init (void);


/* Miscellaneous helper functions.  */

/* Convert BUFFER which is SIZE byte long into its hexadecimal representation
   and store the result in HEX.  HEX must be at least (SIZE*2 + 1) byte
   long.  */
extern void chop_buffer_to_hex_string (const char *buffer, size_t size,
				       char *hex);

/* Convert HEX, an hexadecimal string representation (of SIZE bytes, SIZE
   being an even number), into its binary representation into BUF.  BUF must
   be at least (SIZE / 2) bytes long.  This function doesn't care if HEX is
   zero-terminated or not.  */
extern void chop_hex_string_to_buffer (const char *hex, size_t size,
				       char *buf);

#endif
