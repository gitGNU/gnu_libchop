/* Chop! */

#ifndef __CHOP_H__
#define __CHOP_H__

#include <chop/chop-errors.h>
#include <unistd.h>


/* Helper macros.  */

#ifdef __GNUC__

# if (__GNUC__ >= 3)
#  define _CHOP_PURE_FUNC  __attribute__ ((__pure__))
# endif

#else

# define _CHOP_PURE_FUNC

#endif

#ifdef __cplusplus
# define _CHOP_BEGIN_DECLS  extern "C" {
# define _CHOP_END_DECLS    }
#else
# define _CHOP_BEGIN_DECLS
# define _CHOP_END_DECLS
#endif


_CHOP_BEGIN_DECLS

/* Basic types.  */
typedef struct chop_block_key chop_block_key_t;
typedef enum chop_hash_method chop_hash_method_t;


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
   zero-terminated or not.  Return in END the position where reading
   stopped.  */
extern void chop_hex_string_to_buffer (const char *hex, size_t size,
				       char *buf, const char **end);


_CHOP_END_DECLS

#endif
