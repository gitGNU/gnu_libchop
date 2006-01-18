/* Chop! */

#ifndef __CHOP_H__
#define __CHOP_H__

#include <chop/chop-errors.h>
#include <unistd.h>


/* Helper macros.  */

#ifdef __GNUC__

# if (__GNUC__ >= 3)
#  define _CHOP_PURE_FUNC  __attribute__ ((__pure__))
#  define CHOP_EXPECT      __builtin_expect
# else
#  define CHOP_EXPECT(_exp, _r) (_exp)
#  define _CHOP_PURE_FUNC
# endif

# define _CHOP_UNUSED __attribute__ ((__unused__))

#else

# define _CHOP_PURE_FUNC
# define _CHOP_UNUSED
# define CHOP_EXPECT(_exp, _r)  (_exp)

#endif

#define CHOP_EXPECT_TRUE(_exp)  (CHOP_EXPECT (_exp, 1))
#define CHOP_EXPECT_FALSE(_exp) (CHOP_EXPECT (_exp, 0))

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
typedef enum chop_proxy_semantics chop_proxy_semantics_t;


/* Initialize the Chop library.  This function must be called before using
   the library.  */
extern errcode_t chop_init (void);


/* The following type is used to define the relationship between a proxy and
   an object it proxies.  This is used in proxying streams and proxying
   stores, such as filtered streams and filtered stores.  */
enum chop_proxy_semantics
  {
    CHOP_PROXY_LEAVE_AS_IS = 0,    /* Don't close nor destroy the proxied
				      object.  */
    CHOP_PROXY_EVENTUALLY_CLOSE,   /* On `close ()' (or equivalent), close
				      the proxied object but don't destroy
				      it.  Useful for classes that have an
				      explicit `close ()' operation, like
				      streams and stores.  */
    CHOP_PROXY_EVENTUALLY_DESTROY, /* Upon destruction, destroy the proxied
				      object, using
				      `chop_object_destroy ()'.  */
    CHOP_PROXY_EVENTUALLY_FREE     /* Upon destruction, destroy the proxied
				      object and free its storage using the
				      standard `free ()' function.  */
  };



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
