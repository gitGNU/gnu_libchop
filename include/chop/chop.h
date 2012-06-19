/* libchop -- a utility library for distributed storage
   Copyright (C) 2008, 2010, 2012  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

/* Chop! */

#ifndef CHOP_H
#define CHOP_H

#include <chop/errors.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>


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

/* Memory allocators.  The `chop_class_t' argument allows allocators to know
   on behalf of which class memory is being allocated.  */

struct chop_class;

typedef void * (* chop_malloc_t) (size_t, const struct chop_class *);
typedef void * (* chop_realloc_t) (void *, size_t, const struct chop_class *);
typedef void   (* chop_free_t) (void *, const struct chop_class *);


/* Initialize the Chop library.  This function must be called before using
   the library.  */
extern chop_error_t chop_init (void);

/* Initialize the Chop library using the specified memory allocation
   functions.  */
extern chop_error_t chop_init_with_allocator (chop_malloc_t, chop_realloc_t,
					      chop_free_t);

/* Return the error message associated with ERR.  */
extern const char *chop_error_message (chop_error_t err);

/* Display an error message associated with ERR.  */
extern void chop_error (chop_error_t err, const char *format, ...)
#ifdef __GNUC__
  __attribute__ ((format (printf, 2, 3)))
#endif
  ;


/* Internal memory management functions.  */

extern chop_malloc_t   chop_internal_malloc;
extern chop_realloc_t  chop_internal_realloc;
extern chop_free_t     chop_internal_free;

static __inline__ void *
chop_malloc (size_t amount, const struct chop_class *klass)
{
  if (chop_internal_malloc)
    return (chop_internal_malloc (amount, klass));
  else
    return (malloc (amount));
}

static __inline__ void *
chop_calloc (size_t amount, const struct chop_class *klass)
{
  void *ret;

  if (chop_internal_malloc)
    {
      ret = chop_internal_malloc (amount, klass);
      if (ret)
	memset (ret, 0, amount);
    }
  else
    ret = calloc (1, amount);

  return ret;
}

static __inline__ void *
chop_strdup (const char *str, const struct chop_class *klass)
{
  char *ret;

  if (chop_internal_malloc)
    {
      ret = (char *) chop_internal_malloc (strlen (str) + 1, klass);
      if (ret)
	strcpy (ret, str);
    }
  else
    ret = strdup (str);

  return ret;
}

static __inline__ void *
chop_realloc (void *mem, size_t new_amount, const struct chop_class *klass)
{
  if (chop_internal_realloc)
    return (chop_internal_realloc (mem, new_amount, klass));
  else
    return (realloc (mem, new_amount));
}

static __inline__ void
chop_free (void *mem, const struct chop_class *klass)
{
  if (chop_internal_free)
    chop_internal_free (mem, klass);
  else
    free (mem);
}


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

typedef enum chop_proxy_semantics chop_proxy_semantics_t;


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


/* Convert BUFFER which is SIZE byte long into its base32 (RFC 4648)
   representation and store the result in B32.  B32 must be large enough to
   hold the result, including trailing zero.  */
extern void
chop_buffer_to_base32_string (const char *buffer, size_t size, char *b32);


/* Read the SIZE bytes of base32-encoded data pointed to by B32 and fill
   BUFFER with the decoded binary data.  Set *END to point past the last
   character that was successfully read.  Return the size in bytes of binary
   data written to BUFFER.  BUFFER must be large enough to hold the
   result.  B32 may or may not be padded with `='.  */
extern size_t
chop_base32_string_to_buffer (const char *b32, size_t size, char *buffer,
			      const char **end);


_CHOP_END_DECLS

#endif
