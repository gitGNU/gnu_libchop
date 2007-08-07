/* This file implements the anchor selection mechanism described by Udi
   Manber in [1].  Basically, this algorithm allows to deterministically find
   anchors within a file such that identical blocks among similar files may
   be discovered.  See `find-anchors.scm' in this directory for a quick
   implementation of the algorithm, as a way to get an understanding of what
   it does.  You may also try `chop-show-anchors' in the `utils' directory
   for that purpose.

   This chopper produces blocks of different sizes.

   [1] Udi Manber.  Finding similar files in a large file system.
       In Proceedings of the Usenix Winter 1994 Conference, pages 1--10,
       January, 1994, http://www.cs.arizona.edu/research/reports.html.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/choppers.h>
#include <chop/chop-config.h>

#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <alloca.h>
#include <errno.h>


/* The following should only be defined when debugging things.  */
/* #define AUTO_TEST 1 */


/* A (sort of) Rabin fingerprint.  */
typedef uint32_t fpr_t;

/* A "sliding window": contains two buffers of WINDOW_SIZE bytes and (almost)
   automatically manages transition from one to the other.  This is some sort
   of a double buffering scheme.  */
typedef struct
{
  size_t window_size;        /* size of the sliding window */
  size_t raw_size;           /* 2 * WINDOW_SIZE */
  char *raw_window;          /* pointer to RAW_SIZE bytes pointed to by the
				two subwindows */
  char *windows[2];          /* two subwindows, each of which points
				to WINDOW_SIZE bytes */
  size_t offset;             /* start offset within the sliding window */
  size_t sizes[2];           /* size of each of the subwindows */
} sliding_window_t;


#ifdef HAVE_LIGHTNING_H

#include <lightning.h>


#ifdef __GNUC__
/* GCC has an extensions for ``labels as pointers'' and the other way.  We
   can leverage this to inline the dynamically-generated code.  Beware:  this
   is slower than the no-inlining version.  */
/* #define INLINE_LIGHTNING_CODE 1 */
#endif

/* The type of function we want to compile.  */
typedef unsigned long (* jit_multiplier_func_t) (unsigned long);

#ifdef INLINE_LIGHTNING_CODE
#warning "You're compiling the inlining stuff, great!"

/* In order to inline code, we'll need to generate it once and then patch it
   every time before we use it.  Lightning's patch operations are not
   costly.  */
typedef struct
{
  jit_multiplier_func_t func;
  jit_insn *operand_ptr_movi;
  jit_insn *final_jump;
  int patched;
} jit_multiplier_t;

#define JIT_MULTIPLIER_INIT(_m)				\
do							\
{							\
  (_m).func = NULL;					\
  (_m).patched = 0;					\
  (_m).operand_ptr_movi = (_m).final_jump = NULL;	\
}							\
while (0)

#define JIT_MULTIPLIER_FUNC(_m)  (_m).func

#else /* INLINE_LIGHTNING_CODE */

/* The type of a dynamically compiled multiplication function.  */
typedef jit_multiplier_func_t jit_multiplier_t;

#define JIT_MULTIPLIER_INIT(_m)  (_m) = NULL;
#define JIT_MULTIPLIER_FUNC(_m)  (_m)

#endif /* INLINE_LIGHTNING_CODE */

#else /* !HAVE_LIGHTNING_H */
# warning "Not compiling the Lightning code"
#endif /* HAVE_LIGHTNING_H */



/* Declare `chop_anchor_based_chopper_t' which inherits from
   `chop_chopper_t'.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (anchor_based_chopper, chopper,
				      chopper_class,

		       /* Sliding widow size */
		       size_t window_size;

		       /* Mask used to determing whether a fingerprint if
			  "magic", i.e. whether this should be made a block
			  boundary */
		       fpr_t magic_fpr_mask;

		       /* The value of ANCHOR_PRIME_NUMBER to the
			  WINDOW_SIZE */
		       fpr_t  prime_to_the_ws;

		       /* The sliding window (double buffer) that is used
			  when reading the input stream.  */
		       sliding_window_t sliding_window;

#ifndef HAVE_LIGHTNING_H
		       /* Cache of multiplication of a byte by PRIME to the
			  WINDOW_SIZE */
		       fpr_t product_cache[256];
#else
		       /* Dynamically generated function that computes an
			  integer multiplied by PRIME_TO_THE_WS */
		       jit_multiplier_t jit_multiply_with_prime_to_the_ws;
#endif

		       /* Tells whether this is the first time we process
			  this stream.  */
		       int first;

		       /* If FIRST is false, fingerprint of the previous
			  sliding window (whose offset was one byte earlier)
			  and first character of the previous sliding
			  window.  */
		       fpr_t prev_fpr;
		       char  prev_first_char;

		       /* Message logging */
		       chop_log_t log;);

/* A generic `open' method that chooses default values.  */
static errcode_t
ab_generic_open (chop_stream_t *input, size_t average_size,
		 chop_chopper_t *chopper)
{
  size_t power_of_two = 1;

  if (average_size == 0)
    power_of_two = 0x1fff; /* the 13 LSBs, i.e. 8KB */
  else
    {
      while ((power_of_two << 1) <= average_size)
	power_of_two <<= 1;

      power_of_two -= 1;
    }

  return (chop_anchor_based_chopper_init (input,
					  48 /* window size */,
					  power_of_two /* magic fpr mask */,
					  chopper));
}

static errcode_t ab_ctor (chop_object_t *, const chop_class_t *);
static void ab_dtor (chop_object_t *);

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (anchor_based_chopper, chopper,
				     chopper_class,  /* Metaclass */

				     /* Metaclass inits */
				     .generic_open = ab_generic_open,

				     ab_ctor, ab_dtor,
				     NULL, NULL, /* No copy, equalp */
				     NULL, NULL  /* No serial/deserial */);


/* These are the main parameters of the algorithm.  Here the `M' parameter
   is chosen to be 2^30 (see ANCHOR_MODULO_MASK) and `p'
   (ANCHOR_PRIME_NUMBER) is 3.  This way, in
   `compute_next_window_fingerprint ()', we can multiply the previous
   fingerprint by ANCHOR_PRIME_NUMBER without risking to overflow the 32-bit
   `fpr_t' type.  */
#define ANCHOR_PRIME_NUMBER (3)
#define ANCHOR_MODULO_MASK  (0x3fffffff)




static errcode_t
chop_anchor_chopper_read_block (chop_chopper_t *, chop_buffer_t *,
				size_t *);

static void
chop_anchor_chopper_close (chop_chopper_t *);



#ifndef HAVE_LIGHTNING_H

/* Multiply WHAT by ANCHOR->PRIME to the ANCHOR->WINDOW_SIZE.  */
static inline fpr_t
multiply_with_prime_to_the_ws (chop_anchor_based_chopper_t *anchor,
			       char what)
{
  fpr_t cached;
  unsigned idx;

  if (what == '\0')
    return 0;

  idx = what;
  cached = anchor->product_cache[idx];
  if (cached == 0)
    {
      /* Not computed yet: compute the result and cache it.  */
      fpr_t result = (fpr_t)what;

      result *= anchor->prime_to_the_ws;
      anchor->product_cache[idx] = result;

      return result;
    }

  return (cached);
}

#else /* HAVE_LIGHTNING_H */

/* XXX:  Maybe we should actually compile the whole
   `compute_next_window_fingerprint ()' function here, so as to avoid the
   function-call overhead for just one multiplication.  */

/* Compile and return a function that multiplies an unsigned long with
   PRIME_TO_THE_WS.  The returned function must be freed eventually.  */
static inline jit_multiplier_t
compile_multiplication_function (unsigned long prime_to_the_ws)
{
#define MAX_CODE_SIZE   1024

#ifndef INLINE_LIGHTNING_CODE
  unsigned long input_arg;
#endif
  char *buffer, *start, *end;
  jit_multiplier_t result;

  JIT_MULTIPLIER_INIT (result);
  buffer = chop_malloc (MAX_CODE_SIZE,
			(chop_class_t *) &chop_anchor_based_chopper_class);
  if (!buffer)
    return result;

  JIT_MULTIPLIER_FUNC (result) =
    (jit_multiplier_func_t)(jit_set_ip ((void *)buffer).iptr);
  start = jit_get_ip ().ptr;

#ifndef INLINE_LIGHTNING_CODE

  /* Take one argument (the character), and instruct Lightning that we won't
     call any function.  */
  jit_leaf (1);
  input_arg = jit_arg_ul ();
  jit_getarg_l (JIT_R2, input_arg);

#else /* INLINE_LIGHTNING_CODE */

  /* Create a zero-argument function.  The first `movi' instructions are
     meant to be patched later, in `compute_next_window_fingerprint ()'.  */
  jit_leaf (0);
  jit_pushr_p (JIT_R1);
  jit_pushr_p (JIT_R2);
  result.operand_ptr_movi = jit_movi_p (JIT_R1, NULL);
  jit_ldr_p (JIT_R2, JIT_R1);

#endif

  /* Actually perform the multiplication:  PRIME_TO_THE_WS is now considered
     a compile-time value.  */
  jit_muli_ul (JIT_R2, JIT_R2, prime_to_the_ws);

#ifndef INLINE_LIGHTNING_CODE

  /* Return the value just computed.  */
  jit_movr_ul (JIT_RET, JIT_R2);
  jit_ret ();

#else

  /* Store the result at the address pointed to by R1.  */
  jit_str_p (JIT_R1, JIT_R2);

  /* Restore the registers and jump out.  */
  jit_popr_p (JIT_R2);
  jit_popr_p (JIT_R1);

  result.final_jump = jit_jmpi (NULL);

#endif

  /* Finish.  */
  end = jit_get_ip ().ptr;
  assert (end - start < MAX_CODE_SIZE);
  jit_flush_code (start, end);

  buffer = chop_realloc (buffer, end - start,
			 (chop_class_t *) &chop_anchor_based_chopper_class);
  return result;
#undef MAX_CODE_SIZE
}

#define multiply_with_prime_to_the_ws(_chopper, _char)			\
  (_chopper)->jit_multiply_with_prime_to_the_ws ((unsigned long)(_char))

#endif /* HAVE_LIGHTNING_H */


/* Read a whole window (ie. ANCHOR->WINDOW_SIZE bytes) from ANCHOR's input
   stream and store it into BUFFER.  */
static inline errcode_t
read_sliding_window (chop_anchor_based_chopper_t *anchor,
		     char *buffer, size_t *size)
{
  errcode_t err = 0;
  chop_stream_t *input = anchor->chopper.stream;

  *size = 0;

  /* Read data from INPUT until we got ANCHOR->WINDOW_SIZE bytes or
     end-of-stream is reached.  */
  while (*size < anchor->window_size)
    {
      size_t amount = 0;

      err = chop_stream_read (input, buffer + *size,
			      anchor->window_size - *size, &amount);
      *size += amount;

      if (err)
	{
	  if (err == CHOP_STREAM_END)
	    break;
	  else
	    return err;
	}
    }

  if (((!err) || (err == CHOP_STREAM_END))
      && (*size < anchor->window_size))
    /* Pad with zeros.  This is meant to help the "fast" implementation of
       sliding windows.  */
    memset (buffer + *size, 0, anchor->window_size - *size);

  if ((err == CHOP_STREAM_END) && (*size > 0))
    /* We'll announce the end of stream once there is really nothing left to
       read.  */
    err = 0;

  return err;
}



/* Sliding windows.  */

/* Return the offset starting from WIN->RAW_WINDOW that corresponds to the
   offset OFFS starting from the beginning of the current sliding window.  */
#define sliding_window_get_raw_offset(_win, _offs)	\
((((_win)->windows[0] - (_win)->raw_window) + (_offs))	\
 % (_win)->raw_size)

/* Return the character located at offset OFFS within WIN.  */
#define sliding_window_ref(_win, _offs)					\
((_win)->raw_window[sliding_window_get_raw_offset (_win, _offs)])

/* Return the character at the current start offset of WINDOW.  */
#define sliding_window_first_char(_win)		\
(sliding_window_ref (_win, (_win)->offset))

/* Return the character at the current end offset (start offset + window
   size) of WINDOW.  */
#define sliding_window_last_char(_win)					\
(sliding_window_ref (_win, (_win)->offset + (_win)->window_size - 1))


/* Forward declarations.  */

#ifdef __GNUC__
# define INLINED  __attribute__ ((__always_inline__))
#else
# define INLINED
#endif

static char * sliding_window_dest_buffer (sliding_window_t *,
					  size_t **,
					  size_t *) INLINED;
static void sliding_window_increment_offset (sliding_window_t *) INLINED;



/* Return non-zero if less than WINDOW->WINDOW_SIZE bytes are available from
   WINDOW's current offset.  */
#define sliding_window_unfull(window)				\
  (((window)->sizes[1] > 0)					\
   ? ((window)->offset > (window)->sizes[1])			\
   : (((window)->sizes[0] > 0)					\
      ? (((window)->offset > 0)					\
	 || ((window)->sizes[0] < (window)->window_size))	\
      : 1))

/* Return a pointer to a WINDOW->WINDOW_SIZE byte buffer.  Return in
   DEST_SIZE a pointer to this buffer's size which should be updated and be
   lower than or equal to WINDOW->WINDOW_SIZE.  DISCARDED is set to the
   amount of useful data that were originally pointed to by *DEST_SIZE.  This
   may be used to flush the data pointed to by *DEST_SIZE before overwriting
   it.  */
static inline char *
sliding_window_dest_buffer (sliding_window_t *window,
			    size_t **dest_size,
			    size_t *discarded)
{
  char *dest;

  if (window->sizes[0] == 0)
    {
      dest = window->windows[0];
      *dest_size = &window->sizes[0];
      *discarded = window->sizes[0];
    }
  else
    {
      if (window->sizes[1] == 0)
	{
	  dest = window->windows[1];
	  *dest_size = &window->sizes[1];
	  *discarded = window->sizes[1];
	}
      else
	{
	  /* Discard the contents of the first window.  Make the second
	     window the first one.  */
	  char *new_window = window->windows[0];

	  *discarded = window->sizes[0];

	  window->windows[0] = window->windows[1];
	  window->windows[1] = new_window;
	  window->sizes[0] = window->sizes[1];
	  window->offset %= window->window_size;

	  dest = window->windows[1];
	  *dest_size = &window->sizes[1];
	}
    }

  return dest;
}


static inline size_t
sliding_window_start_offset (sliding_window_t *window)
{
  register size_t size;

  size = window->sizes[0] + window->sizes[1];

  if (window->offset < size)
    return window->offset;

  return (size ? size - 1 : 0);
}

/* Return the end offset of WINDOW, i.e. and integer between zero and
   two times WINDOW->WINDOW_SIZE.  */
static inline size_t
sliding_window_end_offset (sliding_window_t *window)
{
  register size_t size, end;

  size = window->sizes[0] + window->sizes[1];
  end = window->offset + window->window_size;

  return ((end < size) ? end : size);
}

/* Discard AMOUNT bytes (at most WINDOW->WINDOW_SIZE) from WINDOW.  */
static inline void
sliding_window_skip (sliding_window_t *window, size_t amount)
{
  assert (amount <= window->raw_size - window->offset);

  /* Actually, WINDOW->SIZES[0] should be equal to WINDOW->WINDOW_SIZE most
     of the time.  */
  if (window->offset + amount >= window->sizes[0])
    {
      /* Discard the first subwindow.  */
      char *old_window;

      window->offset += amount;
      window->offset -= window->sizes[0];

      old_window = window->windows[0];
      window->windows[0] = window->windows[1];
      window->windows[1] = old_window;
      window->sizes[0] = window->sizes[1];
      window->sizes[1] = 0;
    }
  else
    window->offset += amount;
}

/* Naively increment WINDOW's offset.  Pre-condition:  WINDOW must not be
   `unfull'.  Whether it becomes unfull afterwards should be checked by the
   caller.  */
static inline void
sliding_window_increment_offset (sliding_window_t *window)
{
  assert (!sliding_window_unfull (window));
  window->offset++;
}

/* Append SIZE bytes starting at START_OFFSET from WINDOW to BUFFER.  */
static inline errcode_t
sliding_window_append_to_buffer (sliding_window_t *window,
				 size_t start_offset, size_t size,
				 chop_buffer_t *buffer)
{
  errcode_t err;

  if (start_offset < window->window_size)
    {
      /* Copy from the first sub-window */
      size_t amount, available = window->sizes[0] - start_offset;
      amount = (available > size) ? size : available;
      err = chop_buffer_append (buffer, window->windows[0] + start_offset,
				amount);
      if (err)
	return err;
      size -= amount;

      if (size > 0)
	{
	  /* Copy the remaining bytes from the second sub-window */
	  available = window->sizes[1];
	  amount = (available > size) ? size : available;

	  err = chop_buffer_append (buffer, window->windows[1], amount);
	  if (err)
	    return err;
	}
    }
  else
    {
      /* Copy from the second sub-window */
      size_t available, amount;

      available = window->sizes[1];
      amount = (available > size) ? size : available;

      start_offset -= window->window_size;
      err = chop_buffer_append (buffer, window->windows[1] + start_offset,
				size);
      if (err)
	return err;
    }

  return 0;
}

/* Copy data available starting from offset WINDOW->WINDOW_SIZE from WINDOW
   to BUFFER.  Set SIZE to the number of bytes copied, at most
   WINDOW->WINDOW_SIZE bytes.  */
static inline void
sliding_window_copy_second_half (sliding_window_t *window,
				 char *buffer, size_t *size)
{
  memcpy (buffer, window->windows[1], window->sizes[1]);
  *size = window->sizes[1];
}

static inline const char *
sliding_window_first_half (sliding_window_t *window)
{
  return (window->windows[0]);
}

static inline size_t
sliding_window_first_half_size (sliding_window_t *window)
{
  return (window->sizes[0]);
}

/* Clear WINDOW's contents, i.e. make it empty.  */
static inline void
sliding_window_clear (sliding_window_t *window)
{
  window->sizes[0] = window->sizes[1] = 0;
  window->offset = 0;
}

/* Initialize WINDOW to be a sliding window of size SIZE.  Memory is
   allocated on the stack.  Returns an error code.  */
static inline errcode_t
sliding_window_init (sliding_window_t *window, size_t size)
{
  window->raw_window =
    chop_calloc (2 * size, (chop_class_t *) &chop_anchor_based_chopper_class);
  if (!window->raw_window)
    return ENOMEM;

  window->windows[0] = window->raw_window;
  window->windows[1] = window->raw_window + size;

  window->offset = 0;
  window->sizes[0] = window->sizes[1] = 0;
  window->window_size = size;
  window->raw_size = 2 * size;

  return 0;
}

static inline void
sliding_window_destroy (sliding_window_t *window)
{
  chop_free (window->raw_window,
	     (chop_class_t *) &chop_anchor_based_chopper_class);
  window->raw_window = NULL;
  window->windows[0] = window->windows[1] = NULL;
}


/* Fingerprint computation routines.  */

/* Compute the fingerprint that comes after FPR.  FPR is both an input value
   (the previous fingerprint) and an output argument (the newly computed
   fingerprint).  */
static inline fpr_t
compute_next_window_fingerprint (chop_anchor_based_chopper_t *anchor,
				 char first_char, char last_char,
				 register fpr_t fpr)
{
#if (defined HAVE_LIGHTNING_H) && (defined INLINE_LIGHTNING_CODE)
  register jit_insn *movi;
  volatile unsigned long multiplication;

  if (!anchor->jit_multiply_with_prime_to_the_ws.patched)
    {
      register jit_insn *jmp;

      jmp  = anchor->jit_multiply_with_prime_to_the_ws.final_jump;
      jit_patch_at (jmp, &&after_mult);
      anchor->jit_multiply_with_prime_to_the_ws.patched = 1;
    }

  /* This one has to be patched every type since MULTIPLICATION is on the
     stack. */
  movi = anchor->jit_multiply_with_prime_to_the_ws.operand_ptr_movi;
  jit_patch_movi (movi, &multiplication);
#endif

  fpr *= ANCHOR_PRIME_NUMBER;

#if (defined HAVE_LIGHTNING_H) && (defined INLINE_LIGHTNING_CODE)
  /* Set the input parameter.  */
  multiplication = anchor->prev_first_char;
  goto *anchor->jit_multiply_with_prime_to_the_ws.func;

 after_mult:
  /* At this point, MULTIPLICATION should contain the result.  */
  fpr -= multiplication;
#else
  fpr -= multiply_with_prime_to_the_ws (anchor, anchor->prev_first_char);
#endif

  fpr += last_char;
  fpr &= ANCHOR_MODULO_MASK;

  anchor->prev_first_char = first_char;
  anchor->prev_fpr = fpr;

  return fpr;
}

static inline fpr_t
compute_window_fingerprint (chop_anchor_based_chopper_t *anchor,
			    sliding_window_t *window)
{
  register fpr_t fpr;
  const char *p;
  char first_char = '\0';
  fpr_t prime_power = 1;
  size_t total = anchor->window_size;

#define ITERATE_OVER_SUBWINDOW(subwin, end_offset, start_offset)	\
  do									\
    {									\
      for (p = subwin + end_offset - 1;					\
	   p >= subwin + start_offset;					\
	   p--)								\
	{								\
	  fpr_t this_fpr = *p;						\
									\
	  first_char = *p;						\
	  fpr += this_fpr * prime_power;				\
	  fpr &= ANCHOR_MODULO_MASK;					\
									\
	  prime_power *= ANCHOR_PRIME_NUMBER;				\
	  if (--total == 0)						\
	    break;							\
	}								\
    }									\
  while (0)


  /* Traverse WINDOW from its end offset to its start offset.  That is, first
     traverse the second subwindow from OFFSET to zero, and then the first
     subwindow from WINDOW_SIZE to OFFSET.  */
  fpr = 0;

  if ((sliding_window_start_offset (window) > 0) && (window->sizes[1] > 0))
    ITERATE_OVER_SUBWINDOW (window->windows[1],
			    sliding_window_start_offset (window), 0);

  if (total > 0)
    ITERATE_OVER_SUBWINDOW (window->windows[0], window->sizes[0],
			    sliding_window_start_offset (window));

  /* Note: At this point, if less than WINDOW_SIZE bytes were available from
     WINDOW (WINDOW was ``unfull''), then TOTAL is greater than zero.  But
     that's no problem.  */
  assert ((sliding_window_unfull (window)) || (total == 0));

  /* ANCHOR->PREV_FIRST_CHAR is then used in
     COMPUTE_NEXT_WINDOW_FINGERPRINT.  */
  anchor->prev_first_char = first_char;

  return fpr;
#undef ITERATE_OVER_SUBWINDOW
}



/* Initialization code.  */
static errcode_t
ab_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_anchor_based_chopper_t *chopper =
    (chop_anchor_based_chopper_t *)object;

  chopper->chopper.stream = NULL;
  chopper->chopper.read_block = chop_anchor_chopper_read_block;
  chopper->chopper.typical_block_size = 0;
  chopper->chopper.close = chop_anchor_chopper_close;

  chopper->window_size = 0;
  chopper->first = 1;
#ifndef HAVE_LIGHTNING_H
  memset (&chopper->product_cache, 0, sizeof (chopper->product_cache));
#else
  JIT_MULTIPLIER_INIT (chopper->jit_multiply_with_prime_to_the_ws);
#endif

  return 0;
}

static void
ab_dtor (chop_object_t *object)
{
  chop_anchor_based_chopper_t *anchor =
    (chop_anchor_based_chopper_t *)object;

  sliding_window_destroy (&anchor->sliding_window);
  chop_object_destroy ((chop_object_t *)&anchor->log);

#ifdef HAVE_LIGHTNING_H
  chop_free (JIT_MULTIPLIER_FUNC (anchor->jit_multiply_with_prime_to_the_ws),
	     (chop_class_t *) &chop_anchor_based_chopper_class);
#endif
}

errcode_t
chop_anchor_based_chopper_init (chop_stream_t *input,
				size_t window_size,
				unsigned long magic_fpr_mask,
				chop_chopper_t *uchopper)
{
  errcode_t err;
  size_t i;
  chop_anchor_based_chopper_t *chopper =
    (chop_anchor_based_chopper_t *)uchopper;

  chop_object_initialize ((chop_object_t *)chopper,
			  (chop_class_t *)&chop_anchor_based_chopper_class);

  chopper->chopper.stream = input;
  chopper->chopper.typical_block_size = magic_fpr_mask + window_size;
  chopper->window_size = window_size;
  chopper->magic_fpr_mask = magic_fpr_mask;

  err = sliding_window_init (&chopper->sliding_window, window_size);
  if (err)
    return err;

  /* Precompute ANCHOR_PRIME_NUMBER to the WINDOW_SIZE.  */
  chopper->prime_to_the_ws = 1;
  for (i = 0; i < window_size; i++)
    chopper->prime_to_the_ws *= ANCHOR_PRIME_NUMBER;

#ifdef HAVE_LIGHTNING_H
  chopper->jit_multiply_with_prime_to_the_ws =
    compile_multiplication_function (chopper->prime_to_the_ws);

  if (!JIT_MULTIPLIER_FUNC (chopper->jit_multiply_with_prime_to_the_ws))
    return ENOMEM;
#endif

  err = chop_log_init ("anchor-based-chopper", &chopper->log);

  return err;
}



static errcode_t
chop_anchor_chopper_read_block (chop_chopper_t *chopper,
				chop_buffer_t *buffer, size_t *size)
{
  /* Algorithm:

     1.  If less than WINDOW_SIZE bytes are available starting from the
         current offset of the sliding window, read in WINDOW_SIZE bytes

     2.  Compute the fingerprint of each WINDOW_SIZE-long sliding window,
         i.e. fingerprint of [0..29], then [1..30], ..., [30..59].

     3.  Whenever such a fingerprint is considered "magic", then make it an
         anchor and return all the data read till then, including the
         WINDOW_SIZE bytes which yielded the magic value.  Goto 1.  */


/* Return true if FPR should be chosen as an anchor point.  */
#define IS_ANCHOR_FINGERPRINT(_fpr)   (((_fpr) & magic_fpr_mask) == 0)

  errcode_t err;
  sliding_window_t *window;
  int first = 1;
  char *window_dest;
  size_t start_offset, *window_dest_size;
  register fpr_t window_fpr = 0;
  register fpr_t magic_fpr_mask;
  chop_anchor_based_chopper_t *anchor =
    (chop_anchor_based_chopper_t *)chopper;

  *size = 0;
  chop_buffer_clear (buffer);
  window = &anchor->sliding_window;

  magic_fpr_mask = anchor->magic_fpr_mask;
  start_offset = window->offset;

  anchor->first = 0;
  while (1)
    {
      if (CHOP_EXPECT_FALSE (sliding_window_unfull (window)))
	{
	  /* There are less that WINDOW_SIZE bytes left in WINDOW so we need
	     to get some more.  */
	  size_t discarded;

	  window_dest = sliding_window_dest_buffer (window, &window_dest_size,
						    &discarded);
	  if (discarded)
	    {
	      /* Flush the data we're about to discard.  */
	      assert (discarded >= start_offset);
	      chop_log_printf (&anchor->log, "appending %u bytes to block",
			       discarded - start_offset);
	      err = chop_buffer_append (buffer, window_dest + start_offset,
					discarded - start_offset);
	      if (err)
		return err;

	      start_offset = 0;
	    }

	  *window_dest_size = 0;
	  err = read_sliding_window (anchor, window_dest,
				     window_dest_size);
	  chop_log_printf (&anchor->log, "reloaded sliding window, "
			   "got %u bytes", *window_dest_size);

	  if (CHOP_EXPECT_FALSE (err))
	    {
	      if (err == CHOP_STREAM_END)
		/* That's it: stop computing fingerprints.  */
		break;
	      else
		return err;
	    }

	  if (sliding_window_unfull (window))
	    /* Looks like we're about to read the end of stream.  */
	    continue;
	}

      /* Compute a fingerprint of the current WINDOW_SIZE bytes.  */
      if (first)
	{
	  /* For the first window, we must compute the fingerprint from
	     scratch.  */
	  window_fpr = compute_window_fingerprint (anchor, window);
	  first = 0;
	}
      else
	{
	  /* For subsequent windows, the fingerprint can be computed
	     efficiently based on the previous fingerprint.  */
	  register char first_char, last_char;

	  first_char = sliding_window_first_char (window);
	  last_char = sliding_window_last_char (window);

	  window_fpr = compute_next_window_fingerprint (anchor,
							first_char, last_char,
							window_fpr);
#ifdef AUTO_TEST /* debugging */
	  {
	    fpr_t ref_fpr;

	    ref_fpr = compute_window_fingerprint (anchor, window);
	    assert (ref_fpr == window_fpr);
	  }
#endif
	}

      chop_log_printf (&anchor->log, "fingerprint: 0x%x", window_fpr);
      if (IS_ANCHOR_FINGERPRINT (window_fpr))
	{
	  /* This looks like an anchor.  If we've reached the end of stream,
	     we need to flush the remaining bytes.  */
	  size_t amount;

	  /* Push all the bytes up to the anchor itself into the user's
	     buffer (we consider the anchor to be the location of the end of
	     the current sliding window).  */
	  assert (sliding_window_end_offset (window) >= start_offset);
	  amount = sliding_window_end_offset (window) - start_offset;

	  if (amount)
	    {
	      err = sliding_window_append_to_buffer (window, start_offset,
						     amount, buffer);
	      if (err)
		return err;

	      /* Fingerprinting will resume after the current WINDOW_SIZE
		 bytes on the next call.  */
	      sliding_window_skip (window, window->window_size);

	      chop_log_printf (&anchor->log,
			       "found an anchor (fpr: 0x%x, block size: %u)",
			       window_fpr, chop_buffer_size (buffer));

	      break;
	    }
	}

      /* Shift the sliding window by one byte.  */
      sliding_window_increment_offset (window);
    }

  if (CHOP_EXPECT_FALSE (err == CHOP_STREAM_END))
    {
      /* We've reached the end of the input stream.  */
      size_t amount;

      /* Flush the remaining bytes.  */
      amount = sliding_window_end_offset (window) - start_offset;
      chop_log_printf (&anchor->log, "end of stream, flushing %u bytes left",
		       amount);

      err = sliding_window_append_to_buffer (window, start_offset,
					     amount, buffer);

      /* Clear WINDOW's contents.  */
      sliding_window_clear (window);

      if ((!err) && (chop_buffer_size (buffer) == 0))
	err = CHOP_STREAM_END;
    }


  *size = chop_buffer_size (buffer);
  return err;
}

static void
chop_anchor_chopper_close (chop_chopper_t *chopper)
{
  ab_dtor ((chop_object_t *)chopper);
}


chop_log_t *
chop_anchor_based_chopper_log (chop_chopper_t *chopper)
{
  chop_anchor_based_chopper_t *anchor =
    (chop_anchor_based_chopper_t *)chopper;

  /* Run-time overhead */
  if (chop_object_is_a ((chop_object_t *)chopper,
			(chop_class_t *)&chop_anchor_based_chopper_class))
    return (&anchor->log);

  return NULL;
}

