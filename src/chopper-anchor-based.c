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

#include <chop/chop.h>
#include <chop/choppers.h>

#include <stdint.h>
#include <assert.h>
#include <alloca.h>
#include <errno.h>


/* A (sort of) Rabin fingerprint.  */
typedef uint32_t fpr_t;

/* A "sliding window": contains two buffers of WINDOW_SIZE bytes and (almost)
   automatically manages transition from one to the other.  This is some sort
   of a double buffering scheme.  */
typedef struct
{
  size_t window_size;
  char *windows[2];
  size_t offsets[2];
  size_t sizes[2];
} sliding_window_t;


/* Declare `chop_anchor_based_chopper_t' which inherits from
   `chop_chopper_t'.  */
CHOP_DECLARE_RT_CLASS (anchor_based_chopper, chopper,
		       /* Sliding widow size */
		       size_t window_size;

		       /* The value of ANCHOR_PRIME_NUMBER to the
			  WINDOW_SIZE */
		       fpr_t  prime_to_the_ws;

		       /* The sliding window (double buffer) that is used
			  when reading the input stream.  */
		       sliding_window_t sliding_window;

		       /* Cache of multiplication of a byte by PRIME to the
			  WINDOW_SIZE */
		       fpr_t product_cache[256];

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

CHOP_DEFINE_RT_CLASS (anchor_based_chopper, chopper,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);


/* These are the main parameters of the algorithm.  Here the `M' parameter
   is chosen to be 2^30 (see ANCHOR_MODULO_MASK) and `p'
   (ANCHOR_PRIME_NUMBER) is 3.  This way, in
   `compute_next_window_fingerprint ()', we can multiply the previous
   fingerprint by ANCHOR_PRIME_NUMBER without risking to overflow the 32-bit
   `fpr_t' type.  */
#define ANCHOR_PRIME_NUMBER (3)
#define ANCHOR_MODULO_MASK  (0x3fffffff)

/* Return true if FPR should be chosen as an anchor point.  */
#define IS_ANCHOR_FINGERPRINT(_fpr)   (((_fpr) & 0x3f) == 0)



static errcode_t
chop_anchor_chopper_read_block (chop_chopper_t *, chop_buffer_t *,
				size_t *);

static void
chop_anchor_chopper_close (chop_chopper_t *);



/* Multiply WHAT by ANCHOR->PRIME to the ANCHOR->WINDOW_SIZE.  */
static inline fpr_t
multiply_with_prime_to_the_ws (chop_anchor_based_chopper_t *anchor,
			       char what)
{
  fpr_t cached;

  if (what == '\0')
    return 0;

  cached = anchor->product_cache[what];
  if (cached == 0)
    {
      /* Not computed yet: compute the result and cache it.  */
      fpr_t result = (fpr_t)what;

      result *= anchor->prime_to_the_ws;
      anchor->product_cache[what] = result;

      return result;
    }

  return (cached);
}


/* Read a whole window (ie. ANCHOR->WINDOW_SIZE bytes) from ANCHOR's input
   stream and store it inot BUFFER.  */
static inline errcode_t
read_sliding_window (chop_anchor_based_chopper_t *anchor,
		     char *buffer, size_t *size)
{
  chop_stream_t *input = anchor->chopper.stream;

  return (chop_stream_read (input, buffer, anchor->window_size, size));
}

/* Compute the fingerprint that comes after FPR.  FPR is both an input value
   (the previous fingerprint) and an output argument (the newly computed
   fingerprint).  */
static inline void
compute_next_window_fingerprint (chop_anchor_based_chopper_t *anchor,
				 char first_char, char last_char,
				 fpr_t *fpr)
{
  *fpr *= ANCHOR_PRIME_NUMBER;
  *fpr -= multiply_with_prime_to_the_ws (anchor, anchor->prev_first_char);
  *fpr += last_char;
  *fpr &= ANCHOR_MODULO_MASK;

  anchor->prev_first_char = first_char;
  anchor->prev_fpr = *fpr;
}

static inline void
compute_window_fingerprint (chop_anchor_based_chopper_t *anchor,
			    sliding_window_t *window,
			    fpr_t *fpr)
{
  int subwinnum;
  const char *subwin, *p;
  char first_char = '\0';
  fpr_t prime_power = 1;
  size_t total = anchor->window_size;
  size_t start_offset, end_offset;

  /* Traverse the WINDOW from its end offset to its start offset.  */
  *fpr = 0;
  for (subwinnum = 1, start_offset = 0, end_offset = window->offsets[0];
       subwinnum >= 0;
       subwinnum--,
	 start_offset = window->offsets[0], end_offset = window->sizes[0])
    {
      subwin = window->windows[subwinnum];

      for (p = subwin + end_offset - 1; p >= subwin + start_offset; p--)
	{
	  fpr_t this_fpr = *p;

	  first_char = *p;
	  *fpr += this_fpr * prime_power;
	  prime_power *= ANCHOR_PRIME_NUMBER;
	  if (--total == 0)
	    break;
	}

      if (!total)
	break;
    }

  /* ANCHOR->PREV_FIRST_CHAR is then used in
     COMPUTE_NEXT_WINDOW_FINGERPRINT.  */
  anchor->prev_first_char = first_char;
}


/* Sliding windows.  */

/* Return the character at the current start offset of WINDOW.  */
static inline char
sliding_window_first_char (sliding_window_t *window)
{
  char first_char;

  if (window->offsets[0] < window->sizes[0])
    first_char = window->windows[0][window->offsets[0]];
  else
    first_char = window->windows[1][window->offsets[1]];

  return first_char;
}

/* Return the character at the current end offset (start offset + window
   size) of WINDOW.  */
static inline char
sliding_window_last_char (sliding_window_t *window)
{
  char last_char;

  if (window->offsets[0] + window->window_size - 1 < window->sizes[0])
    last_char = window->windows[0][window->offsets[0] + window->window_size-1];
  else
    {
      if (window->offsets[1] < window->sizes[1])
	{
	  assert (window->offsets[1] > 0);
	  last_char = window->windows[1][window->offsets[1] - 1];
	}
      else
	/* Pad with zeros */
	last_char = '\0';
    }

  return last_char;
}

/* Return non-zero if no more than WINDOW->WINDOW_SIZE bytes are available
   from WINDOW's current offset.  */
static inline int
sliding_window_end (sliding_window_t *window)
{
  if (window->sizes[1] > 0)
    return (window->offsets[1] >= window->sizes[1]);

  return (window->offsets[0] + window->window_size >= window->sizes[0]);
}

/* Return a pointer to a WINDOW->WINDOW_SIZE buffer where a new
   WINDOW->WINDOW_SIZE long block can be written.  Return in *DEST_SIZE a
   pointer to this buffer's size which should be updated and be lower than or
   equal to WINDOW->WINDOW_SIZE.  The DISCARD_SIZE bytes of the returned
   buffer may be saved somewhere by the caller before it overwrites them.  */
static inline char *
sliding_window_dest_buffer (sliding_window_t *window,
			    size_t **dest_size,
			    size_t *discard_size)
{
  char *dest;

  if (window->sizes[0] == 0)
    {
      dest = window->windows[0];
      *dest_size = &window->sizes[0];
      *discard_size = 0;
    }
  else
    {
      if (window->sizes[1] == 0)
	{
	  dest = window->windows[1];
	  *dest_size = &window->sizes[1];
	  *discard_size = 0;
	}
      else
	{
	  /* Discard the contents of the first window.  Make the second
	     window the first one.  */
	  char *new_window = window->windows[0];

	  *discard_size = window->sizes[0];

	  window->windows[0] = window->windows[1];
	  window->windows[1] = new_window;
	  window->sizes[0] = window->sizes[1];
	  window->offsets[0] = window->offsets[1] = 0;

	  dest = window->windows[1];
	  *dest_size = &window->sizes[1];
	}
    }

  return dest;
}


static inline size_t
sliding_window_start_offset (sliding_window_t *window)
{
  return (window->offsets[0]);
}

/* Return the end offset of WINDOW, i.e. and integer between zero and
   two times WINDOW->WINDOW_SIZE.  */
static inline size_t
sliding_window_end_offset (sliding_window_t *window)
{
  if (window->sizes[1] == 0)
    return (window->sizes[0]);

  return (window->offsets[1] + window->window_size);
}

static inline void
sliding_window_increase_offset (sliding_window_t *window, size_t offset)
{
  window->offsets[0] += offset;
  window->offsets[1] += offset;
}

/* Append SIZE bytes starting at START_OFFSET from WINDOW to BUFFER.  */
static inline errcode_t
sliding_window_append_to_buffer (sliding_window_t *window,
				 size_t start_offset, size_t size,
				 chop_buffer_t *buffer)
{
  errcode_t err;

  assert (size < (2 * window->window_size) - start_offset);

  if (start_offset < window->window_size)
    {
      /* Copy from the first sub-window */
      size_t amount, available = window->window_size - start_offset;
      amount = (available > size) ? size : available;
      err = chop_buffer_append (buffer, window->windows[0] + start_offset,
				amount);
      if (err)
	return err;
      size -= amount;

      if (size > 0)
	{
	  /* Copy the remaining bytes from the second sub-window */
	  assert (size < window->window_size);
	  err = chop_buffer_append (buffer, window->windows[1], size);
	  if (err)
	    return err;
	}
    }
  else
    {
      /* Copy from the second sub-window */
      assert (size < window->window_size);
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
  window->offsets[0] = window->offsets[1] = 0;
}

/* Initialize WINDOW to be a sliding window of size SIZE.  Memory is
   allocated on the stack.  Returns an error code.  */
static inline errcode_t
sliding_window_init (sliding_window_t *window, size_t size)
{
  window->windows[0] = calloc (size, 1);
  window->windows[1] = calloc (size, 1);
  if ((!window->windows[0]) || (!window->windows[1]))
    return ENOMEM;

  window->offsets[0] = window->offsets[1] = 0;
  window->sizes[0] = window->sizes[1] = 0;
  window->window_size = size;

  return 0;
}

static inline void
sliding_window_destroy (sliding_window_t *window)
{
  free (window->windows[0]);
  free (window->windows[1]);
  window->windows[0] = window->windows[1] = NULL;
}



/* Initialization code.  */
errcode_t
chop_anchor_based_chopper_init (chop_stream_t *input,
				size_t window_size,
				chop_chopper_t *uchopper)
{
  errcode_t err;
  size_t i;
  chop_anchor_based_chopper_t *chopper =
    (chop_anchor_based_chopper_t *)uchopper;

  chop_object_initialize ((chop_object_t *)chopper,
			  &chop_anchor_based_chopper_class);

  chopper->chopper.stream = input;
  chopper->chopper.read_block = chop_anchor_chopper_read_block;
  chopper->chopper.typical_block_size = window_size; /* FIXME: ??? */
  chopper->chopper.close = chop_anchor_chopper_close;

  chopper->window_size = window_size;
  chopper->first = 1;
  memset (&chopper->product_cache, 0, sizeof (chopper->product_cache));

  err = sliding_window_init (&chopper->sliding_window, window_size);
  if (err)
    return err;

  /* Precompute ANCHOR_PRIME_NUMBER to the WINDOW_SIZE.  */
  chopper->prime_to_the_ws = 1;
  for (i = 0; i < window_size; i++)
    chopper->prime_to_the_ws *= ANCHOR_PRIME_NUMBER;

  err = chop_log_init ("anchor-based-chopper", &chopper->log);

  return err;
}


static errcode_t
chop_anchor_chopper_read_block (chop_chopper_t *chopper,
				chop_buffer_t *buffer, size_t *size)
{
  /* Algorithm:

     1.  Read two times WINDOW_SIZE bytes into a "sliding window", actually a
         double buffer;
     2.  Compute the fingerprint of each WINDOW_SIZE-long sliding window,
         i.e. fingerprint of [0..29], then [1..30], ..., [30..59].
     3.  Whenever such a fingerprint is considered "magic", then make it an
         anchor and return all the data read till then.
     4.  When the start offset within the sliding window WINDOW has reached
         WINDOW_SIZE, read in another WINDOW_SIZE bytes and jump to 2.  */
  errcode_t err;
  sliding_window_t *window;
  int end_of_stream = 0, first = 1;
  char *window_dest;
  size_t start_offset = 0, discard_size, *window_dest_size;
  fpr_t window_fpr;
  chop_anchor_based_chopper_t *anchor =
    (chop_anchor_based_chopper_t *)chopper;

  *size = 0;
  chop_buffer_clear (buffer);
  window = &anchor->sliding_window;

  if (anchor->first)
    {
      /* Read in the first two windows */
      window_dest = sliding_window_dest_buffer (window, &window_dest_size,
						&discard_size);
      assert (discard_size == 0);

      err = read_sliding_window (anchor, window_dest, window_dest_size);
      if (err)
	return err;
      start_offset = 0;
    }
  else
    {
      /* We want to start _after_ the anchor, i.e. after the end offset of
	 the previous sliding window.  */
      size_t end_offset = sliding_window_end_offset (window);
      if (end_offset <= anchor->window_size)
	start_offset = 0;
      else
	start_offset = end_offset - anchor->window_size;
    }

  /* Get the second window */
  window_dest = sliding_window_dest_buffer (window, &window_dest_size,
					    &discard_size);
  assert ((!anchor->first) || (discard_size == 0));

  err = read_sliding_window (anchor, window_dest, window_dest_size);
  if ((err) && (err != CHOP_STREAM_END))
    return err;

  end_of_stream = (err == CHOP_STREAM_END);

  /* Resume fingerprinting after the end of the last sliding window.  */
  sliding_window_increase_offset (window, start_offset);

  anchor->first = 0;
  while (1)
    {
      /* Compute a fingerprint of the current WINDOW_SIZE bytes.  */
      if (first)
	{
	  /* For the first window, we must compute the fingerprint from
	     scratch.  */
	  compute_window_fingerprint (anchor, window, &window_fpr);
	  first = 0;
	}
      else
	{
	  /* For subsequent windows, the fingerprint can be computed
	     efficiently based on the previous fingerprint.  */
	  char first_char, last_char;

	  first_char = sliding_window_first_char (window);
	  last_char = sliding_window_last_char (window);

	  compute_next_window_fingerprint (anchor, first_char, last_char,
					   &window_fpr);
	}

      chop_log_printf (&anchor->log, "fingerprint: 0x%x", window_fpr);
      if (IS_ANCHOR_FINGERPRINT (window_fpr))
	{
	  /* This looks like an anchor.  If we've reached the end of stream,
	     we need to flush the remaining bytes.  */
	  size_t amount;

	  /* Push all the bytes up to the anchor itself (located at WINDOW's
	     start offset) into the user's buffer (we consider the anchor to
	     be the location of the end of the current sliding window).  */
	  amount = sliding_window_end_offset (window) - start_offset;
	  if (amount)
	    {
	      err = sliding_window_append_to_buffer (window, start_offset,
						     amount, buffer);
	      if (err)
		return err;

	      chop_log_printf (&anchor->log,
			       "found an anchor (fpr: 0x%x, block size: %u)",
			       window_fpr, chop_buffer_size (buffer));

	      break;
	    }
	}

      /* Shift the sliding window by one byte.  */
      sliding_window_increase_offset (window, 1);

      if (sliding_window_end (window))
	{
	  /* There are now less that WINDOW_SIZE bytes left in WINDOW.  */
	  if (end_of_stream)
	    {
	      /* We've already reached the end of the input stream.  */
	      size_t amount;

	      chop_log_printf (&anchor->log, "end of stream");

	      /* Flush the remaining bytes.  */
	      amount = sliding_window_end_offset (window) - start_offset;
	      err = sliding_window_append_to_buffer (window, start_offset,
						     amount, buffer);

	      /* Clear WINDOW's contents.  */
	      sliding_window_clear (window);

	      if (!err)
		err = CHOP_STREAM_END;

	      break;
	    }

	  /* Append the first window to BUFFER.  Make the second window the
	     first one and fetch a new window.  */
	  window_dest = sliding_window_dest_buffer (window,
						    &window_dest_size,
						    &discard_size);
	  chop_log_printf (&anchor->log, "appending %u bytes to block",
			   discard_size - start_offset);
	  err = chop_buffer_append (buffer, window_dest + start_offset,
				    discard_size - start_offset);
	  if (err)
	    return err;

	  chop_log_printf (&anchor->log, "reloading sliding window");
	  err = read_sliding_window (anchor, window_dest,
				     window_dest_size);
	  start_offset = 0;
	  end_of_stream = (err == CHOP_STREAM_END);

	  /* Don't return CHOP_STREAM_END right now, first return the last
	     block.  */
	  if ((err) && (!end_of_stream))
	    return err;
	}
    }

  *size = chop_buffer_size (buffer);
  return err;
}

static void
chop_anchor_chopper_close (chop_chopper_t *chopper)
{
  chop_anchor_based_chopper_t *anchor =
    (chop_anchor_based_chopper_t *)chopper;

  sliding_window_destroy (&anchor->sliding_window);
}

chop_log_t *
chop_anchor_based_chopper_log (chop_chopper_t *chopper)
{
  chop_anchor_based_chopper_t *anchor =
    (chop_anchor_based_chopper_t *)chopper;

  /* Run-time overhead */
  if (chop_object_is_a ((chop_object_t *)chopper,
			&chop_anchor_based_chopper_class))
    return (&anchor->log);

  return NULL;
}

