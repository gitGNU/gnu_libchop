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

#define ANCHOR_PRIME_NUMBER (7)

/* Declare `chop_anchor_based_chopper_t' which inherits from
   `chop_chopper_t'.  */
CHOP_DECLARE_RT_CLASS (anchor_based_chopper, chopper,
		       /* Sliding widow size */
		       size_t window_size;

		       /* The value of ANCHOR_PRIME_NUMBER to the
			  WINDOW_SIZE */
		       fpr_t  prime_to_the_ws;

		       /* Cache of a sliding window previously read */
		       char  *cached_window;
		       size_t cached_window_offset;
		       size_t cached_window_size;

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



static errcode_t
chop_anchor_chopper_read_block (chop_chopper_t *, chop_buffer_t *,
				size_t *);

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
  /* FIXME:  Implement `close ()'! */

  chopper->window_size = window_size;
  chopper->first = 1;
  memset (&chopper->product_cache, 0, sizeof (chopper->product_cache));

  chopper->cached_window = calloc (window_size, sizeof (char));
  if (!chopper->cached_window)
    return ENOMEM;

  chopper->cached_window_size = 0;
  chopper->cached_window_offset = 0;

  /* Precompute ANCHOR_PRIME_NUMBER to the WINDOW_SIZE.  */
  chopper->prime_to_the_ws = ANCHOR_PRIME_NUMBER;
  for (i = 0; i < window_size; i++)
    chopper->prime_to_the_ws *= ANCHOR_PRIME_NUMBER;

  err = chop_log_init ("anchor-based-chopper", &chopper->log);

  return err;
}


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

static inline void
compute_next_window_fingerprint (chop_anchor_based_chopper_t *anchor,
				 char first_char, char last_char,
				 fpr_t *fpr)
{
  *fpr  = anchor->prev_fpr * ANCHOR_PRIME_NUMBER;
  *fpr -= multiply_with_prime_to_the_ws (anchor, anchor->prev_first_char);
  *fpr += last_char;

  anchor->prev_first_char = first_char;
  anchor->prev_fpr = *fpr;
}

static inline void
compute_window_fingerprint (chop_anchor_based_chopper_t *anchor,
			    const char *window, size_t size,
			    fpr_t *fpr)
{
  const char *p;
  fpr_t prime_power = 1;

  *fpr = 0;
  for (p = window + size - 1; p >= window; p--)
    {
      fpr_t this = *p;
      *fpr += this * prime_power;
      prime_power *= ANCHOR_PRIME_NUMBER;
    }
}


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

  if (window->offsets[0] + window->window_size < window->sizes[0])
    last_char = window->windows[0][window->offsets[0] + window->window_size];
  else
    {
      if (window->offsets[1] < window->sizes[1])
	last_char = window->windows[1][window->offsets[1]];
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
	     window the first one and fetch a new window.  */
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


/* Initialize WINDOW to be a sliding window of size SIZE.  Memory is
   allocated on the stack.  Returns an error code (this uses a GNU C
   extension).  */
#define sliding_window_init(_window, _size)			\
({								\
  errcode_t err = 0;						\
  (_window)->windows[0] = alloca (_size);			\
  (_window)->windows[1] = alloca (_size);			\
  if ((!(_window)->windows[0]) || (!(_window)->windows[1]))	\
    err = ENOMEM;						\
  else								\
    {								\
      (_window)->window_size = (_size);				\
      (_window)->offsets[0] = (_window)->offsets[1] = 0;	\
      (_window)->sizes[0] = (_window)->sizes[1] = 0;		\
    }								\
  err;								\
})



/* Return true if FPR should be chosen as an anchor point.  */
#define IS_ANCHOR_FINGERPRINT(_fpr)   (((_fpr) & 0x3f) == 0)

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
  sliding_window_t window;
  int end_of_stream = 0;
  char *window_dest;
  size_t start_offset, discard_size, *window_dest_size;
  fpr_t window_fpr;
  chop_anchor_based_chopper_t *anchor =
    (chop_anchor_based_chopper_t *)chopper;

  *size = 0;
  chop_buffer_clear (buffer);

  err = sliding_window_init (&window, anchor->window_size);
  if (err)
    return err;

  window_dest = sliding_window_dest_buffer (&window, &window_dest_size,
					    &discard_size);
  assert (discard_size == 0);

  if (anchor->first)
    {
      /* Read in the first two windows */
      err = read_sliding_window (anchor, window_dest, window_dest_size);
      if (err)
	return err;
      start_offset = 0;
    }
  else
    {
      /* Get the first window from cache */
      memcpy (window_dest, anchor->cached_window,
	      anchor->cached_window_size);
      *window_dest_size = anchor->cached_window_size;
      start_offset = anchor->cached_window_offset;
      sliding_window_increase_offset (&window, start_offset);
    }

  /* Get the second window */
  assert (sliding_window_end (&window));
  window_dest = sliding_window_dest_buffer (&window, &window_dest_size,
					    &discard_size);
  assert (discard_size == 0);

  err = read_sliding_window (anchor, window_dest, window_dest_size);
  if ((err) && (err != CHOP_STREAM_END))
    return err;

  end_of_stream = (err == CHOP_STREAM_END);
  if ((end_of_stream) && (!anchor->first))
    /* This is the only place where we return CHOP_STREAM_END.  */
    return CHOP_STREAM_END;

  while (1)
    {
      /* Compute a fingerprint of the current WINDOW_SIZE bytes.  */
      if (anchor->first)
	{
	  const char *first_part;
	  first_part = sliding_window_first_half (&window);
	  compute_window_fingerprint (anchor, first_part,
				      sliding_window_first_half_size (&window),
				      &window_fpr);
	  anchor->first = 0;
	}
      else
	{
	  char first_char, last_char;

	  first_char = sliding_window_first_char (&window);
	  last_char = sliding_window_last_char (&window);

	  compute_next_window_fingerprint (anchor, first_char, last_char,
					   &window_fpr);
	}

      if ((IS_ANCHOR_FINGERPRINT (window_fpr)) || (end_of_stream))
	{
	  /* This looks like an anchor: append the whole window till the
	     end to the user's buffer.  Otherwise, if we've reached the end
	     of stream, we need to flush the remaining bytes.  */
	  size_t amount;

	  amount = sliding_window_end_offset (&window) - start_offset;
	  err = sliding_window_append_to_buffer (&window, start_offset,
						 amount, buffer);
	  if (err)
	    return err;

	  *size = chop_buffer_size (buffer);
	  chop_log_printf (&anchor->log, "found an anchor (block size: %u)",
			   *size);

	  /* Copy the second window into cache */
	  sliding_window_copy_second_half (&window, anchor->cached_window,
					   &anchor->cached_window_size);
	  anchor->cached_window_offset =
	    sliding_window_end_offset (&window) - anchor->window_size;
	  break;
	}

      /* Shift the sliding window by one byte.  */
      sliding_window_increase_offset (&window, 1);

      if (sliding_window_end (&window))
	{
	  /* There are now less that WINDOW_SIZE bytes left in WINDOW.  */
	  if (end_of_stream)
	    {
	      /* We've already reached the end of the input stream.  */
	      chop_log_printf (&anchor->log, "end of stream");
	      return 0;
	    }

	  /* Append the first window to BUFFER.  Make the second window the
	     first one and fetch a new window.  */
	  window_dest = sliding_window_dest_buffer (&window,
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

  return err;
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

