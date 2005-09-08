/* Simple and stupid buffer allocation and reuse.  */

#include <chop/buffers.h>

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>


/* Define the following variable to compile-in pool support.  */
#define ENABLE_POOL 1

#ifdef ENABLE_POOL
/* Keep at most BUFFER_POOL_MAX_SIZE buffers in the buffer pool, for a total
   of at most BUFFER_POOL_MAX_AVAILABLE bytes.  */
#define BUFFER_POOL_MAX_SIZE       (20)
#define BUFFER_POOL_MAX_AVAILABLE  (3000)

static chop_buffer_t buffer_pool[BUFFER_POOL_MAX_SIZE];
static size_t        buffer_pool_size = 0;      /* Number of buffers in pool */
static size_t        buffer_pool_available = 0; /* In bytes */


/* Find a buffer in the pool whose size is at greater than or equal to SIZE.
   Return non-zero if a matching buffer was found and set *FOUND to the
   matching buffer's description.  */
static inline int
find_buffer_in_pool (size_t size, chop_buffer_t *found)
{
  unsigned buf;
  for (buf = 0; buf < buffer_pool_size; buf++)
    {
      if (buffer_pool[buf].real_size >= size)
	{
	  *found = buffer_pool[buf];
	  buffer_pool_size--;
	  if (buffer_pool_size > 0)
	    /* Move the last buffer */
	    buffer_pool[buf] = buffer_pool[buffer_pool_size];

	  return 1;
	}
    }

  return 0;
}
#endif


errcode_t
chop_buffer_init (chop_buffer_t *buffer, size_t size)
{
#ifdef ENABLE_POOL
  if (find_buffer_in_pool (size, buffer))
    return 0;
#endif

  buffer->buffer = (char *)calloc (1, size);
  if (!buffer->buffer)
    return ENOMEM;

  buffer->real_size = size;
  buffer->size = 0;

  return 0;
}

errcode_t
chop_buffer_grow (chop_buffer_t *buffer, size_t size)
{
  size_t new_size = buffer->real_size;
  char *larger;

#ifdef ENABLE_POOL
  if (find_buffer_in_pool (size, buffer))
    return 0;
#endif

  new_size = (new_size == 0) ? 1 : new_size;
  while (new_size < size)
    new_size <<= 1;

  larger = realloc (buffer->buffer, new_size);
  if (!larger)
    return ENOMEM;

  buffer->buffer = larger;
  buffer->real_size = new_size;

  return 0;
}

errcode_t
chop_buffer_push (chop_buffer_t *buffer,
		  const char *buf, size_t size)
{
  errcode_t err;

  if (size > buffer->real_size)
    {
      err = chop_buffer_grow (buffer, size);
      if (err)
	return err;
    }

  memcpy (buffer->buffer, buf, size);
  buffer->size = size;

  return 0;
}

errcode_t
chop_buffer_append (chop_buffer_t *buffer,
		    const char *buf, size_t size)
{
  errcode_t err;
  size_t new_size = buffer->size + size;

  if (new_size > buffer->real_size)
    {
      err = chop_buffer_grow (buffer, new_size);
      if (err)
	return err;
    }

  memcpy (buffer->buffer + buffer->size, buf, size);
  buffer->size = new_size;

  return 0;
}

#ifdef ENABLE_POOL
static inline void
_chop_buffer_return (chop_buffer_t *buffer)
{
  if ((buffer_pool_size < BUFFER_POOL_MAX_SIZE)
      && (buffer_pool_available + buffer->real_size
	  < BUFFER_POOL_MAX_AVAILABLE))
    {
      buffer_pool[buffer_pool_size++] = *buffer;
      buffer_pool_available += buffer->real_size;
    }
}
#endif

void
chop_buffer_return (chop_buffer_t *buffer)
{
#ifdef ENABLE_POOL
  _chop_buffer_return (buffer);
#else
  if (buffer->buffer)
    free (buffer->buffer);
#endif

  buffer->size = buffer->real_size = 0;
  buffer->buffer = 0;
}

