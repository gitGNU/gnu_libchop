
#ifndef __CHOP_STREAMS_H__
#define __CHOP_STREAMS_H__

/* Input data streams.  */

struct chop_stream
{
  char *name;
  size_t preferred_block_size;

  /* common methods */
  errcode_t (* read)  (struct chop_stream *, char *, size_t, size_t *);
  void (* close) (struct chop_stream *);

  /* placeholders */
  void *_u1;
  void *_u2;
  void *_u3;
  void *_u4;
  void *_u5;
};

/* Derived types */
typedef struct chop_file_stream chop_file_stream_t;
typedef struct chop_ext2_stream chop_ext2_stream_t;
typedef struct chop_mem_stream chop_mem_stream_t;
typedef struct chop_filter_stream chop_filter_stream_t;


/* File stream implementation */
struct chop_file_stream
{
  chop_stream_t stream;

  int    fd;
  size_t size;
  char  *map;
  size_t position;
};


/* Stream methods */

static __inline__ size_t
chop_stream_preferred_block_size (const chop_stream_t *__stream)
{
  return (__stream->preferred_block_size);
}

static __inline__ errcode_t
chop_stream_read (chop_stream_t *__stream,
		  char *__buffer,
		  size_t __size,
		  size_t *__read)
{
  return (__stream->read (__stream, __buffer, __size, __read));
}

static __inline__ void
chop_stream_close (chop_stream_t *__stream)
{
  __stream->close (__stream);
}


/* Specific stream constructors.  */

extern errcode_t chop_file_stream_open (const char *path,
					chop_file_stream_t *stream);

extern errcode_t chop_ext2_stream_open (const char *path,
					const char *fs,
					chop_ext2_stream_t *stream);

extern errcode_t chop_mem_stream_open (const char *buffer,
				       size_t size,
				       chop_mem_stream_t *stream);

#endif
