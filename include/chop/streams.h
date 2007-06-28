
#ifndef __CHOP_STREAMS_H__
#define __CHOP_STREAMS_H__

/* Input data streams.  */

#include <chop/chop.h>
#include <chop/objects.h>


_CHOP_BEGIN_DECLS

/* Declare the input stream class `chop_stream_t' (represented at run-time by
   CHOP_STREAM_CLASS) inheriting from `chop_object_t'.  */
CHOP_DECLARE_RT_CLASS (stream, object,
		       char *name;
		       size_t preferred_block_size;

		       /* common methods */
		       errcode_t (* read)  (struct chop_stream *,
					    char *, size_t, size_t *);
		       void (* close) (struct chop_stream *););


extern const chop_class_t chop_file_stream_class;
extern const chop_class_t chop_mem_stream_class;
extern const chop_class_t chop_filtered_stream_class;



/* Stream methods */

/* Return the preferred block size for reading STREAM.  */
static __inline__ size_t
chop_stream_preferred_block_size (const chop_stream_t *__stream)
{
  return (__stream->preferred_block_size);
}

/* Read at most SIZE bytes from STREAM into BUFFER.  On failure, return an
   error code (non-zero).  Otherwise, return in READ the number of bytes
   actually read.  */
static __inline__ errcode_t
chop_stream_read (chop_stream_t *__stream,
		  char *__buffer,
		  size_t __size,
		  size_t *__read)
{
  return (__stream->read (__stream, __buffer, __size, __read));
}

/* Close STREAM, i.e. deallocate any resources associated to it.  */
static __inline__ void
chop_stream_close (chop_stream_t *__stream)
{
  __stream->close (__stream);
}



/* Specific stream constructors.  */

/* Open file located at PATH and initialize STREAM as a file stream
   representing this file.  STREAM has to point to a large-enough memory area
   to hold an object whose class is CHOP_FILE_STREAM_CLASS.  */
extern errcode_t chop_file_stream_open (const char *path,
					chop_stream_t *stream);

/* Open a memory-backed stream, i.e. a stream whose input is read from BASE
   which is SIZE byte-long.  If FREE_FUNC is NULL, it is called upon closing
   STREAM.  */
extern void chop_mem_stream_open (const char *base, size_t size,
				  void (* free_func) (void *),
				  chop_stream_t *stream);

#include <chop/filters.h>

/* Initialize STREAM as a filtered stream that reads input data from BACKEND
   through FILTER.  BPS defines the semantics of STREAM as a proxy of BACKEND
   (whether BACKEND should eventually be destroyed, etc.).  Similarly, if
   OWNS_FILTER is true, then closing STREAM will destroy FILTER.  */
extern errcode_t chop_filtered_stream_open (chop_stream_t *backend,
					    chop_proxy_semantics_t bps,
					    chop_filter_t *filter,
					    int owns_filter,
					    chop_stream_t *stream);


_CHOP_END_DECLS;

#endif
