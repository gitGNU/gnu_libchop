/* Filters, as in GStreamer's `GstElement', `GstBin' and `GstPipeline',
   `GstPad' also.
   http://gstreamer.freedesktop.org/data/doc/gstreamer/head/gstreamer/html/GstElement.html
*/

#include "chop.h"

/* A few examples of filters: rsync, zlib/bzlib, ciphering.  */

typedef struct chop_filter chop_filter_t;
typedef enum chop_filter_event chop_filter_event_t;

struct chop_filter
{
  struct chop_filter_event_handler;

  errcode_t (* push) (struct chop_filter *, const char *, size_t);
  errcode_t (* pull) (struct chop_filter *, char *, size_t, size_t *);
};

struct chop_filter_event_handler
{
  errcode_t (* send_event) (struct chop_filter *,
			    enum chop_filter_event_t);
  void *data;
};

enum chop_filter_event
  {
    CHOP_FILTER_EVENT_NONE,
    CHOP_FILTER_EVENT_FULL,
    CHOP_FILTER_EVENT_EMPTY
  };


extern errcode_t chop_filter_stream_open (chop_stream_t *source,
					  chop_filter_t *filter,
					  chop_stream_t *stream);


extern errcode_t chop_filter_push (chop_filter_t *, const char *,
				   size_t);

extern errcode_t chop_filter_pull (chop_filter_t *, char *,
				   size_t, size_t *);

extern errcode_t chop_filter_set_event_handler (chop_filter_t *,
						chop_filter_event_handler_t *);

extern errcode_t chop_filter_event_handler (const chop_filter_t *);


extern errcode_t chop_filter_event_handler_init (chop_filter_event_handler_t *,
						 void *data);

extern errcode_t chop_filter_event_handler_send_event (chop_filter_event_handler_t *,
						       chop_filter_event_t);

