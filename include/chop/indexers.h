
/* Indexers */

#include <chop/chop.h>

typedef struct chop_indexer chop_indexer_t;
typedef struct chop_index_handle chop_index_handle_t;

struct chop_indexer
{
  errcode_t (* index_stream) (struct chop_indexer *,
			      chop_stream_t *,
			      chop_chopper_t *,
			      chop_store_t *,
			      chop_index_handle_t *);

  errcode_t (* fetch_stream) (struct chop_indexer *,
			      const chop_index_handle_t *,
			      chop_store_t *,
			      chop_stream_t *);

  size_t sizeof_stream;
  size_t sizeof_index_handle;
};

#define _make_sizeof_getter(_type)					\
static __inline__ size_t						\
chop_indexer_size_of_ ## _type (const chop_indexer_t *__indexer)	\
{									\
  return (__indexer->sizeof_ ## _type);					\
}

_make_sizeof_getter (stream);
_make_sizeof_getter (index_handle);

#undef _make_sizeof_getter

#define chop_indexer_alloca_stream(__indexer)				\
((chop_stream_t *)alloca (chop_indexer_sizeof_stream (__indexer)))

#define chop_indexer_alloca_index_handle(__indexer)			       \
((chop_index_handle_t *)alloca (chop_indexer_sizeof_index_handle (__indexer)))

