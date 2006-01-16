#include <chop/chop.h>
#include <chop/cipher.h>
#include <chop/streams.h>
#include <chop/objects.h>  /* Serializable objects */

#include <chop/chop-config.h>

#include <stdio.h>
#include <ctype.h>
#include <assert.h>


/* Define this to enable run-time tracking of the objects created and
   destroyed.  Among other things, this allows to check that an object about
   to be destroyed was actually constructed before.  */
#define USE_OBJECT_TRACKER

/* If both USE_OBJECT_TRACKER and TRACK_OBJECT_LEAKS, then libchop will
   report about non-destroy objects when the program finishes.  */
#undef TRACK_OBJECT_LEAKS


#ifdef USE_OBJECT_TRACKER

#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

#ifdef __GNU_LIBRARY__
# include <execinfo.h>
#endif

typedef struct chop_tracked_object
{
  struct chop_tracked_object *next;
  chop_object_t *object;
#ifdef __GNU_LIBRARY__
# define BACKTRACE_SIZE  20
  void  *backtrace[BACKTRACE_SIZE];
  size_t backtrace_size;
#endif
} chop_tracked_object_t;

#define BUCKET_COUNT  4096
static chop_tracked_object_t *tracked_objects[BUCKET_COUNT];

/* A list of the recently untracked (i.e. probably destroyed) objects.  */
#define RECENTLY_UNTRACKED_COUNT 4096
static chop_object_t *recently_untracked[RECENTLY_UNTRACKED_COUNT];
static size_t         recently_untracked_head = 0;

#define chop_object_hash(_obj) (((uintptr_t)(_obj) >> 2) & (BUCKET_COUNT - 1))


static inline void
chop_object_tracker_init (void)
{
  memset (tracked_objects, '\0', sizeof (tracked_objects));
}

static inline errcode_t
chop_track_object (chop_object_t *object)
{
  uintptr_t bucket;
  chop_tracked_object_t *new_object, *next;

  new_object = malloc (sizeof (*new_object));
  if (!new_object)
    return ENOMEM;

  bucket = chop_object_hash (object);
  next = tracked_objects[bucket];
  tracked_objects[bucket] = new_object;

  new_object->next = next;
  new_object->object = object;
#ifdef __GNU_LIBRARY__
  new_object->backtrace_size = backtrace (new_object->backtrace,
					  BACKTRACE_SIZE);
#endif

  return 0;
}

static inline int
chop_object_is_tracked (chop_object_t *object)
{
  uintptr_t bucket;
  chop_tracked_object_t *p;

  bucket = chop_object_hash (object);
  for (p = tracked_objects[bucket]; p; p = p->next)
    {
      if (p->object == object)
	return 1;
    }

  return 0;
}

static inline void
chop_object_mark_as_untracked (chop_object_t *object)
{
  recently_untracked[recently_untracked_head] = object;
  recently_untracked_head =
    (recently_untracked_head + 1) % RECENTLY_UNTRACKED_COUNT;
}

static inline int
chop_object_was_recently_untracked (chop_object_t *object)
{
  size_t num;

  for (num = (recently_untracked_head - 1) % RECENTLY_UNTRACKED_COUNT;
       num != recently_untracked_head;
       num = (num - 1) % RECENTLY_UNTRACKED_COUNT)
    {
      if (recently_untracked[num] == object)
	return 1;

      if (!recently_untracked[num])
	return 0;
    }

  return 0;
}

static inline int
chop_test_and_untrack_object (chop_object_t *object)
{
  uintptr_t bucket;
  chop_tracked_object_t *obj, *prev;

  bucket = chop_object_hash (object);

  for (obj = tracked_objects[bucket], prev = NULL;
       obj;
       prev = obj, obj = obj->next)
    {
      if (obj->object == object)
	{
	  chop_tracked_object_t *next = obj->next;
	  if (prev)
	    prev->next = next;
	  else
	    tracked_objects[bucket] = next;

	  chop_object_mark_as_untracked (object);

	  free (obj);

	  return 1;
	}
    }

  return 0;
}

static inline void
show_tracked_object (chop_tracked_object_t *p)
{
#ifdef __GNU_LIBRARY__
  char **symbols, **s;

  symbols = backtrace_symbols (p->backtrace, p->backtrace_size);

  fprintf (stderr, "%s: object @ %p, class `%s', initialized from:\n",
	   __FUNCTION__, p->object,
	   chop_class_name (p->object->class));
  if (symbols)
    {
      size_t remaining = p->backtrace_size;

      for (s = symbols; remaining; s++, remaining--)
	fprintf (stderr, "    %s\n", *s);
      fprintf (stderr, "\n");

      free (symbols);
    }
  else
    fprintf (stderr,  "   (stack trace unavailable)\n");
#else
  fprintf (stderr, "%s: object @ %p, class `%s'\n",
	   __FUNCTION__, p->object,
	   chop_class_name (p->object->class));
#endif
}

static inline void
chop_show_object_info (chop_object_t *object)
{
  uintptr_t bucket;
  chop_tracked_object_t *p;

  bucket = chop_object_hash (object);
  for (p = tracked_objects[bucket]; p; p = p->next)
    {
      if (p->object == object)
	{
	  show_tracked_object (p);
	  return;
	}
    }
}

static inline void
chop_show_tracked_objects (void)
{
  uintptr_t bucket;
  chop_tracked_object_t *p;

  for (bucket = 0;
       bucket < sizeof (tracked_objects) / sizeof (*tracked_objects);
       bucket++)
    {
      for (p = tracked_objects[bucket]; p; p = p->next)
	{
	  /* CHOP_CIPHER_LOG is always be leaked, so there's no point in
	     tracking it.  */
	  if (p->object != (chop_object_t *)&chop_cipher_log)
	    show_tracked_object (p);
	}
    }
}

#ifdef TRACK_OBJECT_LEAKS
static void
show_leaks (void)
{
  fprintf (stderr, "\n\n* libchop objects leaked:\n\n");
  chop_show_tracked_objects ();
}
#endif

#endif /* USE_OBJECT_TRACKER */



/* The constructor of class objects.  */
static errcode_t
_class_primitive_init (chop_object_t *object,
		       const chop_class_t *metaclass)
{
  chop_class_t *class = (chop_class_t *)object;
  class->name = NULL;
  class->parent = NULL;
  class->constructor = NULL;
  class->destructor = NULL;
  class->serializer = NULL;
  class->deserializer = NULL;

  return 0;
}

/* The destructor of classes.  */
static void
_class_primitive_destroy (chop_object_t *object)
{
  chop_class_t *class = (chop_class_t *)object;
  class->parent = NULL;
}

/* Primitive object constructor.  */
static errcode_t
_object_primitive_init (chop_object_t *object, const chop_class_t *class)
{
  object->class = class;

#ifdef USE_OBJECT_TRACKER
  {
    errcode_t err;

    err = chop_track_object (object);
    if (err)
      return err;
  }
#endif

  return 0;
}

/* Destructor of `chop_object_t' objects.  */
static void
_object_primitive_destroy (chop_object_t *object)
{
#ifdef USE_OBJECT_TRACKER
  if (!chop_test_and_untrack_object (object))
    {
      fprintf (stderr, "%s: trying to destroy invalid chop object @ %p\n",
	       __FUNCTION__, object);
      if (chop_object_was_recently_untracked (object))
	fprintf (stderr, "%s: object %p was recently destroyed already\n",
		 __FUNCTION__, object);

      abort ();
    }
#endif

  object->class = NULL;
}


/* The root classes.  */
CHOP_DEFINE_RT_CLASS (class, class,
		      _class_primitive_init, _class_primitive_destroy,
		      NULL, NULL  /* No serializer/deserializer */);

const chop_class_t chop_object_class =
  {
    .name = "object",
    .object = { .class = &chop_class_class },
    .parent = NULL,
    .constructor = _object_primitive_init,
    .destructor = _object_primitive_destroy,
    .serializer = NULL,
    .deserializer = NULL,
    .instance_size = sizeof (chop_object_t)
  };



/* Run-time object system support code.  */

errcode_t
chop_object_initialize (chop_object_t *object,
			const chop_class_t *class)
{
  errcode_t err = 0;
  int parentcnt = 0;
  const chop_class_t *parent, *parents[256];

  for (parent = class->parent;
       parent != NULL;
       parent = parent->parent)
    {
      parents[parentcnt++] = parent;
      if (parent->parent == parent)
	/* The parent of `chop_class_class' is itself.  */
	break;
    }

  if (parentcnt)
    {
      for (parent = parents[--parentcnt];
	   parentcnt >= 0;
	   parent = parents[--parentcnt])
	{
	  if (parent->constructor)
	    {
	      err = parent->constructor (object, class);
	      if (err)
		break;
	    }
	}
    }

  if (!err)
    {
      if (class->constructor)
	class->constructor (object, class);

      object->class = (chop_class_t *)class;
    }

  return err;
}

void
chop_object_destroy (chop_object_t *object)
{
  const chop_class_t *class;

#ifdef USE_OBJECT_TRACKER
  /* Actual ``untracking'' takes place in `_object_primitive_destroy ()'.  */
  if (!chop_object_is_tracked (object))
    {
      fprintf (stderr, "%s: trying to destroy invalid chop object @ %p\n",
	       __FUNCTION__, object);
      if (chop_object_was_recently_untracked (object))
	fprintf (stderr, "%s: object %p was recently destroyed already\n",
		 __FUNCTION__, object);

      abort ();
    }
#endif

  for (class = object->class;
       class != NULL;
       class = class->parent)
    {
      if (class->destructor)
	class->destructor (object);
    }
}


/* Class lookup by name (when GPerf is available).  */

#ifdef HAVE_GPERF

/* The following header declares a class.  */
#include <chop/store-stats.h>

/* Class definitions that are internal to `indexer-hash-tree.c',
   `block-indexer-hash.c' and `block-indexer-chk.c'.  (FIXME)  */
extern const chop_class_t chop_hash_index_handle_class,
  chop_hash_block_indexer_class,
  chop_hash_block_fetcher_class,
  chop_chk_index_handle_class,
  chop_chk_block_indexer_class,
  chop_chk_block_fetcher_class,
  chop_uuid_index_handle_class,
  chop_uuid_block_indexer_class,
  chop_uuid_block_fetcher_class,
  chop_tree_stream_class;

/* Store-related class definitions.  (FIXME too: this is becoming ugly!) */
extern const chop_class_t chop_gdbm_block_iterator_class,
  chop_tdb_block_iterator_class,
  chop_bdb_block_iterator_class,
  chop_qdbm_block_iterator_class;


/* Include the gperf-generated perfect hash table.  */
#include "class-lookup.c"

typedef struct chop_class_entry chop_class_entry_t;

const chop_class_t *
chop_class_lookup (const char *name)
{
  const chop_class_entry_t *entry;

  entry = chop_lookup_class_entry (name, strlen (name));
  if (!entry)
    /* FIXME:  There should be a class registry available at run-time for
       classes that are not built-in.  */
    return NULL;

  return ((chop_class_t *)entry->class);
}

#else
# warning "`chop_class_lookup ()' not compiled in."
#endif


/* Block keys helper functions.  */

void
chop_buffer_to_hex_string (const char *buffer, size_t size, char *hex)
{
#define tochar(_num) (((_num) < 10) ? ('0' + (_num)) : ('a' - 10 + (_num)))
  const unsigned char *p, *end = (unsigned char *)buffer + size;
  for (p = (unsigned char *)buffer; p < end; p++)
    {
      *(hex++) = tochar (*p >> 4);
      *(hex++) = tochar (*p & 0xf);
    }
#undef tochar

  *hex = '\0';
}

void
chop_hex_string_to_buffer (const char *hex, size_t size, char *buffer,
			   const char **end)
{
#define tonum(_chr) (((_chr) >= 'a') ? ((_chr) - 'a' + 10) : ((_chr) - '0'))
  const unsigned char *p, *end_of_buf = (unsigned char *)hex + size;

  if (size & 1)
    /* If SIZE is odd, discard the last character from HEX */
    size--;

  for (p = (unsigned char *)hex; p < end_of_buf; p += 2)
    {
      if ((!isxdigit (p[0])) || (!isxdigit (p[1])))
	break;

      *buffer = tonum (p[0]) << 4;
      *buffer |= tonum (p[1]);
      buffer++;
    }
#undef tonum

  *end = (char *)p;
}

void /* untested */
chop_integer_to_hex_string (unsigned num, char *hex)
{
#define tochar(_num) (((_num) < 10) ? ('0' + (_num)) : ('a' - 10 + (_num)))
  size_t size;

  for (size = sizeof (unsigned) / 4;
       size;
       size--)
    {
      *(hex++) = tochar (num & 0xf);
      num >>= 4;
    }
#undef tochar

  *hex = '\0';
}



/* Initialization.  */

errcode_t
chop_init (void)
{
  initialize_chop_error_table ();

#ifdef USE_OBJECT_TRACKER
  chop_object_tracker_init ();

#ifdef TRACK_OBJECT_LEAKS
  /* Provide a list of leaked objects.  */
  atexit (show_leaks);
#endif
#endif

  return chop_log_init ("cipher", &chop_cipher_log);
}

