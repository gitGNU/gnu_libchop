#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/serializable.h>  /* Serializable objects */

#include <stdio.h>
#include <assert.h>


/* The constructor of class objects.  */
static void
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
}

/* The destructor of classes.  */
static void
_class_primitive_destroy (chop_object_t *object)
{
  chop_class_t *class = (chop_class_t *)object;
  class->parent = NULL;
}

/* Primitive object constructor.  */
static void
_object_primitive_init (chop_object_t *object, const chop_class_t *class)
{
  object->class = class;
}

/* Destructor of `chop_object_t' objects.  */
static void
_object_primitive_destroy (chop_object_t *object)
{
  object->class = NULL;
}


/* The root classes.  */
CHOP_DEFINE_RT_CLASS (class, class,
		      _class_primitive_init, _class_primitive_destroy,
		      NULL, NULL  /* No serializer/deserializer */);

const chop_class_t chop_object_class =
  {
    .name = "object",
    .object { .class = &chop_class_class },
    .parent = NULL,
    .constructor = _object_primitive_init,
    .destructor = _object_primitive_destroy,
    .serializer = NULL,
    .deserializer = NULL,
    .instance_size = sizeof (chop_object_t)
  };



/* Run-time object system support code.  */

void
chop_object_initialize (chop_object_t *object,
			const chop_class_t *class)
{
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
	    parent->constructor (object, class);
	}
    }

  if (class->constructor)
    class->constructor (object, class);

  object->class = (chop_class_t *)class;
}

void
chop_object_destroy (chop_object_t *object)
{
  const chop_class_t *class;
  for (class = object->class;
       class != NULL;
       class = class->parent)
    {
      if (class->destructor)
	class->destructor (object);
    }
}


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
chop_hex_string_to_buffer (const char *hex, size_t size, char *buffer)
{
#define tonum(_chr) (((_chr) >= 'a') ? ((_chr) - 'a' + 10) : ((_chr) - '0'))
  const unsigned char *p, *end = hex + size;

  if (size & 1)
    /* If SIZE is odd, discard the last character from HEX */
    size--;

  for (p = (unsigned char *)hex; p < end; p += 2)
    {
      char digit[2];
      memcpy (digit, p, 2);
      *buffer = tonum (digit[0]) << 4;
      *buffer |= tonum (digit[1]);
      buffer++;
    }
#undef tonum
}


/* Initialization.  */

errcode_t
chop_init (void)
{
  initialize_chop_error_table ();
  return 0;
}

