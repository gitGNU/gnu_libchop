#include <chop/chop.h>
#include <chop/streams.h>
#include <chop/serializable.h>  /* Serializable objects */

#include <stdio.h>


/* The constructor of class objects.  */
static void
_chop_class_primitive_init (chop_object_t *object,
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
chop_class_destroy (chop_object_t *object)
{
  chop_class_t *class = (chop_class_t *)object;
  class->parent = NULL;
}

/* Primitive object constructor.  */
static void
_chop_object_primitive_init (chop_object_t *object, const chop_class_t *class)
{
  object->class = class;
}

/* Destructor of `chop_object_t' objects.  */
static void
_chop_object_primitive_destroy (chop_object_t *object)
{
  object->class = NULL;
}


/* The root classes.  */
const chop_class_t chop_class_class =
  {
    .object = { .class = &chop_class_class },
    .parent = NULL,
    .constructor = _chop_class_primitive_init,
    .destructor = chop_class_destroy,
    .serializer = NULL,
    .deserializer = NULL,
    .instance_size = sizeof (chop_class_t)
  };

const chop_class_t chop_object_class =
  {
    .object { .class = &chop_class_class },
    .parent = NULL,
    .constructor = _chop_object_primitive_init,
    .destructor = _chop_object_primitive_destroy,
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
  const char *p, *end = buffer + size;
  for (p = buffer; p < end; p++)
    {
      sprintf (hex, "%02x", *p);
      hex += 2;
    }

  *hex = '\0';
}



/* Initialization.  */

errcode_t
chop_init (void)
{
  initialize_chop_error_table ();
  return 0;
}

