#ifndef __CHOP_SERIALIZABLE_H__
#define __CHOP_SERIALIZABLE_H__

/* Definition of a mini run-time object system that is used as the basis of a
   serializable object framework.  */

#include <chop/chop.h>
#include <chop/hash.h>
#include <chop/buffers.h>


/* Serializable objects.  */
typedef struct chop_class chop_class_t;
typedef struct chop_object chop_object_t;
typedef errcode_t (* chop_serializer_t) (const chop_object_t *,
					 chop_hash_method_t,
					 chop_buffer_t *);
typedef errcode_t (* chop_deserializer_t) (const char *buffer,
					   size_t size,
					   chop_object_t *);
typedef void (* chop_constructor_t) (chop_object_t *,
				     const chop_class_t *);
typedef void (* chop_destructor_t) (chop_object_t *);

typedef enum chop_serial_method chop_serial_method_t;

enum chop_serial_method
  {
    CHOP_SERIAL_ASCII,
    CHOP_SERIAL_BINARY
  };

struct chop_object
{
  const chop_class_t *class;
};

struct chop_class
{
  /* Obviously, classes are objects whose class is `chop_class_class' */
  chop_object_t object;

  const char *name;
  const struct chop_class *parent;

  size_t instance_size;
  chop_constructor_t constructor;
  chop_destructor_t destructor;
  chop_serializer_t serializer;
  chop_deserializer_t deserializer;
};

/* The base class object, or the "root object" if you prefer.  */
extern const chop_class_t chop_class_class;

/* The object class that corresponds to `chop_object_t'.  */
extern const chop_class_t chop_object_class;

/* Convenience macros for defining run-time classes.  */

#define _DEF_STRUCT(_name, _parent, _fields)	\
typedef struct chop_ ## _name			\
{						\
  chop_ ## _parent ## _t _parent;		\
  _fields					\
}						\
chop_ ## _name ## _t;

#define __STRINGIFY(_x) # _x
#define _STRINGIFY(_z) __STRINGIFY (_z)

/* Declare a run-time class (typically in a public header file).  */
#define CHOP_DECLARE_RT_CLASS(_name, _parent, _fields)		\
     _DEF_STRUCT (_name, _parent, _fields);			\
     extern const chop_class_t chop_ ## _name ## _class;

/* Define a run-time class that has been declared previously.  The _PARENT
   argument must be consistent with the one used in the declaration.  */
#define CHOP_DEFINE_RT_CLASS(_name, _parent, _cons, _dest,	\
			     _serial, _deserial)		\
     const chop_class_t chop_ ## _name ## _class =		\
       {							\
	 .object = { .class = &chop_class_class },		\
	 .name = _STRINGIFY (_name),				\
	 .parent = &(chop_ ## _parent ## _class),		\
	 .instance_size = sizeof (chop_ ## _name ## _t),	\
	 .constructor = _cons,					\
	 .destructor = _dest,					\
	 .serializer = _serial,					\
	 .deserializer = _deserial				\
       };


/* Example:

   CHOP_DEFINE_RT_CLASS (cow, animal, cow_init, cow_destroy,
			 cow_serialize, cow_deserialize,
			 struct chop_cow *parent;
			 chop_field_t *home;
			 );

*/

extern void
chop_class_set_serializer (chop_class_t *,
			   chop_hash_method_t method,
			   chop_serializer_t);

/* Initialize OBJECT which is to be an instance of CLASS.  This allows for
   "virtual constructors" in C++ terms.  */
extern void chop_object_initialize (chop_object_t *object,
				    const chop_class_t *class);

static __inline__ void
chop_class_initialize (chop_class_t *__class,
		       const chop_class_t *__parent,
		       chop_constructor_t __constructor,
		       chop_destructor_t __destructor,
		       chop_serializer_t __serializer,
		       chop_deserializer_t __deserializer)
{
  chop_object_initialize ((chop_object_t *)&__class, &chop_class_class);
  __class->parent = __parent;
  __class->constructor = __constructor;
  __class->destructor = __destructor;
  __class->serializer = __serializer;
  __class->deserializer = __deserializer;
}

/* Destroy OBJECT, i.e. deallocate any resources allocated by it.  */
extern void chop_object_destroy (chop_object_t *object);

/* Return the name of class CLASS.  */
static __inline__ const char *
chop_class_name (const chop_class_t *__class)
{
  return (__class->name);
}

/* Return the class of object OBJECT.  */
static __inline__ const chop_class_t *
chop_object_get_class (const chop_object_t *__object)
{
  return (__object->class);
}

#endif

