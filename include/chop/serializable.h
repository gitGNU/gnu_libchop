#ifndef __CHOP_SERIALIZABLE_H__
#define __CHOP_SERIALIZABLE_H__

/* Definition of a mini run-time object system that is used as the basis of a
   serializable object framework.  The point of this system was originally to
   provide a generic object serialization framework.  Besides, it can be used
   to provide binary compatibility with almost no gratuitous overhead: the
   size of a given class' instances can be known at run-time, which allows
   for caller allocation.  Callers can allocate objects of a given class as
   they prefer (e.g. on the stack) while still having binary compatilibity
   accross changes of a class' fields layout.  */

#include <chop/chop.h>
#include <chop/hash.h>
#include <chop/buffers.h>


/* Serializable objects.  */

typedef enum chop_serial_method chop_serial_method_t;

enum chop_serial_method
  {
    CHOP_SERIAL_ASCII,
    CHOP_SERIAL_BINARY
  };


typedef struct chop_class chop_class_t;
typedef struct chop_object chop_object_t;
typedef errcode_t (* chop_serializer_t) (const chop_object_t *,
					 chop_serial_method_t,
					 chop_buffer_t *);
/* FIXME:  There should be a `size_t *offset' stating where deserialization
   ended within BUFFER and a CHOP_SERIAL_TOO_SHORT error when BUFFER is not
   long enough.  */
typedef errcode_t (* chop_deserializer_t) (const char *buffer,
					   size_t size,
					   chop_serial_method_t,
					   chop_object_t *);
typedef void (* chop_constructor_t) (chop_object_t *,
				     const chop_class_t *);
typedef void (* chop_destructor_t) (chop_object_t *);

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

#define CHOP_DECLARE_RT_CLASS_WITH_METACLASS(_name, _parent, _metaclass,	\
					     _fields)				\
     _DEF_STRUCT (_name, _parent, _fields);					\
     extern const chop_ ## _metaclass ## _t chop_ ## _name ## _class;

/* Define a run-time class that has been declared previously.  The _PARENT
   argument must be consistent with the one used in the declaration.  _PARENT
   mush never be null since all classes must inherit from CHOP_OBJECT_CLASS
   (and `chop_object_t').  The constructor/destructor and
   serializer/deserializer may be NULL if they are not needed or not
   implemented.

   FIXME:  We shouldn't declare them as `const': this would allow users to do
   funny things like change the class constructors, etc.  */
#define CHOP_DEFINE_RT_CLASS(_name, _parent, _cons, _dest,	\
			     _serial, _deserial)		\
     const chop_class_t chop_ ## _name ## _class =		\
       {							\
	 .name = _STRINGIFY (_name),				\
	 .object = { .class = &chop_class_class },		\
	 .parent = &(chop_ ## _parent ## _class),		\
	 .constructor = _cons,					\
	 .destructor = _dest,					\
	 .serializer = _serial,					\
	 .deserializer = _deserial,				\
	 .instance_size = sizeof (chop_ ## _name ## _t),	\
       };


/* Same as above except that METACLASS gives the name of the class to be used
   as the class of the class being defined.  METACLASS_INITS are C static
   structure initializers.  */
#define CHOP_DEFINE_RT_CLASS_WITH_METACLASS(_name, _parent,			\
					    _metaclass, _metaclass_inits,	\
					    _cons, _dest,			\
					    _serial, _deserial)			\
     const chop_ ## _metaclass ## _t						\
     chop_ ## _name ## _class =							\
       {									\
	 .class =								\
	 {									\
	   .name = _STRINGIFY (_name),						\
	   .object = { .class = &chop_ ## _metaclass ## _class },		\
	   .parent = &(chop_ ## _parent ## _class),				\
	   .constructor = _cons,						\
	   .destructor = _dest,							\
	   .serializer = _serial,						\
	   .deserializer = _deserial,						\
	   .instance_size = sizeof (chop_ ## _name ## _t),			\
	 },									\
	 _metaclass_inits							\
       };



/* Example:

   CHOP_DECLARE_RT_CLASS (cow, animal,
			  struct chop_cow *parent;
                          struct chop_cow *boyfriend;
			  chop_field_t *home;)

   ...  defines `struct chop_cow' and `chop_cow_t';  declares
   `chop_cow_class', a global variable of type `chop_class_t'.
   The type `chop_animal_t' must already exist and inherit from
   `chop_object_t'.

   CHOP_DEFINE_RT_CLASS (cow, animal, cow_init, cow_destroy,
			 cow_serialize, cow_deserialize);

   ...  defines and initializes `chop_cow_class'.  The variable
   `chop_animal_class' must have been declared earlier.
*/


/* Initialize OBJECT which is to be an instance of CLASS.  This allows for
   "virtual constructors" in C++ terms.  This means that it can also be
   relativey costly since it may yield several function calls.  */
extern void chop_object_initialize (chop_object_t *object,
				    const chop_class_t *class);

/* Initialize OBJECT, which is expected to be of type CLASS, by deserializing
   BUFFER (of SIZE bytes), according to METHOD.  On success, zero is returned
   and OBJECT is initialized.  Otherwise, OBJECT is left in an undefined
   state.  */
static __inline__ errcode_t
chop_object_deserialize (chop_object_t *__object,
			 const chop_class_t *__class,
			 chop_serial_method_t __method,
			 const char *__buffer,
			 size_t __size)
{
  if (!__class->deserializer)
    return CHOP_ERR_NOT_IMPL;

  __object->class = __class;
  return (__class->deserializer (__buffer, __size, __method, __object));
}

/* Destroy OBJECT, i.e. deallocate any resources allocated by it.  */
extern void chop_object_destroy (chop_object_t *object);

/* Initialize CLASS with the parameters provided.  PARENT mush never be null
   since all classes must inherit from CHOP_OBJECT_CLASS (and
   `chop_object_t').  The constructor/destructor and serializer/deserializer
   may be NULL if they are not needed or not implemented.  If is usually
   sufficient to use the CHOP_DEFINE_RT_CLASS macro which initializes a class
   object at compile-time.  */
static __inline__ void
chop_class_initialize (chop_class_t *__class,
		       size_t __instance_size,
		       const chop_class_t *__parent,
		       chop_constructor_t __constructor,
		       chop_destructor_t __destructor,
		       chop_serializer_t __serializer,
		       chop_deserializer_t __deserializer)
{
  chop_object_initialize ((chop_object_t *)&__class, &chop_class_class);
  __class->instance_size = __instance_size;
  __class->parent = __parent;
  __class->constructor = __constructor;
  __class->destructor = __destructor;
  __class->serializer = __serializer;
  __class->deserializer = __deserializer;
}

/* Return the name of class CLASS.  */
static __inline__ const char *
chop_class_name (const chop_class_t *__class)
{
  return (__class->name);
}

/* Return the size of an instance of CLASS.  */
static __inline__ size_t
chop_class_instance_size (const chop_class_t *__class)
{
  return (__class->instance_size);
}

/* Return the parent class of CLASS.  All classes should inherit from
   `chop_object_t'.  However, `chop_object_t' inherits from nobody so this
   function returns NULL when passed CHOP_OBJECT_CLASS.  */
static __inline__ const chop_class_t *
chop_class_parent_class (const chop_class_t *__class)
{
  return (__class->parent);
}

/* Return the constructor of CLASS.  */
static __inline__ chop_constructor_t
chop_class_constructor (const chop_class_t *__class)
{
  return (__class->constructor);
}

/* Return the destructor of CLASS.  */
static __inline__ chop_destructor_t
chop_class_destructor (const chop_class_t *__class)
{
  return (__class->destructor);
}

/* Set the constructor of CLASS to CTOR.  */
static __inline__ void
chop_class_set_constructor (chop_class_t *__class,
			    chop_constructor_t __ctor)
{
  __class->constructor = __ctor;
}

/* Set the destructor of CLASS to DTOR.  */
static __inline__ void
chop_class_set_destructor (chop_class_t *__class,
			   chop_destructor_t __dtor)
{
  __class->destructor = __dtor;
}

/* Allocate an instance of CLASS on the stack.  The instance _must_ be
   initialized afterwards, e.g. with `chop_object_initialize ()'.  */
#define chop_class_alloca_instance(_class)			\
  ((void *)alloca (chop_class_instance_size (_class)))

/* Return the class of object OBJECT.  */
static __inline__ const chop_class_t *
chop_object_get_class (const chop_object_t *__object)
{
  return (__object->class);
}

/* Return non-zero OBJECT's type is CLASS or a derivative.  */
static __inline__ int
chop_object_is_a (const chop_object_t *__object, const chop_class_t *__class)
{
  const chop_class_t *_c;
  for (_c = chop_object_get_class (__object);
       _c != NULL;
       _c = chop_class_parent_class (_c))
    {
      if (_c == __class)
	return 1;
    }
  return 0;
}

/* Serialize OBJECT according to serialization method METHOD into BUFFER.  If
   not serializer exists for OBJECT's class, CHOP_ERR_NOT_IMPL is returned.
   On success, zero is returned.  */
static __inline__ errcode_t
chop_object_serialize (const chop_object_t *__object,
		       chop_serial_method_t __method,
		       chop_buffer_t *__buffer)
{
  const chop_class_t *__class = chop_object_get_class (__object);
  if (__class->serializer)
    return (__class->serializer (__object, __method, __buffer));

  return CHOP_ERR_NOT_IMPL;
}


#include <chop/chop-config.h>

#ifdef HAVE_GPERF
/* Note: we currently use GPerf for provide an O(1) lookup of all the
   built-in classes.  */

/* Lookup the built-in class named NAME.  If NAME was not found, return
   NULL.  Note that classes' canonical name doesn't include the `chop_'
   prefix.  For instance, the name of the `chop_stream_t' class is simply
   "stream".  */
extern const chop_class_t *chop_class_lookup (const char *name);

#endif

#endif

