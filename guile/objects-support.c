/* Support code for the object API.  */

#include <chop/chop.h>
#include <chop/objects.h>

#include <g-wrap/guile-runtime.h>

#include <string.h>

#include "core-support.h"


/* Return the `<chop-class>' WCT.  */
static inline SCM
gwrap_chop_class_wct (void)
{
  static SCM guile_chop_class_wct = SCM_BOOL_F;

  if (CHOP_EXPECT_FALSE (guile_chop_class_wct == SCM_BOOL_F))
    {
      /* Find the `<chop-class>' WCT.  */
      SCM module;

      module = scm_c_resolve_module ("chop objects");
      guile_chop_class_wct = scm_c_module_lookup (module, "<chop-class>");
      guile_chop_class_wct = scm_variable_ref (guile_chop_class_wct);
      guile_chop_class_wct = scm_gc_protect_object (guile_chop_class_wct);
    }

  return guile_chop_class_wct;
}

static inline int
chop_scm_object_is_a (SCM obj, const chop_class_t *c_class)
{
  /* FIXME: We should check whether OBJ is an actual `object' WCP.  */
  if (gw_wcp_p (obj))
    {
      const chop_object_t *c_obj;

      c_obj = (chop_object_t *) gw_wcp_get_ptr (obj);
      return (chop_object_is_a (c_obj, c_class));
    }

  return 0;
}

static inline errcode_t
chop_scm_serialize_object_ascii (SCM obj, char **c_str)
{
  /* FIXME: We should check whether OBJ is an actual `object' WCP.  */
  if (CHOP_EXPECT_TRUE (gw_wcp_p (obj)))
    {
      errcode_t err;
      chop_buffer_t c_buf;
      const chop_object_t *c_obj;

      c_obj = (chop_object_t *) gw_wcp_get_ptr (obj);
      err = chop_buffer_init (&c_buf, 0);
      if (err)
	return err;

      err = chop_object_serialize (c_obj, CHOP_SERIAL_ASCII, &c_buf);
      if (err)
	{
	  chop_buffer_return (&c_buf);
	  return err;
	}

      /* Note: We use raw `malloc ()' since the caller will use
	 `scm_take_locale_string ()'.  */
      *c_str = malloc (chop_buffer_size (&c_buf));
      memcpy (*c_str, chop_buffer_content (&c_buf),
	      chop_buffer_size (&c_buf));

      return 0;
    }

  return CHOP_INVALID_ARG;
}

static inline errcode_t
chop_scm_deserialize_object (const chop_class_t *c_class,
			     chop_serial_method_t c_serial,
			     const char *c_input, size_t c_input_len,
			     SCM *object, size_t *c_bytes_read)
{
  errcode_t err;
  chop_object_t *c_obj;

  c_obj = gwrap_chop_malloc (c_class);
  err = chop_object_deserialize (c_obj, c_class, c_serial,
				 c_input, c_input_len,
				 c_bytes_read);
  if (CHOP_EXPECT_TRUE (err == 0))
    *object = gw_wcp_assimilate_ptr ((void *) c_obj,
				     gwrap_chop_class_wct ());
  else
    *object = SCM_BOOL_F, gwrap_chop_free_uninitialized (c_obj, c_class);

  return err;
}

static inline errcode_t
chop_scm_deserialize_object_ascii (const chop_class_t *c_class,
				   const char *c_input,
				   SCM *object, size_t *c_bytes_read)
{
  return (chop_scm_deserialize_object (c_class, CHOP_SERIAL_ASCII,
				       c_input, strlen (c_input),
				       object, c_bytes_read));
}

/* arch-tag: 81116c21-55dd-4ec4-b4bd-0db91964ff29
 */
