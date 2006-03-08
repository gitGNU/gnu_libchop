/* Service discovery and browsing.  The service we are interested in here is
   ``block stores''.  */

#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/store-browsers.h>



/* Hash specifications.  FIXME: Move it to `hash.c'.  */

chop_hash_method_spec_t
chop_read_hash_method_spec (const char *str)
{
  chop_hash_method_spec_t spec;

  if (!str)
    spec.spec_type = CHOP_HASH_SPEC_NONE;
  else
    {
      if (!strcmp (str, "none"))
	spec.spec_type = CHOP_HASH_SPEC_ANY;
      else
	{
	  chop_hash_method_t hash;

	  if (chop_hash_method_lookup (str, &hash))
	    spec.spec_type = CHOP_HASH_SPEC_UNKNOWN;
	  else
	    {
	      spec.spec_type = CHOP_HASH_SPEC_SPECIFIED;
	      spec.method = hash;
	    }
	}
    }

  return spec;
}

const char *
chop_hash_method_spec_to_string (chop_hash_method_spec_t spec)
{
  static const char s_none[] = "none";
  static const char s_any[] = "any";
  static const char s_unknown[] = "unknown";

  switch (spec.spec_type)
    {
    case CHOP_HASH_SPEC_NONE:
      return s_none;

    case CHOP_HASH_SPEC_ANY:
      return s_any;

    case CHOP_HASH_SPEC_SPECIFIED:
      {
	const char *h = chop_hash_method_name (spec.method);
	return (h ? h : s_unknown);
      }

    case CHOP_HASH_SPEC_UNKNOWN:
    default:
      return s_unknown;
    }

  return s_unknown;
}


/* Store browser interface.  */

CHOP_DEFINE_RT_CLASS (store_browser, object,
		      NULL, NULL, /* ctor/dtor */
		      NULL, NULL, /* copy/equal */
		      NULL, NULL  /* serial/deserial */);


errcode_t
chop_store_browser_iterate (chop_store_browser_t *browser,
			    unsigned timeout)
{
  if (browser->iterate)
    return browser->iterate (browser, timeout);

  return CHOP_ERR_NOT_IMPL;
}

errcode_t
chop_store_browser_loop (chop_store_browser_t *browser)
{
  if (browser->loop)
    return browser->loop (browser);

  return CHOP_ERR_NOT_IMPL;
}



/* arch-tag: fbe8e8cb-1944-481b-a74e-a71c3f26e1b7
 */
