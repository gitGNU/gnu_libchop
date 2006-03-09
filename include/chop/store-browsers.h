/* Service discovery for remote block stores.  */

#ifndef __CHOP_STORE_BROWSERS_H__
#define __CHOP_STORE_BROWSERS_H__

#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/hash.h>

/* Hash method specification.  Block stores may advertise a particular block
   naming scheme based on hashes of blocks' contents.  This structure is
   meant to represent such advertisements.  */
typedef struct
{
  enum _spec
    {
      CHOP_HASH_SPEC_NONE = 0,   /* no hash method specification */
      CHOP_HASH_SPEC_ANY,        /* any hash method */
      CHOP_HASH_SPEC_SPECIFIED,  /* specific hash method (see next field) */
      CHOP_HASH_SPEC_UNKNOWN     /* unknown specification (e.g., unknown hash
				    method) */
    } spec_type :2;

  chop_hash_method_t method: 6;
} chop_hash_method_spec_t;

#if 0
/* Store browser events.  */
typedef enum
  {
    CHOP_HASH_BROWSER_EVENT_UNKNOWN = 0,
    CHOP_HASH_BROWSER_EVENT_NEW,     /* new service discovered */
    CHOP_HASH_BROWSER_EVENT_REMOVED  /* service disappeared */
  } chop_store_browser_event_t;

typedef enum
  {
    CHOP_TRANSPORT_PROTOCOL_UNKNOWN = 0,
    CHOP_TRANSPORT_PROTOCOL_TCP,
    CHOP_TRANSPORT_PROTOCOL_UDP
  }
#endif

struct chop_store_browser;


/* Store browser event handlers.  If they return non-zero, then the main
   event loop (or iteration) leaves with an exit status of zero.  */

typedef int (* chop_store_browser_discovery_handler_t)
     (struct chop_store_browser *,
      const char *service_name,
      const char *host,
      unsigned port,
      chop_hash_method_spec_t spec,
      const chop_class_t *client,
      void *userdata);

typedef int (* chop_store_browser_removal_handler_t)
     (struct chop_store_browser *,
      const char *service_name,
      void *userdata);


/* A basic store browser.  */
CHOP_DECLARE_RT_CLASS (store_browser, object,
		       chop_store_browser_discovery_handler_t discovery;
		       void *discovery_data;
		       chop_store_browser_removal_handler_t removal;
		       void *removal_data;
		       errcode_t (* iterate) (struct chop_store_browser *,
					      unsigned timeout);
		       errcode_t (* loop) (struct chop_store_browser *););


_CHOP_BEGIN_DECLS

/* Hash specifications.  */

/* Read the hash method specification denoted by STR (which may be NULL).  */
extern chop_hash_method_spec_t chop_read_hash_method_spec (const char *str);

/* Return true (non-zero) if hash method HASH is compatible with the
   specification SPEC.  */
static __inline__ int
chop_hash_method_compatible (chop_hash_method_spec_t spec,
			     chop_hash_method_t hash)
{
  switch (spec.spec_type)
    {
    case CHOP_HASH_SPEC_NONE:
    case CHOP_HASH_SPEC_ANY:
      return 1;

    case CHOP_HASH_SPEC_UNKNOWN:
      return 0;

    case CHOP_HASH_SPEC_SPECIFIED:
      return (hash == spec.method);

    default:
      return 0;
    }

  return 0;
}

/* Return a string representing hash specification SPEC.  */
const char *chop_hash_method_spec_to_string (chop_hash_method_spec_t spec);


/* Interface.  */

extern errcode_t chop_store_browser_iterate (chop_store_browser_t *browser,
					     unsigned timeout);

extern errcode_t chop_store_browser_loop (chop_store_browser_t *browser);


/* Implementations.  */

extern const chop_class_t chop_avahi_store_browser_class;

/* Return a browser that looks for block stores in domain DOMAIN (a FQDN,
   e.g., `.local', or `.laas.fr.') and invokes DISCOVERY upon new service
   discoveries and REMOVAL upon disappearance of a service.  DOMAIN may be
   NULL in which case it defaults to the local domain.  */
extern errcode_t
chop_avahi_store_browser_open (const char *domain,
			       chop_store_browser_discovery_handler_t discovery,
			       void *discovery_data,
			       chop_store_browser_removal_handler_t removal,
			       void *removal_data,
			       chop_store_browser_t *browser);

#include <chop/logs.h>

/* If BROWSER is an instance of CHOP_AVAHI_STORE_BROWSER_CLASS, then return
   its log, otherwise return NULL (used for debugging purposes).  */
extern chop_log_t *
chop_avahi_store_browser_log (chop_store_browser_t *browser);


_CHOP_END_DECLS

#endif

/* arch-tag: 3aaf2c18-5655-4f4d-a7bc-90f2b568b747
 */
