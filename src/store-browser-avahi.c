/* Avahi-based store browser.  This allows services to be discovered on a
   LAN.  In case Avahi is not present, this file is still compiled but a stub
   is provided instead of the real functions.  */

/* Based on `client-browse-services.c' in Avahi.  */

#include <chop/chop.h>
#include <chop/objects.h>
#include <chop/store-browsers.h>

#include <chop/logs.h>
#include <chop/stores.h>
#include <chop/chop-config.h>


#ifdef HAVE_AVAHI
# include <avahi-client/client.h>
# include <avahi-client/lookup.h>

# include <avahi-common/simple-watch.h>
# include <avahi-common/malloc.h>
# include <avahi-common/error.h>
#endif



CHOP_DECLARE_RT_CLASS (avahi_store_browser, store_browser,
		       chop_log_t           log;
#ifdef HAVE_AVAHI
		       AvahiSimplePoll     *simple_poll;
		       AvahiClient         *client;
		       AvahiServiceBrowser *sb;
#endif
		       );

static errcode_t
asb_ctor (chop_object_t *object, const chop_class_t *class)
{
#ifdef HAVE_AVAHI
  chop_avahi_store_browser_t *avahi = (chop_avahi_store_browser_t *)object;

  avahi->simple_poll = NULL;
  avahi->client = NULL;
  avahi->sb = NULL;

  return (chop_log_init ("avahi-store-browser", &avahi->log));
#else
  return CHOP_ERR_NOT_IMPL;
#endif
}

static void
asb_dtor (chop_object_t *object)
{
  chop_avahi_store_browser_t *avahi = (chop_avahi_store_browser_t *)object;

#ifdef HAVE_AVAHI
  if (avahi->sb)
    {
      avahi_service_browser_free (avahi->sb);
      avahi->sb = NULL;
    }

  if (avahi->client)
    {
      avahi_client_free (avahi->client);
      avahi->client = NULL;
    }

  if (avahi->simple_poll)
    {
      avahi_simple_poll_free (avahi->simple_poll);
      avahi->simple_poll = NULL;
    }
#endif

  chop_object_destroy ((chop_object_t *)&avahi->log);
}

CHOP_DEFINE_RT_CLASS (avahi_store_browser, store_browser,
		      asb_ctor, asb_dtor, /* ctor/dtor */
		      NULL, NULL,         /* copy/equal */
		      NULL, NULL          /* serial/deserial */);


chop_log_t *
chop_avahi_store_browser_log (chop_store_browser_t *browser)
{
  if (chop_object_is_a ((chop_object_t *)browser,
			&chop_avahi_store_browser_class))
    {
      chop_avahi_store_browser_t *avahi;
      avahi = (chop_avahi_store_browser_t *)browser;
      return &avahi->log;
    }

  return NULL;
}



#ifdef HAVE_AVAHI

#include <assert.h>
#include <stdlib.h>


static void
resolve_callback (AvahiServiceResolver *r,
		  AvahiIfIndex interface,
		  AvahiProtocol protocol,
		  AvahiResolverEvent event,
		  const char *name,
		  const char *type,
		  const char *domain,
		  const char *host_name,
		  const AvahiAddress *address,
		  uint16_t port,
		  AvahiStringList *txt,
		  AvahiLookupResultFlags flags,
		  void* userdata)
{
  int quit = 0;
  chop_avahi_store_browser_t *avahi = (chop_avahi_store_browser_t *)userdata;

  assert (r);

  /* Called whenever a service has been resolved successfully or timed out */

  switch (event)
    {
    case AVAHI_RESOLVER_FAILURE:
      chop_log_printf (&avahi->log, "failed to resolve service `%s' of "
		       "type `%s' in domain `%s': %s", name, type,
		       domain,
		       avahi_strerror (avahi_client_errno
				       (avahi_service_resolver_get_client (r))));
      break;

    case AVAHI_RESOLVER_FOUND:
      {
	char a[AVAHI_ADDRESS_STR_MAX];
	chop_hash_method_spec_t spec;
	const chop_class_t *client_class = NULL;
	AvahiStringList *hash, *protocol;

	chop_log_printf (&avahi->log,
			 "resolved service `%s' of type `%s' in domain `%s'",
			 name, type, domain);

	avahi_address_snprint (a, sizeof (a), address);

	/* Decoding the `TXT' entries.  This should be the same as what's
	   done on the service publication side, namely in
	   `chop-block-server.c'.  */

	/* Hash specification.  */
	hash = avahi_string_list_find (txt, "hash");
	if (hash)
	  {
	    char *key, *value;

	    avahi_string_list_get_pair (hash, &key, &value, NULL);
	    if (key)
	      avahi_free (key);
	    if (value)
	      {
		spec = chop_read_hash_method_spec (value);
		avahi_free (value);
	      }
	    else
	      spec.spec_type = CHOP_HASH_SPEC_NONE;
	  }
	else
	  spec.spec_type = CHOP_HASH_SPEC_NONE;

	/* Protocol type (either `SunRPC' or `DBus').  */
	protocol = avahi_string_list_find (txt, "protocol");
	if (protocol)
	  {
	    char *key, *value;

	    avahi_string_list_get_pair (protocol, &key, &value, NULL);
	    if (key)
	      avahi_free (key);
	    if (value)
	      {
		if (!strcmp (value, "SunRPC"))
		  client_class = &chop_sunrpc_block_store_class;
		else if (!strcmp (value, "DBus"))
		  client_class = &chop_dbus_block_store_class;
		avahi_free (value);
	      }
	  }

	quit = avahi->store_browser.discovery ((chop_store_browser_t *)avahi,
					       name, a, port,
					       spec,
					       client_class,
					       avahi->store_browser.discovery_data);

      }
    }

  avahi_service_resolver_free(r);

  if (quit)
    avahi_simple_poll_quit (avahi->simple_poll);
}

static void
browse_callback (AvahiServiceBrowser *b,
		 AvahiIfIndex interface,
		 AvahiProtocol protocol,
		 AvahiBrowserEvent event,
		 const char *name,
		 const char *type,
		 const char *domain,
		 AvahiLookupResultFlags flags,
		 void* userdata)
{
  chop_avahi_store_browser_t *avahi = (chop_avahi_store_browser_t *)userdata;
  AvahiClient *c = avahi->client;

  assert(b);

  /* Called whenever a new service becomes available on the LAN or is removed
     from the LAN.  */

  switch (event)
    {
    case AVAHI_BROWSER_FAILURE:

      chop_log_printf (&avahi->log, "(browser) error: %s",
		       avahi_strerror (avahi_client_errno
				       (avahi_service_browser_get_client (b))));
      avahi_simple_poll_quit (avahi->simple_poll);
      return;

    case AVAHI_BROWSER_NEW:
      chop_log_printf (&avahi->log,
		       "new service: `%s' of type `%s' in domain `%s'",
		       name, type, domain);

      /* We ignore the returned resolver object.  In the callback function we
	 free it.  If the server is terminated before the callback function
	 is called the server will free the resolver for us.  */

      if (!(avahi_service_resolver_new (c, interface, protocol,
					name, type, domain,
					AVAHI_PROTO_UNSPEC,
					0, /* AVAHI_LOOKUP_USE_MULTICAST, */
					resolve_callback, avahi)))
	chop_log_printf (&avahi->log, "failed to resolve service `%s': %s",
			 name, avahi_strerror (avahi_client_errno (c)));

      break;

    case AVAHI_BROWSER_REMOVE:
      {
	int quit;

	quit = avahi->store_browser.removal ((chop_store_browser_t *)avahi,
					     name,
					     avahi->store_browser.removal_data);
	if (quit)
	  avahi_simple_poll_quit (avahi->simple_poll);
      }
      break;

    case AVAHI_BROWSER_ALL_FOR_NOW:
    case AVAHI_BROWSER_CACHE_EXHAUSTED:
      chop_log_printf (&avahi->log, "(browser) %s",
		       event == AVAHI_BROWSER_CACHE_EXHAUSTED
		       ? "CACHE_EXHAUSTED" : "ALL_FOR_NOW");
      break;
    }
}

static void
client_callback (AvahiClient *c, AvahiClientState state,
		 void *userdata)
{
  chop_avahi_store_browser_t *avahi;

  avahi = (chop_avahi_store_browser_t *)userdata;

  /* Called whenever the client or server state changes.  */

  switch (state)
    {
    case AVAHI_CLIENT_FAILURE:
      chop_log_printf (&avahi->log, "server connection failre: %s",
		       avahi_strerror (avahi_client_errno (c)));
      avahi_simple_poll_quit (avahi->simple_poll);
      break;

    case AVAHI_CLIENT_CONNECTING:
      chop_log_printf (&avahi->log, "client connecting");
      break;

    case AVAHI_CLIENT_S_RUNNING:
      chop_log_printf (&avahi->log, "client running");
      break;

    default:
      chop_log_printf (&avahi->log, "client is in unknown state %i",
		       (int)state);
    }
}

static errcode_t
asb_iterate (chop_store_browser_t *browser, unsigned timeout)
{
  int err;
  chop_avahi_store_browser_t *avahi = (chop_avahi_store_browser_t *)browser;

  chop_log_printf (&avahi->log, "iterating for %u msecs", timeout);
  err = avahi_simple_poll_iterate (avahi->simple_poll, timeout);
  switch (err)
    {
    case 0:
      break;

    case 1:
      /* A quit request was scheduled.  */
      break;

    default:
      return CHOP_INVALID_ARG;  /* FIXME */
    }

  return 0;
}

static errcode_t
asb_loop (chop_store_browser_t *browser)
{
  int err;
  chop_avahi_store_browser_t *avahi = (chop_avahi_store_browser_t *)browser;

  err = avahi_simple_poll_loop (avahi->simple_poll);
  switch (err)
    {
    case 0:
      break;

    case 1:
      /* A quit request was scheduled.  */
      break;

    default:
      return CHOP_INVALID_ARG;  /* FIXME */
    }

  return 0;
}


errcode_t
chop_avahi_store_browser_open (const char *domain,
			       chop_store_browser_discovery_handler_t discovery,
			       void *discovery_data,
			       chop_store_browser_removal_handler_t removal,
			       void *removal_data,
			       chop_store_browser_t *browser)
{
  chop_avahi_store_browser_t *avahi;

  avahi = (chop_avahi_store_browser_t *)browser;
  int error;
  errcode_t ret = CHOP_INVALID_ARG;

  ret = chop_object_initialize ((chop_object_t *)avahi,
				&chop_avahi_store_browser_class);
  if (ret)
    return ret;

  ret = CHOP_INVALID_ARG;  /* FIXME */

  /* Allocate main loop object */
  if (!(avahi->simple_poll = avahi_simple_poll_new()))
    {
      chop_log_printf (&avahi->log, "failed to create simple poll object");
      goto fail;
    }

  /* Allocate a new client */
  avahi->client = avahi_client_new (avahi_simple_poll_get (avahi->simple_poll),
				    AVAHI_CLIENT_NO_FAIL,
				    client_callback, avahi, &error);

  /* Check wether creating the client object succeeded */
  if (!avahi->client)
    {
      chop_log_printf (&avahi->log, "failed to create client: %s",
		       avahi_strerror (error));
      goto fail;
    }

  /* Create the service browser */
  avahi->sb = avahi_service_browser_new (avahi->client, AVAHI_IF_UNSPEC,
					 AVAHI_PROTO_UNSPEC,
					 "_block-server._tcp",
					 domain,
					 0, /* AVAHI_LOOKUP_USE_MULTICAST, */
					 browse_callback,
					 avahi);
  if (!avahi->sb)
    {
      chop_log_printf (&avahi->log, "failed to create service browser: %s",
		       avahi_strerror (avahi_client_errno (avahi->client)));
      goto fail;
    }

  ret = 0;
  goto leave;

 fail:

  /* Cleanup things.  */
  chop_object_destroy ((chop_object_t *)avahi);

  return ret;

 leave:
  avahi->store_browser.loop = asb_loop;
  avahi->store_browser.iterate = asb_iterate;
  avahi->store_browser.discovery_data = discovery_data;
  avahi->store_browser.removal_data = removal_data;
  avahi->store_browser.discovery = discovery;
  avahi->store_browser.removal = removal;

  return ret;
}


#else /* !HAVE_AVAHI */

errcode_t
chop_avahi_store_browser_open (const char *domain,
			       chop_store_browser_discovery_handler_t d,
			       void *discovery_data,
			       chop_store_browser_removal_handler_t r,
			       void *removal_data,
			       chop_store_browser_t *browser)
{
  return CHOP_ERR_NOT_IMPL;
}

#endif /* HAVE_AVAHI */

/* arch-tag: b1518f68-34d5-40aa-bc2c-1e7b40db1f8e
 */
