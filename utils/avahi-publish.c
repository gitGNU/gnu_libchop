/* Publication of service information on the LAN using multicast, thanks to
   Avahi.  */

#ifndef USE_AVAHI
# error "This file must be included from `chop-block-server.c'."
#endif

/* Based on the `client-publish-service.c' example from Avahi.  */

#include <avahi-client/client.h>
#include <avahi-client/publish.h>

#include <avahi-common/alternative.h>
#include <avahi-common/simple-watch.h>
#include <avahi-common/malloc.h>
#include <avahi-common/error.h>
#include <avahi-common/timeval.h>

#include <unistd.h>  /* `gethostname' */
#include <stdio.h>   /* `cuserid' */
#include <assert.h>


/* The base service type name.  This must be augmented with either `._tcp' or
   `._udp'.  */
#define CHOP_AVAHI_SERVICE_BASE_TYPE "_block-server"


static AvahiEntryGroup *group = NULL;
static AvahiSimplePoll *simple_poll = NULL;

static void create_services (AvahiClient *c);

static void
entry_group_callback (AvahiEntryGroup *g, AvahiEntryGroupState state,
		      void *userdata)
{
  assert (g == group);

  /* Called whenever the entry group state changes */

  switch (state)
    {
    case AVAHI_ENTRY_GROUP_ESTABLISHED:
      /* The entry group has been established successfully */
      info ("service `%s' successfully established", service_name);
      break;

    case AVAHI_ENTRY_GROUP_COLLISION:
      {
	char *n;

	/* A service name collision happened. Let's pick a new name */
	n = avahi_alternative_service_name (service_name);
	avahi_free (service_name);
	service_name = n;

	info ("service name collision, renaming service to `%s'",
	      service_name);

	/* And recreate the services */
	create_services (avahi_entry_group_get_client (g));
	break;
      }

    case AVAHI_ENTRY_GROUP_FAILURE:
      /* Some kind of failure happened while we were registering our
	 services */
      avahi_simple_poll_quit (simple_poll);
      break;

    case AVAHI_ENTRY_GROUP_UNCOMMITED:
    case AVAHI_ENTRY_GROUP_REGISTERING:
      break;

    default:
      info ("unknown Avahi state (%i)", (int)state);
      break;
    }
}

static void
create_services (AvahiClient *c)
{
  int ret;
  const char *hash_name;
  char *txt_hash;
  char *service_type;

  assert(c);

  /* If this is the first time we're called, let's create a new entry group */
  if (!group)
    if (!(group = avahi_entry_group_new (c, entry_group_callback, NULL)))
      {
	info ("avahi_entry_group_new() failed: %s",
	      avahi_strerror (avahi_client_errno (c)));
	goto fail;
      }

  if (!service_name)
    {
      /* Choose a service name of the form `user@host'.  */
      char *name, *host;
      size_t total_size;
#ifdef HAVE_CUSERID
      char *at;

      total_size = L_cuserid + 1024 + 2;
      name = (char *)alloca (total_size);
      cuserid (name);
      at = name + strlen (name);
      host = at + 1;

      *at = '@';
#else
      host = name = (char *)alloca (1024);
      total_size = 1024;
#endif

      if (gethostname (host, total_size -1 - ((size_t)(host - name))))
	strcpy  (host, "anonymous-block-server");
      else
	/* Just to make sure...  */
	name[total_size - 1] = '\0';

      service_name = avahi_strdup (name);
    }

  info ("adding service `%s'", service_name);

  /* Construct the service type name.  */
  service_type = (char *)alloca (strlen (CHOP_AVAHI_SERVICE_BASE_TYPE) + 6);
  strcpy (service_type, CHOP_AVAHI_SERVICE_BASE_TYPE);
  strcat (service_type, (protocol_type == IPPROTO_TCP) ? "._tcp" : "._udp");

  /* Prepare `TXT' properties.  */
  hash_name = (content_hash_enforced == CHOP_HASH_NONE)
    ? "none" : chop_hash_method_name (content_hash_enforced);
  txt_hash = (char *)alloca (strlen ("hash=") + strlen (hash_name) + 1);
  strcpy (txt_hash, "hash=");
  strcat (txt_hash, hash_name);

  /* Add the service.  */
  ret = avahi_entry_group_add_service (group, AVAHI_IF_UNSPEC,
				       AVAHI_PROTO_UNSPEC, 0, service_name,
				       service_type,
				       NULL,
				       NULL /* local host */,
				       service_port /* port */,

				       /* `TXT' information.  */
				       "implementation=" PACKAGE_STRING,
				       "protocol=SunRPC",
				       "version=" "0" /* RPC interface */,
				       txt_hash,

				       NULL);
  if (ret)
    {
      info ("failed to add `%s' service: %s", service_type,
	    avahi_strerror (ret));
      goto fail;
    }


  /* Tell the server to register the service */
  if ((ret = avahi_entry_group_commit(group)) < 0)
    {
      info ("failed to commit entry_group: %s", avahi_strerror (ret));
      goto fail;
    }

  info ("registered service of type `%s'", service_type);

  return;

 fail:
  avahi_simple_poll_quit (simple_poll);
}

static void
client_callback (AvahiClient *c, AvahiClientState state, void *userdata)
{
  assert(c);

  /* Called whenever the client or server state changes */

  switch (state)
    {
    case AVAHI_CLIENT_S_RUNNING:

      /* The server has startup successfully and registered its host
       * name on the network, so it's time to create our services */
      if (!group)
	create_services (c);
      break;

    case AVAHI_CLIENT_S_COLLISION:

      /* Let's drop our registered services. When the server is back
       * in AVAHI_SERVER_RUNNING state we will register them
       * again with the new host name. */
      if (group)
	avahi_entry_group_reset (group);
      break;

    case AVAHI_CLIENT_FAILURE:

      info ("client failure: %s", avahi_strerror (avahi_client_errno (c)));
      avahi_simple_poll_quit (simple_poll);

      break;

    case AVAHI_CLIENT_CONNECTING:
    case AVAHI_CLIENT_S_REGISTERING:
      ;
    }
}


static void *
avahi_thread_entry_point (void *unused)
{
  AvahiClient *client = NULL;
  int error;

  /* Allocate main loop object */
  if (!(simple_poll = avahi_simple_poll_new ()))
    {
      info ("failed to create simple poll object");
      goto fail;
    }

  /* Allocate a new client */
  client = avahi_client_new (avahi_simple_poll_get (simple_poll), 0,
			     client_callback, NULL, &error);

  /* Check wether creating the client object succeeded */
  if (!client)
    {
      info ("failed to create client: %s", avahi_strerror (error));
      goto fail;
    }

  /* Run the main loop */
  avahi_simple_poll_loop(simple_poll);

 fail:
  exit (1);

  return NULL;
}

/* arch-tag: a1afe464-88c8-4583-b7db-2118475df916
 */
