#ifndef USE_AVAHI
# error "This file must be included from `chop-block-server.c'."
#endif

/* This file publishes service information on the LAN using multicast, thanks
   to Avahi.  */

/* Based on the `client-publish-service.c' example from Avahi.  */

#include <avahi-client/client.h>
#include <avahi-client/publish.h>

#include <avahi-common/alternative.h>
#include <avahi-common/simple-watch.h>
#include <avahi-common/malloc.h>
#include <avahi-common/error.h>
#include <avahi-common/timeval.h>

#define SERVICE_TYPE "_sunrpc-block-server._tcp"
#define SERVICE_NAME "block-server"

static AvahiEntryGroup *group = NULL;
static AvahiSimplePoll *simple_poll = NULL;
static char *service_name = NULL;

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
      fprintf (stderr, "Service `%s' successfully established.\n",
	       service_name);
      break;

    case AVAHI_ENTRY_GROUP_COLLISION:
      {
	char *n;

	/* A service name collision happened. Let's pick a new name */
	n = avahi_alternative_service_name (service_name);
	avahi_free (service_name);
	service_name = n;

	fprintf (stderr, "Service name collision, renaming service to `%s'\n",
		 service_name);

	/* And recreate the services */
	create_services (avahi_entry_group_get_client(g));
	break;
      }

    case AVAHI_ENTRY_GROUP_FAILURE:
      /* Some kind of failure happened while we were registering our
	 services */
      avahi_simple_poll_quit (simple_poll);
      break;

    case AVAHI_ENTRY_GROUP_UNCOMMITED:
    case AVAHI_ENTRY_GROUP_REGISTERING:
      ;
    }
}

static void
create_services (AvahiClient *c)
{
  int ret;

  assert(c);

  /* If this is the first time we're called, let's create a new entry group */
  if (!group)
    if (!(group = avahi_entry_group_new (c, entry_group_callback, NULL)))
      {
	fprintf (stderr, "avahi_entry_group_new() failed: %s\n",
		 avahi_strerror(avahi_client_errno(c)));
	goto fail;
      }

  service_name = avahi_strdup (SERVICE_NAME);
  fprintf(stderr, "adding service `%s'\n", service_name);

  /* Add the service.  */
  ret = avahi_entry_group_add_service (group, AVAHI_IF_UNSPEC,
				       AVAHI_PROTO_UNSPEC, 0, service_name,
				       SERVICE_TYPE, NULL,
				       NULL /* local host */,
				       111 /* port: SunRPC */,

				       /* `TXT' information.  */
				       "implementation=" PACKAGE_NAME,
				       "version=" PACKAGE_STRING,

				       /* TODO/FIXME: Add hash information,
					  etc.  */

				       NULL);
  if (ret)
    {
      fprintf (stderr, "failed to add `"SERVICE_TYPE"' service: %s\n",
	       avahi_strerror(ret));
      goto fail;
    }


  /* Tell the server to register the service */
  if ((ret = avahi_entry_group_commit(group)) < 0)
    {
      fprintf (stderr, "failed to commit entry_group: %s\n",
	       avahi_strerror (ret));
      goto fail;
    }

  fprintf (stderr, "registered service of type `"SERVICE_TYPE"'\n");

  return;

 fail:
  avahi_simple_poll_quit(simple_poll);
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

      fprintf (stderr, "client failure: %s\n",
	       avahi_strerror (avahi_client_errno (c)));
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
      fprintf(stderr, "Failed to create simple poll object.\n");
      goto fail;
    }

  /* Allocate a new client */
  client = avahi_client_new (avahi_simple_poll_get (simple_poll), 0,
			     client_callback, NULL, &error);

  /* Check wether creating the client object succeeded */
  if (!client)
    {
      fprintf(stderr, "Failed to create client: %s\n", avahi_strerror (error));
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
