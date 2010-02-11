/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

/* Publication of service information on the LAN using multicast, thanks to
   Avahi.  */

/* Based on the `client-publish-service.c' example from Avahi.  */

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/logs.h>
#include <chop/objects.h>
#include <chop/block-server.h>

#include <avahi-client/client.h>
#include <avahi-client/publish.h>

#include <avahi-common/alternative.h>
#include <avahi-common/simple-watch.h>
#include <avahi-common/malloc.h>
#include <avahi-common/error.h>
#include <avahi-common/timeval.h>

#include <stdlib.h>
#include <errno.h>



/* Class declaration.  */

CHOP_DECLARE_RT_CLASS (avahi_store_publisher, store_publisher,
		       chop_log_t       log;
		       AvahiClient     *client;
		       AvahiSimplePoll *poll;
		       AvahiEntryGroup *group;);



/* The base service type name.  This must be augmented with either `._tcp' or
   `._udp'.  */
#define CHOP_AVAHI_SERVICE_BASE_TYPE "_block-server"


static void create_services (AvahiClient *, chop_avahi_store_publisher_t *);

static void
entry_group_callback (AvahiEntryGroup *g, AvahiEntryGroupState state,
		      void *userdata)
{
  chop_avahi_store_publisher_t *avahi;

  avahi = (chop_avahi_store_publisher_t *) userdata;


  /* The callback may be called before the publisher and group are
     created.  */
  if (avahi != NULL)
    if (avahi->group != NULL)
      if (g != avahi->group)
	abort ();

  /* Called whenever the entry group state changes */

  switch (state)
    {
    case AVAHI_ENTRY_GROUP_ESTABLISHED:
      /* The entry group has been established successfully */
      chop_log_printf (&avahi->log,
		       "service `%s' successfully established",
		       avahi->store_publisher.service_name);
      break;

    case AVAHI_ENTRY_GROUP_COLLISION:
      {
	char *new_name;

	/* A service name collision happened. Let's pick a new name */
	new_name =
	  avahi_alternative_service_name (avahi->store_publisher.service_name);
	free (avahi->store_publisher.service_name);
	avahi->store_publisher.service_name = strdup (new_name);
	avahi_free (new_name);

	chop_log_printf (&avahi->log,
			 "service name collision, renaming service to `%s'",
			 avahi->store_publisher.service_name);

	/* And recreate the services */
	create_services (avahi_entry_group_get_client (g),
			 avahi);
	break;
      }

    case AVAHI_ENTRY_GROUP_FAILURE:
      /* Some kind of failure happened while we were registering our
	 services.  */
      avahi_simple_poll_quit (avahi->poll);
      break;

    case AVAHI_ENTRY_GROUP_UNCOMMITED:
    case AVAHI_ENTRY_GROUP_REGISTERING:
      break;

    default:
      /* This should not happen.  */
      chop_log_printf (&avahi->log, "unknown Avahi state (%i)", (int) state);
      abort ();
      break;
    }
}

static void
create_services (AvahiClient *c, chop_avahi_store_publisher_t *avahi)
{
  static const char txt_tls_yes[] = "tls=yes";
  static const char txt_tls_no[] = "tls=no";

  const int protocol_type = IPPROTO_TCP;

  int ret;
  const char *hash_name;
  const char *txt_tls;
  char *txt_hash, *txt_openpgp;
  char *service_type;

  /* If this is the first time we're called, let's create a new entry group */
  if (!avahi->group)
    if (!(avahi->group = avahi_entry_group_new (c, entry_group_callback,
						(void *) avahi)))
      {
	chop_log_printf (&avahi->log,
			 "`avahi_entry_group_new ()' failed: %s",
			 avahi_strerror (avahi_client_errno (c)));
	goto fail;
      }

  /* Construct the service type name.  */
  service_type = (char *)alloca (strlen (CHOP_AVAHI_SERVICE_BASE_TYPE) + 6);
  strcpy (service_type, CHOP_AVAHI_SERVICE_BASE_TYPE);
  strcat (service_type, (protocol_type == IPPROTO_TCP) ? "._tcp" : "._udp");

  /* Prepare `TXT' properties.  */
  hash_name = (avahi->store_publisher.hash_spec.spec_type
	       == CHOP_HASH_SPEC_NONE)
    ? "none"
    : chop_hash_method_name (avahi->store_publisher.hash_spec.method);

  txt_hash = (char *)alloca (strlen ("hash=") + strlen (hash_name) + 1);
  strcpy (txt_hash, "hash=");
  strcat (txt_hash, hash_name);

  txt_tls = (avahi->store_publisher.use_tls) ? txt_tls_yes : txt_tls_no;

  if ((avahi->store_publisher.openpgp_fingerprint != NULL)
      && (avahi->store_publisher.openpgp_fingerprint_size != 0))
    {
      const char *fpr = avahi->store_publisher.openpgp_fingerprint;
      size_t fpr_size = avahi->store_publisher.openpgp_fingerprint_size;

      txt_openpgp =
	(char *) alloca (fpr_size * 2
			 + sizeof ("openpgp-fingerprint=") + 1);
      strcpy (txt_openpgp, "openpgp-fingerprint=");
      chop_buffer_to_hex_string (fpr, fpr_size,
				 &txt_openpgp[strlen (txt_openpgp)]);
    }
  else
    txt_openpgp = NULL;

  /* Add the service.  */
  ret = avahi_entry_group_add_service (avahi->group, AVAHI_IF_UNSPEC,
				       AVAHI_PROTO_UNSPEC, 0,
				       avahi->store_publisher.service_name,
				       service_type,
				       NULL,
				       NULL /* local host */,
				       avahi->store_publisher.port /* port */,

				       /* `TXT' information.  */
				       "implementation=" PACKAGE_STRING,
				       "protocol=SunRPC",
				       "version=" "0" /* RPC interface */,
				       txt_hash,
				       txt_tls,
				       txt_openpgp,

				       NULL);
  if (ret)
    goto fail;


  /* Tell the server to register the service */
  ret = avahi_entry_group_commit (avahi->group);
  if (ret < 0)
    goto fail;

  return;

 fail:
  avahi_simple_poll_quit (avahi->poll);
}

static void
client_callback (AvahiClient *c, AvahiClientState state, void *userdata)
{
  chop_avahi_store_publisher_t *avahi;

  avahi = (chop_avahi_store_publisher_t *) userdata;


  /* Called whenever the client or server state changes */

  switch (state)
    {
    case AVAHI_CLIENT_S_RUNNING:

      /* The server has startup successfully and registered its host
       * name on the network, so it's time to create our services */
      if (!avahi->group)
	create_services (c, avahi);
      break;

    case AVAHI_CLIENT_S_COLLISION:

      /* Let's drop our registered services. When the server is back
       * in AVAHI_SERVER_RUNNING state we will register them
       * again with the new host name. */
      if (avahi->group)
	avahi_entry_group_reset (avahi->group);
      break;

    case AVAHI_CLIENT_FAILURE:

      avahi_simple_poll_quit (avahi->poll);

      break;

    case AVAHI_CLIENT_CONNECTING:
    case AVAHI_CLIENT_S_REGISTERING:
      ;
    }
}



/* Class definition.  */

static errcode_t
avahi_publisher_iterate (chop_store_publisher_t *publisher,
			 unsigned timeout);

static errcode_t
avahi_publisher_loop (chop_store_publisher_t *publisher);


static errcode_t
avahi_ctor (chop_object_t *object, const chop_class_t *class)
{
  int err;
  chop_avahi_store_publisher_t *avahi;

  avahi = (chop_avahi_store_publisher_t *) object;

  avahi->store_publisher.iterate = avahi_publisher_iterate;
  avahi->store_publisher.loop    = avahi_publisher_loop;

  avahi->client = NULL;
  avahi->poll = NULL;
  avahi->group = NULL;

  err = chop_log_init ("avahi-store-publisher", &avahi->log);
  if (err)
    goto fail;

  /* Allocate main loop object */
  avahi->poll = avahi_simple_poll_new ();
  if (!avahi->poll)
    {
      err = CHOP_INVALID_ARG;  /* XXX: Find another error code.  */
      goto fail;
    }

  return 0;

 fail:
  if (avahi->poll)
    avahi_simple_poll_free (avahi->poll);

  return err;
}

static void
avahi_dtor (chop_object_t *object)
{
  chop_avahi_store_publisher_t *avahi;

  avahi = (chop_avahi_store_publisher_t *) object;
  if (avahi->client)
    avahi_client_free (avahi->client);
  if (avahi->poll)
    {
      avahi_simple_poll_quit (avahi->poll);
      avahi_simple_poll_free (avahi->poll);
    }

  chop_object_destroy ((chop_object_t *) &avahi->log);
}

CHOP_DEFINE_RT_CLASS (avahi_store_publisher, store_publisher,
		      avahi_ctor, avahi_dtor,
		      NULL, NULL, /* copy/equal */
		      NULL, NULL  /* serial/deserial */);


/* Methods.  */

static errcode_t
avahi_publisher_iterate (chop_store_publisher_t *publisher,
			 unsigned timeout)
{
  int err;
  chop_avahi_store_publisher_t *avahi;

  avahi = (chop_avahi_store_publisher_t *) publisher;
  err = avahi_simple_poll_iterate (avahi->poll, timeout);
  switch (err)
    {
    case 0:
      break;

    case 1:
      /* A quit request was scheduled.  */
      chop_log_printf (&avahi->log, "iterate: a quit request was "
		       "scheduled");
      break;

    default:
      return CHOP_INVALID_ARG;  /* FIXME */
    }

  return 0;
}

/* Run the main loop */
static errcode_t
avahi_publisher_loop (chop_store_publisher_t *publisher)
{
  int err;
  chop_avahi_store_publisher_t *avahi;

  avahi = (chop_avahi_store_publisher_t *) publisher;
  err = avahi_simple_poll_loop (avahi->poll);

  switch (err)
    {
    case 0:
      break;

    case 1:
      /* A quit request was scheduled.  */
      chop_log_printf (&avahi->log, "loop: a quit request was "
		       "scheduled");
      break;

    default:
      return CHOP_INVALID_ARG;  /* FIXME */
    }

  return 0;
}

errcode_t
chop_avahi_store_publisher_open (const char *service_name,
				 const char *host, unsigned int port,
				 chop_hash_method_spec_t spec,
				 int use_tls,
				 const char *openpgp_fpr,
				 size_t openpgp_fpr_size,
				 chop_store_publisher_t *publisher)
{
  int ret;
  errcode_t err;
  chop_avahi_store_publisher_t *avahi;

  err = chop_object_initialize ((chop_object_t *) publisher,
				&chop_avahi_store_publisher_class);
  if (err)
    return err;

#define INIT_STRING_FIELD(f)			\
  do						\
    {						\
      publisher-> f = strdup (f);		\
      if (!publisher-> f)			\
	{					\
	  err = ENOMEM;				\
	  goto fail;				\
	}					\
    }						\
  while (0)

  INIT_STRING_FIELD (service_name);

  /* If HOST is NULL, the local host name is used.  */
  if (host)
    INIT_STRING_FIELD (host);

#undef INIT_STRING_FIELD

  if ((openpgp_fpr != NULL) && (openpgp_fpr_size != 0))
    {
      publisher->openpgp_fingerprint = malloc (openpgp_fpr_size);
      if (!publisher->openpgp_fingerprint)
	{
	  err = ENOMEM;
	  goto fail;
	}

      memcpy (publisher->openpgp_fingerprint, openpgp_fpr,
	      openpgp_fpr_size);
      publisher->openpgp_fingerprint_size = openpgp_fpr_size;
    }

  publisher->port = port;
  publisher->hash_spec = spec;
  publisher->use_tls = use_tls;

  /* Allocate a new client */
  avahi = (chop_avahi_store_publisher_t *) publisher;
  avahi->client =
    avahi_client_new (avahi_simple_poll_get (avahi->poll), 0,
		      client_callback, (void *) avahi, &ret);

  /* Check wether creating the client object succeeded */
  if (!avahi->client)
    {
      err = CHOP_INVALID_ARG;  /* XXX: Find another error code.  */
      goto fail;
    }

  return 0;

 fail:
  chop_object_destroy ((chop_object_t *) publisher);

  return err;
}

chop_log_t *
chop_avahi_store_publisher_log (chop_store_publisher_t *publisher)
{
  chop_avahi_store_publisher_t *avahi;

  if (!chop_object_is_a ((chop_object_t *)publisher,
			 &chop_avahi_store_publisher_class))
    return NULL;

  avahi = (chop_avahi_store_publisher_t *) publisher;

  return (&avahi->log);
}

/* arch-tag: a1afe464-88c8-4583-b7db-2118475df916
 */
