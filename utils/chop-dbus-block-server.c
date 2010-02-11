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

/* A D-BUS-based block server.

   XXX: This server is far from fully functional and is still quite
   experimental.  Rather use the SunRPC-based server for the real thing.  */


#include <alloca.h>

#include <chop/chop.h>
#include <chop/stores.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>

#include <sys/poll.h>


/* XXX: Until DBus has reached 1.0, we must define this to tell it that we're
   aware that it may change.  */
#define DBUS_API_SUBJECT_TO_CHANGE 1

#include <dbus/dbus.h>


/* Utilities.  */

typedef struct
{
  void **elements;
  size_t element_count;
  size_t max_size;
} pointer_array_t;

#define POINTER_ARRAY_NIL					\
  { .elements = NULL, .element_count = 0, .max_size = 0 };

static pointer_array_t pointer_array_nil = POINTER_ARRAY_NIL;

static pointer_array_t
make_pointer_array (size_t max_size)
{
  pointer_array_t array;

  array.elements = calloc (max_size, sizeof (void *));
  if (!array.elements)
    return pointer_array_nil;

  array.element_count = 0;
  array.max_size = max_size;

  return array;
}

static size_t
pointer_array_size (const pointer_array_t *array)
{
  return array->element_count;
}

static int
pointer_array_is_null (const pointer_array_t *array)
{
  return ((array->elements == NULL)
	  || (array->max_size == 0));
}

static int
pointer_array_add (pointer_array_t *array, void *element)
{
  if (array->element_count >= array->max_size)
    return -1;

  array->elements[array->element_count++] = element;
  return 0;
}

static void
pointer_array_remove (pointer_array_t *array, void *element)
{
  void **p;

  assert (array->element_count > 0);

  for (p = &array->elements[0];
       p - array->elements < array->element_count;
       p++)
    {
      if (*p == element)
	break;
    }

  assert (*p == element);

  array->element_count--;
  if (p != &array->elements[array->element_count])
    /* Move the last element to the former place of ELEMENT.  */
    *p = array->elements[array->element_count];

  array->elements[array->element_count] = NULL;
}

#define foreach(_var, _pa)						\
  {									\
    unsigned _var ## _rank;						\
    for (_var ## _rank = 0, _var = (_pa)->elements [0];			\
	 _var ## _rank < (_pa)->element_count;				\
	 _var ## _rank ++,  _var = (_pa)->elements[_var ## _rank])

#define end_foreach			\
  }


/* Handling D-BUS watches.  */

#define MAX_WATCHES  1024

static pointer_array_t watches = POINTER_ARRAY_NIL;
static int watches_changed = 1;

static dbus_bool_t
handle_watch_addition (DBusWatch *watch, void *data)
{
  printf ("new watch (fd=%i)!\n", dbus_watch_get_fd (watch));

  if (pointer_array_add (&watches, watch))
    return FALSE;

  watches_changed = 1;

  return TRUE;
}

static void
handle_watch_removal (DBusWatch *watch, void *data)
{
  printf ("watch removed! (fd=%i)\n", dbus_watch_get_fd (watch));
  pointer_array_remove (&watches, watch);
  watches_changed = 1;
}

static void
handle_watch_toggle (DBusWatch *watch, void *data)
{
  printf ("watch toggled! (fd=%i)\n", dbus_watch_get_fd (watch));
  watches_changed = 1;
}


/* Handling method calls and signals.  */

/* Deallocate the resources associated to the client that was linked via
   CONNECTION.  */
static void
handle_object_unregistration (DBusConnection  *connection,
			      void            *user_data)
{
  char *client_name = (char *)user_data;

  assert (client_name);
  printf ("client `%s' has left\n", client_name);
  free (client_name);
}

static DBusHandlerResult
handle_method_call (DBusConnection  *connection,
		    DBusMessage     *message,
		    void            *user_data)
{
  fprintf (stdout, "%s: unhandled call: %s\n",
	   __FUNCTION__, dbus_message_get_member (message));

  return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusHandlerResult
handle_client_signals (DBusConnection *connection,
		       DBusMessage    *message,
		       void           *data)
{
  DBusHandlerResult result;
  char *client_name = (char *)data;

  if (dbus_message_is_signal (message,
			      DBUS_INTERFACE_LOCAL, "Disconnected"))
    {
      printf ("connection with client `%s' lost\n", client_name);
      dbus_connection_unregister_object_path (connection, client_name);
      result = DBUS_HANDLER_RESULT_HANDLED;
      dbus_message_unref (message);
    }

  return result;
}

static DBusHandlerResult
handle_ctor_message (DBusConnection  *connection,
		     DBusMessage     *message,
		     void            *user_data)
{
  DBusHandlerResult result = DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
  DBusMessage *reply = NULL;

  printf ("handling message!\n");

  if (!dbus_message_get_type (message) == DBUS_MESSAGE_TYPE_METHOD_CALL)
    {
      /* Drop the connection.  */
      result = DBUS_HANDLER_RESULT_HANDLED;
      goto close_connection;
    }

  if (strcmp (dbus_message_get_interface (message), "fr.laas.BlockStore"))
    {
      fprintf (stderr, "wrong interface: %s\n",
	       dbus_message_get_interface (message));
      result = DBUS_HANDLER_RESULT_HANDLED;
      goto close_connection;
    }

  fprintf (stdout, "path: %s\n", dbus_message_get_path (message));

  {
    static char goodsig[] = { DBUS_MESSAGE_TYPE_INVALID };

    if ((!strcmp (dbus_message_get_member (message), "NewClient"))
	&& (dbus_message_has_signature (message, goodsig)))
      {
	/* The constructor.  */
	static const DBusObjectPathVTable object_vtable =
	  {
	    .unregister_function = handle_object_unregistration,
	    .message_function = handle_method_call
	  };
	char client_name[1024], *c;
	unsigned long int number = random ();

	/* Hyphens are not allowed as part of an object path.  */
	snprintf (client_name, sizeof (client_name),
		  "/fr/laas/BlockStore/Client%0lx", number);
	c = client_name;

	reply = dbus_message_new_method_return (message);
	if (!dbus_message_append_args (reply, DBUS_TYPE_OBJECT_PATH,
				       &c, DBUS_TYPE_INVALID))
	  {
	    result = DBUS_HANDLER_RESULT_NEED_MEMORY;
	    goto finish;
	  }

	if (!dbus_connection_send (connection, reply, NULL))
	  {
	    result = DBUS_HANDLER_RESULT_NEED_MEMORY;
	    goto finish;
	  }


	/* Prepare CONNECTION to handle method calls to the object named by
	   CLIENT_NAME and signals.  */
	dbus_connection_register_object_path (connection, client_name,
					      &object_vtable,
					      strdup (client_name));

/* 	dbus_connection_add_filter (connection, handle_client_signals, */
/* 				    strdup (client_name), free); */

	printf ("new client `%s'\n", client_name);

	result = DBUS_HANDLER_RESULT_HANDLED;
	goto finish;
      }
  }

  fprintf (stdout, "unhandled ctor call: %s\n",
	   dbus_message_get_member (message));

 close_connection:
  dbus_connection_close (connection);
  dbus_connection_unref (connection);

 finish:

  return result;
}


/* Connection management.  */

/* Currently live connections.  */
#define MAX_CONNECTIONS   10
static pointer_array_t connections = POINTER_ARRAY_NIL;

static int
add_connection (DBusConnection *c)
{
  return pointer_array_add (&connections, c);
}

static void
rm_connection (DBusConnection *c)
{
  pointer_array_remove (&connections, c);

  dbus_connection_close (c);
  dbus_connection_unref (c);

  printf ("just dropped connection %p\n", c);
}

static void
handle_dispatch_status_change (DBusConnection *connection,
			       DBusDispatchStatus new_status,
			       void           *data)
{
  /* XXX: This function actually only gets called on
     `dbus_connection_close ()' it seems.  However, when we get to that
     point, we don't really care about the remaining messages so we just drop
     them happily.  */
  if (new_status == DBUS_DISPATCH_DATA_REMAINS)
    printf ("connection %p has remaining data\n", connection);
}

/* Handle new connection CONNECTION on server SERVER.  More precisely,
   prepare CONNECTION to handle method calls on its constructor.  */
static void
handle_new_connection (DBusServer     *server,
		       DBusConnection *connection,
		       void           *data)
{
  static const DBusObjectPathVTable ctor_vtable =
    {
      .unregister_function = NULL,
      .message_function = handle_ctor_message
    };


  printf ("new connection!\n");

  /* Do what's necessary to monitor the connection's activity.  */
  dbus_connection_set_watch_functions (connection,
				       handle_watch_addition,
				       handle_watch_removal,
				       handle_watch_toggle,
				       NULL, NULL);

  dbus_connection_set_dispatch_status_function (connection,
						handle_dispatch_status_change,
						NULL, NULL);

  /* XXX: We should add option to check the originator of the connection and
     only keep it if it matches the user's requirements.  */
  dbus_connection_register_object_path (connection, "/constructor",
					&ctor_vtable, NULL);
/*   dbus_connection_add_filter (connection, handle_client_signals, */
/* 			      "server", NULL); */

  /* Protect ourselves from DoS attacks.  */
  dbus_connection_set_max_message_size (connection, 16384);

  /* Keep track of this connection.  */
  dbus_connection_ref (connection);
  add_connection (connection);
}


/* Converting from D-BUS watch flags to `poll ()' events.  */

static void
print_watch_flags (unsigned int flags)
{
  if (flags & DBUS_WATCH_READABLE)
    printf ("READABLE | ");
  if (flags & DBUS_WATCH_WRITABLE)
    printf ("WRITABLE | ");
  if (flags & DBUS_WATCH_ERROR)
    printf ("ERROR | ");
  if (flags & DBUS_WATCH_HANGUP)
    printf ("HANGUP | ");

  printf ("0\n");
}

static int
watch_flags_to_poll (unsigned int flags)
{
  register int ret = 0;

  if (flags & DBUS_WATCH_READABLE)
    ret |= POLLIN;
  if (flags & DBUS_WATCH_WRITABLE)
    ret |= POLLOUT;

  return ret;
}

static unsigned int
poll_flags_to_watch (int flags)
{
  register unsigned int ret = 0;

  if ((flags & POLLIN) || (flags & POLLPRI))
    ret |= DBUS_WATCH_READABLE;
  if (flags & POLLOUT)
    ret |= DBUS_WATCH_WRITABLE;
  if (flags & POLLERR)
    ret |= DBUS_WATCH_ERROR;
  if (flags & POLLHUP)
    ret |= DBUS_WATCH_HANGUP;

  assert (ret != 0);
  print_watch_flags (ret);

  return ret;
}


/* The guts of the server.  */

int
main (int argc, char *argv[])
{
  DBusError derr;
  DBusServer *server;
  struct pollfd pollfds[1024];

  chop_init ();

  watches = make_pointer_array (MAX_WATCHES);
  connections = make_pointer_array (MAX_CONNECTIONS);
  assert (!pointer_array_is_null (&watches));
  assert (!pointer_array_is_null (&connections));

  dbus_error_init (&derr);


  /* Set up the server.  */

  server = dbus_server_listen ("tcp:host=localhost,port=7777", &derr);
  if (!server)
    {
      fprintf (stderr, "%s: DBus server error: %s: %s\n",
	       argv[0], derr.name, derr.message);
      dbus_error_free (&derr);
      return 1;
    }

  dbus_server_set_new_connection_function (server, handle_new_connection,
					   NULL, NULL);
  dbus_server_set_watch_functions (server,
				   handle_watch_addition,
				   handle_watch_removal,
				   handle_watch_toggle,
				   server, NULL);

#if 0 /* Debugging */
  {
    DBusConnection *bus;
    bus = dbus_bus_get (DBUS_BUS_SESSION, &derr);
    if (bus)
      handle_new_connection (NULL, bus, &bus);
  }
#endif

  /* The event loop: poll for events on the server's and connections'
     active watches.  Upon data availability, process these data (e.g.,
     handle method calls).  */
  while (1)
    {
      int events;
      unsigned poll_count = 0;

      if (watches_changed)
	{
	  /* Update POLLFDS.  */
	  /* FIXME: We should create an array that maps entries in POLLFDS to
	     watches for faster lookup.  */
	  DBusWatch *w;

	  poll_count = 0;
	  foreach (w, &watches)
	    {
	      if (dbus_watch_get_enabled (w))
		{
		  pollfds[poll_count].fd = dbus_watch_get_fd (w);
		  pollfds[poll_count].events =
		    watch_flags_to_poll (dbus_watch_get_flags (w));
		  poll_count++;
		}
	    }
	  end_foreach

	  watches_changed = 0;
	}

      if (poll_count)
	{
	  printf ("polling %u descriptors, %u connections\n",
		  poll_count, connections.element_count);
	  events = poll (pollfds, poll_count, -1);
	  if (events < 0)
	    {
	      if (errno == EINTR)
		continue;

	      com_err (argv[0], errno, "poll");
	    }
	  else
	    {
	      unsigned i, confirmed = 0;

	      printf ("%i events!\n", events);

	      for (i = 0; i < poll_count; i++)
		{
		  if (pollfds[i].revents)
		    {
		      DBusWatch *w;
		      int found = 0;

		      foreach (w, &watches)
			{
			  if (dbus_watch_get_fd (w) == pollfds[i].fd)
			    {
			      found = 1;
			      break;
			    }
			}
		      end_foreach

		      assert (found);
		      dbus_watch_handle (w,
					 poll_flags_to_watch (pollfds[i].revents));
		      pollfds[i].revents = 0;
		      confirmed++;
		    }
		}

	      assert (events == confirmed);
	    }
	}


      {
	/* Now that data is available, process data on each and every
	   connection we have.  At the same time, fill up an array with
	   connections that have become disconnected.  */
	DBusConnection *c;
	DBusConnection **disconnections;
	size_t disconnection_count = 0;

	disconnections =
	  (DBusConnection **)alloca (pointer_array_size (&connections));
	disconnections[0] = NULL;

	foreach (c, &connections)
	  {
	    printf ("dispatching connection %p\n", c);
	    switch (dbus_connection_get_dispatch_status (c))
	      {
	      case DBUS_DISPATCH_DATA_REMAINS:
		dbus_connection_dispatch (c);
		break;

	      case DBUS_DISPATCH_COMPLETE:
		dbus_connection_flush (c);
		dbus_connection_read_write_dispatch (c, 0);
		break;

	      case DBUS_DISPATCH_NEED_MEMORY:
		/* FIXME: We should handle DoS attacks, we should drop the
		   most resource-demanding connection or something like
		   that.  */

	      default:
		fprintf (stderr, "unhandled dispatch status for %p\n", c);
		abort ();
	      }
	    printf ("end of dispatch %p\n", c);

	    if (!dbus_connection_get_is_connected (c))
	      disconnections[disconnection_count++] = c;
	  }
	end_foreach

	{
	  /* Handle disconnections.  */
	  size_t i;

	  for (i = 0; i < disconnection_count; i++)
	    {
	      DBusConnection *c = disconnections[i];
	      printf ("connection %p got disconnected\n", c);
	      rm_connection (c);
	    }
	}
      }
    }

  return 1;
}

/* arch-tag: 99b857bb-0c66-469d-b4a2-b6e7eca5c74a
 */
