/* XXX FIXME TODO

   This is just a stub.  */

#include <chop/chop.h>
#include <chop/stores.h>

#include <stdio.h>
#include <errno.h>
#include <assert.h>

/* XXX: Until DBus has reached 1.0, we must define this to tell it that we're
   aware that it may change.  */
#define DBUS_API_SUBJECT_TO_CHANGE 1

#include <dbus/dbus.h>


#define MAX_WATCHES  1024
static DBusWatch *watches[MAX_WATCHES];
static unsigned watch_count = 0;

static dbus_bool_t
handle_watch_addition (DBusWatch *watch, void *data)
{
  if (watch_count >= MAX_WATCHES)
    return FALSE;

  watches[watch_count++] = watch;
  return TRUE;
}

static dbus_bool_t
handle_watch_removal (DBusWatch *watch, void *data)
{
  DBusWatch **p;

  for (p = &watches[0];
       p - watches < watch_count;
       p++)
    {
      if (*p == watch)
	break;
    }

  assert (*p == watch);

  watch_count--;
  if (p != &watches[watch_count])
    *p = watches[watch_count];

  return TRUE;
}

static dbus_bool_t
handle_watch_toggle (DBusWatch *watch, void *data)
{
  return TRUE;
}


static void
handle_message ()
{
}

static void
handle_new_connection (DBusConnection *connection,
		       void *data)
{
  static const DBusObjectPathVTable vtable =
    {
      .unregister_function = NULL,
      .message_function = handle_message
    };

  /* XXX: We should add option to check the originator of the connection and
     only keep it if it matches the user's requirements.  */
  dbus_connection_ref (connection);

  dbus_connection_register_object_path (connection, "/store",
					&vtable, NULL);
  dbus_connection_set_watch_functions (connection, add, rm, to,
				       NULL, NULL);
}


int
main (int argc, char *argv[])
{
  DBusError derr;
  DBusServer *server;

  dbus_error_init (&derr);
  server = dbus_server_listen ("tcp:host=localhost,port=7777", &derr);
  if (!server)
    {
      fprintf (stderr, "%s: DBus server error: %s\n",
	       argv[0], derr.error_message);
      dbus_error_free (&derr);
      return 1;
    }

  dbus_server_set_new_connection_function (server, handle_new_connection,
					   NULL, NULL);
  dbus_server_set_watch_functions (server, add, rm, toggle, data, free);

  return 1;
}

/* arch-tag: 99b857bb-0c66-469d-b4a2-b6e7eca5c74a
 */
