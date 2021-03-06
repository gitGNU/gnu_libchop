/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2012  Ludovic Courtès <ludo@gnu.org>
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

/* A remote, DBus-based, block store.  */

#include <alloca.h>

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/objects.h>
#include <chop/logs.h>

#include <errno.h>
#include <stdio.h>
#include <assert.h>


/* XXX: Until DBus has reached 1.0, we must define this to tell it that we're
   aware that it may change.  */
#define DBUS_API_SUBJECT_TO_CHANGE 1

#include <dbus/dbus.h>

#define CHOP_DBUS_BLOCK_STORE_INTERFACE  "fr.laas.BlockStore"
#define CHOP_DBUS_BLOCK_STORE_PATH       "/store"


CHOP_DECLARE_RT_CLASS (dbus_block_store, block_store,
		       chop_log_t log;
		       DBusConnection *connection;
		       char *object_path;)

static chop_error_t
dbus_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_dbus_block_store_t *remote;

  remote = (chop_dbus_block_store_t *)object;
  remote->connection = NULL;
  remote->object_path = NULL;

  return chop_log_init ("remote-block-store", &remote->log);
}

static void
dbus_dtor (chop_object_t *object)
{
  chop_dbus_block_store_t *remote;

  if (remote->connection)
    {
      dbus_connection_close (remote->connection);
      dbus_connection_unref (remote->connection);
      remote->connection = NULL;
    }

  if (remote->object_path)
    {
      free (remote->object_path);
      remote->object_path = NULL;
    }

  chop_object_destroy ((chop_object_t *)&remote->log);
}

CHOP_DEFINE_RT_CLASS (dbus_block_store, block_store,
		      dbus_ctor, dbus_dtor,
		      NULL, NULL,
		      NULL, NULL  /* No serializer/deserializer */);





static chop_error_t chop_dbus_blocks_exist (chop_block_store_t *,
					    size_t n,
					    const chop_block_key_t keys[n],
					    bool e[n]);

static chop_error_t chop_dbus_read_block  (struct chop_block_store *,
					   const chop_block_key_t *,
					   chop_buffer_t *, size_t *);

static chop_error_t chop_dbus_write_block (struct chop_block_store *,
					   const chop_block_key_t *,
					   const char *, size_t);

static chop_error_t chop_dbus_delete_block (chop_block_store_t *,
					    const chop_block_key_t *);

static chop_error_t chop_dbus_first_it (chop_block_store_t *,
					chop_block_iterator_t *);

static chop_error_t chop_dbus_it_next (chop_block_iterator_t *);

static chop_error_t chop_dbus_close (struct chop_block_store *);

static chop_error_t chop_dbus_sync (struct chop_block_store *);


static chop_error_t
invoke_new_client_method (DBusConnection *connection, char **path,
			  chop_log_t *log)
{
  chop_error_t err = 0;
  DBusError derr;
  DBusMessage *call = NULL, *reply = NULL;
  DBusMessageIter iter;
  const char *path_buf = NULL;
  static char valid_sig[] = { DBUS_TYPE_OBJECT_PATH, DBUS_TYPE_INVALID };

  *path = NULL;
  call = dbus_message_new_method_call (NULL, "/constructor",
				       CHOP_DBUS_BLOCK_STORE_INTERFACE,
				       "NewClient");
  if (!call)
    {
      err = ENOMEM;
      goto finish;
    }

  dbus_error_init (&derr);
  reply = dbus_connection_send_with_reply_and_block (connection, call,
						     -1, &derr);
  if ((!reply) || (dbus_error_is_set (&derr)))
    {
      chop_log_printf (log, "`NewClient' invocation failed: %s: %s (%p)",
		       derr.name, derr.message, reply);
      err = CHOP_INVALID_ARG;
      goto finish;
    }

  if ((dbus_message_get_type (reply) != DBUS_MESSAGE_TYPE_METHOD_RETURN)
      || (!dbus_message_has_signature (reply, valid_sig)))
    {
      err = CHOP_INVALID_ARG;
      goto finish;
    }

  chop_log_printf (log, "got valid `NewClient' reply");

  if (!dbus_message_iter_init (reply, &iter))
    {
      chop_log_printf (log, "empty `NewClient' reply message");
      err = CHOP_INVALID_ARG;
      goto finish;
    }
  chop_log_printf (log, "iter type: %i",
		   dbus_message_iter_get_arg_type (&iter));

  dbus_message_iter_get_basic (&iter, &path_buf);
  if (path_buf)
    {
      chop_log_printf (log, "got object path `%s'", path_buf);
      *path = strdup (path_buf);
      if (!*path)
	err = ENOMEM;
    }
  else
    {
      chop_log_printf (log, "failed to retrieve object path");
      err = CHOP_INVALID_ARG;
    }

 finish:
  if (call)
    dbus_message_unref (call);
  if (reply)
    dbus_message_unref (reply);

  return err;
}

chop_error_t
chop_dbus_block_store_open (const char *d_address,
			    chop_block_store_t *store)
{
  chop_error_t err;
  DBusError d_err;
  DBusConnection *connection;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  err = chop_object_initialize ((chop_object_t *)store,
				&chop_dbus_block_store_class);
  if (err)
    return err;

  /* XXX: debugging */
  chop_log_attach (&remote->log, 2, 0);

  dbus_error_init (&d_err);

  connection = dbus_connection_open (d_address, &d_err);
  if ((!connection) || (dbus_error_is_set (&d_err)))
    {
      fprintf (stderr, "DBus error: %s: %s\n",
	       d_err.name, d_err.message);
      return CHOP_INVALID_ARG;
    }

  remote->connection = connection;

  store->blocks_exist = chop_dbus_blocks_exist;
  store->read_block = chop_dbus_read_block;
  store->write_block = chop_dbus_write_block;
  store->delete_block = chop_dbus_delete_block;
  store->first_block = chop_dbus_first_it;
  store->close = chop_dbus_close;
  store->sync = chop_dbus_sync;

  err = invoke_new_client_method (connection, &remote->object_path,
				  &remote->log);
  if (err)
    {
      chop_object_destroy ((chop_object_t *)store);
      return err;
    }

  return err;
}


static chop_error_t
chop_dbus_blocks_exist (chop_block_store_t *store,
			size_t n,
			const chop_block_key_t keys[n],
			bool exists[n])
{
  size_t i;
  chop_error_t err = 0;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  /* FIXME: Optimize by using a `BlocksExist' (plural) method.  */
  for (i = 0; i < n && err == 0; i++)
    {
      DBusError derr;
      DBusMessage *call, *reply;

      call = dbus_message_new_method_call (NULL,
					   remote->object_path,
					   CHOP_DBUS_BLOCK_STORE_INTERFACE,
					   "BlockExists");
      if (!call)
	return ENOMEM;

      if (!dbus_message_append_args (call, DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
				     chop_block_key_buffer (&keys[i]),
				     chop_block_key_size (&keys[i])))
	{
	  err = ENOMEM;
	  goto finish;
	}

      dbus_error_init (&derr);
      reply = dbus_connection_send_with_reply_and_block (remote->connection, call,
							 -1, &derr);

      if (!reply)
	err = ENOMEM;
      else
	{
	  if (dbus_error_is_set (&derr))
	    {
	      chop_log_printf (&remote->log, "DBus error: %s\n", derr.message);
	      dbus_error_free (&derr);
	      err = CHOP_STORE_ERROR;
	    }
	  else
	    {
	      if (dbus_message_get_type (reply)
		  != DBUS_MESSAGE_TYPE_METHOD_RETURN)
		err = CHOP_STORE_ERROR;
	      else
		{
		  static const char good_sig[] =
		    { DBUS_TYPE_BOOLEAN, DBUS_TYPE_INaVALID };
		  if (!dbus_message_has_signature (reply, good_sig))
		    err = CHOP_INVALID_ARG;
		  else
		    {
		      DBusMessageIter it;
		      dbus_bool_t dexists = 0;

		      dbus_message_iter_init (reply, &it);
		      dbus_message_iter_get_basic (&it, &exists);
		      exists[i] = dexists;
		    }
		}
	    }

	  dbus_message_unref (reply);
	}

    finish:
      dbus_message_unref (call);
    }

  return err;
}

static chop_error_t
chop_dbus_read_block (chop_block_store_t *store,
		      const chop_block_key_t *key,
		      chop_buffer_t *buffer, size_t *read)
{
  chop_error_t err;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;
  DBusError derr;

  DBusMessage *call, *reply;

  call = dbus_message_new_method_call (NULL,
				       remote->object_path,
				       CHOP_DBUS_BLOCK_STORE_INTERFACE,
				       "ReadBlock");
  if (!call)
    return ENOMEM;

  if (!dbus_message_append_args (call, DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
				 chop_block_key_buffer (key),
				 chop_block_key_size (key)))
    {
      err = ENOMEM;
      goto finish;
    }

  *read = 0;
  dbus_error_init (&derr);
  reply = dbus_connection_send_with_reply_and_block (remote->connection, call,
						     -1, &derr);

  if (!reply)
    err = ENOMEM;
  else
    {
      if (dbus_error_is_set (&derr))
	{
	  chop_log_printf (&remote->log , "DBus error: %s\n", derr.message);
	  dbus_error_free (&derr);
	  err = CHOP_STORE_ERROR;
	}
      else
	{
	  if (dbus_message_get_type (reply) != DBUS_MESSAGE_TYPE_METHOD_RETURN)
	    err = CHOP_STORE_ERROR;
	  else
	    {
	      static const char good_sig[] =
		{ DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE, DBUS_TYPE_INVALID };
	      if (!dbus_message_has_signature (reply, good_sig))
		err = CHOP_INVALID_ARG;
	      else
		{
		  DBusMessageIter it;
		  size_t len = 0, len2;
		  unsigned char *dblock;

		  dbus_message_iter_init (reply, &it);
		  len = (size_t)dbus_message_iter_get_array_len (&it);
		  dblock = (unsigned char *)alloca (len);

		  dbus_message_iter_get_fixed_array (&it, dblock, (int *)&len2);
		  assert (len2 == len);

		  err = chop_buffer_push (buffer, (const char *)dblock, len);
		  *read = err ? 0 : len;
		}
	    }
	}

      dbus_message_unref (reply);
    }

 finish:
  dbus_message_unref (call);

  return err;
}

static chop_error_t
chop_dbus_write_block (chop_block_store_t *store,
		       const chop_block_key_t *key,
		       const char *buffer, size_t size)
{
  chop_error_t err;
  const char *key_buffer;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;
  DBusError derr;

  DBusMessage *call, *reply;

  call = dbus_message_new_method_call (NULL,
				       remote->object_path,
				       CHOP_DBUS_BLOCK_STORE_INTERFACE,
				       "WriteBlock");
  if (!call)
    return ENOMEM;

  key_buffer = chop_block_key_buffer (key);
  if (!dbus_message_append_args (call,
				 DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
				 &key_buffer, chop_block_key_size (key),
				 DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
				 &buffer, size,
				 DBUS_TYPE_INVALID))
    {
      err = ENOMEM;
      goto finish;
    }

  dbus_error_init (&derr);
  reply = dbus_connection_send_with_reply_and_block (remote->connection, call,
						     -1, &derr);

  if ((!reply) || (dbus_error_is_set (&derr)))
    {
      chop_log_printf (&remote->log,
		       "DBus error: %s: %s\n", derr.name, derr.message);
      if (dbus_error_has_name (&derr, DBUS_ERROR_UNKNOWN_METHOD))
	err = CHOP_ERR_NOT_IMPL;
      else
	err = CHOP_STORE_ERROR;

      dbus_error_free (&derr);
    }
  else
    {
      if (dbus_message_get_type (reply) != DBUS_MESSAGE_TYPE_METHOD_RETURN)
	err = CHOP_STORE_ERROR;
      else
	{
	  static const char good_sig[] =
	    { DBUS_TYPE_BOOLEAN, DBUS_TYPE_INVALID };
	  if (!dbus_message_has_signature (reply, good_sig))
	    err = CHOP_INVALID_ARG;
	  else
	    {
	      DBusMessageIter it;
	      dbus_bool_t success;

	      dbus_message_iter_init (reply, &it);
	      dbus_message_iter_get_basic (&it, &success);

	      /* XXX: We could have better error handling here.  And we
		 could use DBus' error messages for that.  */
	      err = success ? 0 : CHOP_STORE_ERROR;
	    }
	}
    }

  if (reply)
    dbus_message_unref (reply);

 finish:
  dbus_message_unref (call);

  return err;
}

static chop_error_t
chop_dbus_delete_block (chop_block_store_t *store,
			  const chop_block_key_t *key)
{
  return CHOP_ERR_NOT_IMPL;
}

static chop_error_t
chop_dbus_first_it (chop_block_store_t *store,
		    chop_block_iterator_t *it)
{
  chop_dbus_it_next (it);  /* Shut up the "unused function" warning.  */
  return CHOP_ERR_NOT_IMPL;
}

static chop_error_t
chop_dbus_it_next (chop_block_iterator_t *it)
{
  return CHOP_ERR_NOT_IMPL;
}

static chop_error_t
chop_dbus_close (chop_block_store_t *store)
{
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  if (remote->connection)
    {
      /* FIXME: Implement a `close' method call.  */
    }

  return 0;
}

static chop_error_t
chop_dbus_sync (chop_block_store_t *store)
{
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  /* FIXME: Implement it.  */

  return 0;
}


/* arch-tag: cbba20fc-a3a1-43c1-8580-b54431a1d804
 */

