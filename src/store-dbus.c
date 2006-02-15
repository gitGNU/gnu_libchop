/* A remote, DBus-based, block store.  */

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
		       DBusConnection *connection;)

static errcode_t
dbus_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_dbus_block_store_t *remote;

  remote = (chop_dbus_block_store_t *)object;
  remote->connection = NULL;

  return chop_log_init ("remote-block-store", &remote->log);
}

static void
dbus_dtor (chop_object_t *object)
{
  chop_dbus_block_store_t *remote;

  dbus_connection_close (remote->connection);
  dbus_connection_unref (remote->connection);
  remote->connection = NULL;
  chop_object_destroy ((chop_object_t *)&remote->log);
}

CHOP_DEFINE_RT_CLASS (dbus_block_store, block_store,
		      dbus_ctor, dbus_dtor,
		      NULL, NULL,
		      NULL, NULL  /* No serializer/deserializer */);





static errcode_t chop_dbus_block_exists (chop_block_store_t *,
					   const chop_block_key_t *,
					   int *);

static errcode_t chop_dbus_read_block  (struct chop_block_store *,
					  const chop_block_key_t *,
					  chop_buffer_t *, size_t *);

static errcode_t chop_dbus_write_block (struct chop_block_store *,
					  const chop_block_key_t *,
					  const char *, size_t);

static errcode_t chop_dbus_delete_block (chop_block_store_t *,
					   const chop_block_key_t *);

static errcode_t chop_dbus_first_it (chop_block_store_t *,
				     chop_block_iterator_t *);

static errcode_t chop_dbus_it_next (chop_block_iterator_t *);

static errcode_t chop_dbus_close (struct chop_block_store *);

static errcode_t chop_dbus_sync (struct chop_block_store *);


errcode_t
chop_dbus_block_store_open (const char *host,
			    chop_block_store_t *store)
{
  errcode_t err;
  char *d_address;
  DBusError d_err;
  DBusConnection *connection;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  dbus_error_init (&d_err);

  d_address = alloca (strlen (host) + 5);
  strcpy (d_address, "tcp:");
  strcat (d_address, host);

  connection = dbus_connection_open (d_address, &d_err);
  err = chop_object_initialize ((chop_object_t *)store,
				&chop_dbus_block_store_class);

  /* FIXME: Invoke some `hello-world' method there.  */

  remote->connection = connection;

  store->block_exists = chop_dbus_block_exists;
  store->read_block = chop_dbus_read_block;
  store->write_block = chop_dbus_write_block;
  store->delete_block = chop_dbus_delete_block;
  store->first_block = chop_dbus_first_it;
  store->close = chop_dbus_close;
  store->sync = chop_dbus_sync;

  return err;
}


static errcode_t
chop_dbus_block_exists (chop_block_store_t *store,
			const chop_block_key_t *key,
			int *exists)
{
  errcode_t err = 0;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  DBusError derr;
  DBusMessage *call, *reply;

  *exists = 0;

  call = dbus_message_new_method_call (NULL,
				       CHOP_DBUS_BLOCK_STORE_PATH,
				       CHOP_DBUS_BLOCK_STORE_INTERFACE,
				       "block-exists?");
  if (!call)
    return ENOMEM;

  if (!dbus_message_append_args (call, DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
				 chop_block_key_buffer (key),
				 chop_block_key_size (key)))
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
	  fprintf (stderr, "DBus error: %s\n", derr.message);
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
		{ DBUS_TYPE_BOOLEAN, DBUS_TYPE_INVALID };
	      if (!dbus_message_has_signature (reply, good_sig))
		err = CHOP_INVALID_ARG;
	      else
		{
		  DBusMessageIter it;
		  dbus_bool_t dexists = 0;

		  dbus_message_iter_init (reply, &it);
		  dbus_message_iter_get_basic (&it, &exists);
		  *exists = (int)dexists;
		}
	    }
	}

      dbus_message_unref (reply);
    }

 finish:
  dbus_message_unref (call);

  return err;
}

static errcode_t
chop_dbus_read_block (chop_block_store_t *store,
		      const chop_block_key_t *key,
		      chop_buffer_t *buffer, size_t *read)
{
  errcode_t err;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;
  DBusError derr;

  DBusMessage *call, *reply;

  call = dbus_message_new_method_call (NULL,
				       CHOP_DBUS_BLOCK_STORE_PATH,
				       CHOP_DBUS_BLOCK_STORE_INTERFACE,
				       "read-block");
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
	  fprintf (stderr, "DBus error: %s\n", derr.message);
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

static errcode_t
chop_dbus_write_block (chop_block_store_t *store,
		       const chop_block_key_t *key,
		       const char *buffer, size_t size)
{
  errcode_t err;
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;
  DBusError derr;

  DBusMessage *call, *reply;

  call = dbus_message_new_method_call (NULL,
				       CHOP_DBUS_BLOCK_STORE_PATH,
				       CHOP_DBUS_BLOCK_STORE_INTERFACE,
				       "write-block");
  if (!call)
    return ENOMEM;

  if (!dbus_message_append_args (call, DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
				 chop_block_key_buffer (key),
				 chop_block_key_size (key),
				 DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE,
				 buffer, size))
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
	  fprintf (stderr, "DBus error: %s\n", derr.message);
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

      dbus_message_unref (reply);
    }

 finish:
  dbus_message_unref (call);

  return err;
}

static errcode_t
chop_dbus_delete_block (chop_block_store_t *store,
			  const chop_block_key_t *key)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_dbus_first_it (chop_block_store_t *store,
		    chop_block_iterator_t *it)
{
  chop_dbus_it_next (it);  /* Shut up the "unused function" warning.  */
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_dbus_it_next (chop_block_iterator_t *it)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_dbus_close (chop_block_store_t *store)
{
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  if (remote->connection)
    {
      /* FIXME: Implement a `close' method call.  */
    }

  return 0;
}

static errcode_t
chop_dbus_sync (chop_block_store_t *store)
{
  chop_dbus_block_store_t *remote = (chop_dbus_block_store_t *)store;

  /* FIXME: Implement it.  */

  return 0;
}


/* arch-tag: cbba20fc-a3a1-43c1-8580-b54431a1d804
 */

