/* A remote, Sun RPC-based, block store.  */

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/objects.h>
#include <chop/logs.h>

#include <chop/block_rstore.h>


CHOP_DECLARE_RT_CLASS (sunrpc_remote_block_store, block_store,
		       chop_log_t log;
		       CLIENT *rpc_client;);

static errcode_t
remote_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_sunrpc_remote_block_store_t *remote;

  remote = (chop_sunrpc_remote_block_store_t *)object;

  remote->block_store.iterator_class = NULL;
  remote->rpc_client = NULL;

  return chop_log_init ("remote-block-store", &remote->log);
}

static void
remote_dtor (chop_object_t *object)
{
  chop_sunrpc_remote_block_store_t *remote;

  remote = (chop_sunrpc_remote_block_store_t *)object;

  chop_object_destroy ((chop_object_t *)&remote->log);
}

CHOP_DEFINE_RT_CLASS (sunrpc_remote_block_store, block_store,
		      remote_ctor, remote_dtor,
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);





static errcode_t chop_sunrpc_block_exists (chop_block_store_t *,
					   const chop_block_key_t *,
					   int *);

static errcode_t chop_sunrpc_read_block  (struct chop_block_store *,
					  const chop_block_key_t *,
					  chop_buffer_t *, size_t *);

static errcode_t chop_sunrpc_write_block (struct chop_block_store *,
					  const chop_block_key_t *,
					  const char *, size_t);

static errcode_t chop_sunrpc_delete_block (chop_block_store_t *,
					   const chop_block_key_t *);

static errcode_t chop_sunrpc_first_block (chop_block_store_t *,
					  chop_block_iterator_t *);

static errcode_t chop_sunrpc_it_next (chop_block_iterator_t *);

static errcode_t chop_sunrpc_close (struct chop_block_store *);

static errcode_t chop_sunrpc_sync (struct chop_block_store *);


errcode_t
chop_sunrpc_remote_block_store_open (const char *host, const char *protocol,
				     chop_block_store_t *store)
{
  static const char generic_hello_arg[] = "libchop's remote block store client";
  CLIENT *rpc_client;
  int *granted;
  char *hello_arg;
  chop_sunrpc_remote_block_store_t *remote = (chop_sunrpc_remote_block_store_t *)store;


  rpc_client = clnt_create (host, BLOCK_STORE_PROGRAM,
			    BLOCK_STORE_VERSION,
			    protocol);
  if (!rpc_client)
    {
      clnt_pcreateerror (host);  /* FIXME */
      return -1;
    }

  hello_arg = (char *)generic_hello_arg;
  granted = say_hello_0 (&hello_arg, rpc_client);
  if ((!granted) || (!*granted))
    {
      if (!granted)
	clnt_perror (rpc_client, "`hello' call failed");
      else
	clnt_perror (rpc_client,
		     "remote host didn't wanna say `hello'");

      clnt_destroy (rpc_client);
      remote->rpc_client = NULL;
      return -1;
    }

  chop_object_initialize ((chop_object_t *)store,
			  &chop_sunrpc_remote_block_store_class);

  remote->rpc_client = rpc_client;

  store->block_exists = chop_sunrpc_block_exists;
  store->read_block = chop_sunrpc_read_block;
  store->write_block = chop_sunrpc_write_block;
  store->delete_block = chop_sunrpc_delete_block;
  store->first_block = chop_sunrpc_first_block;
  store->close = chop_sunrpc_close;
  store->sync = chop_sunrpc_sync;

  return 0;
}


static errcode_t
chop_sunrpc_block_exists (chop_block_store_t *store,
			  const chop_block_key_t *key,
			  int *exists)
{
  errcode_t err = 0;
  int *ret;
  chop_rblock_key_t rkey;
  chop_sunrpc_remote_block_store_t *remote = (chop_sunrpc_remote_block_store_t *)store;

  rkey.chop_rblock_key_t_len = chop_block_key_size (key);
  rkey.chop_rblock_key_t_val = (char *)chop_block_key_buffer (key);

  ret = block_exists_0 (&rkey, remote->rpc_client);
  if (!ret)
    {
      chop_log_printf (&remote->log, "exists RPC failed");

      *exists = 0;
      err = CHOP_STORE_ERROR;
    }
  else
    *exists = *ret;

  return err;
}

static errcode_t
chop_sunrpc_read_block (chop_block_store_t *store,
			const chop_block_key_t *key,
			chop_buffer_t *buffer, size_t *read)
{
  errcode_t err;
  block_store_read_block_ret *ret;
  chop_rblock_key_t rkey;
  chop_sunrpc_remote_block_store_t *remote = (chop_sunrpc_remote_block_store_t *)store;

  rkey.chop_rblock_key_t_len = chop_block_key_size (key);
  rkey.chop_rblock_key_t_val = (char *)chop_block_key_buffer (key);

  ret = read_block_0 (&rkey, remote->rpc_client);
  if ((!ret) || (ret->status))
    {
      if (ret)
	chop_log_printf (&remote->log, "read RPC failed with %i",
			 ret->status);
      else
	chop_log_printf (&remote->log, "read RPC failed");

      return CHOP_STORE_ERROR;
    }

  err = chop_buffer_push (buffer,
			  ret->block.chop_rblock_content_t_val,
			  ret->block.chop_rblock_content_t_len);
  if (err)
    *read = 0;
  else
    *read = ret->block.chop_rblock_content_t_len;

  return err;
}

static errcode_t
chop_sunrpc_write_block (chop_block_store_t *store,
			 const chop_block_key_t *key,
			 const char *buffer, size_t size)
{
  int *ret;
  block_store_write_block_args args;
  chop_sunrpc_remote_block_store_t *remote = (chop_sunrpc_remote_block_store_t *)store;

  /* FIXME:  Copy the args?! */
  args.key.chop_rblock_key_t_len = chop_block_key_size (key);
  args.key.chop_rblock_key_t_val = (char *)chop_block_key_buffer (key);
  args.block.chop_rblock_content_t_len = size;
  args.block.chop_rblock_content_t_val = (char *)buffer;

  ret = write_block_0 (&args, remote->rpc_client);
  if ((!ret) || (*ret))
    {
      if (ret)
	chop_log_printf (&remote->log, "write RPC failed with %i", *ret);
      else
	chop_log_printf (&remote->log, "write RPC failed");

      return CHOP_STORE_ERROR;
    }

  return 0;
}

static errcode_t
chop_sunrpc_delete_block (chop_block_store_t *store,
			  const chop_block_key_t *key)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_sunrpc_first_block (chop_block_store_t *store,
			 chop_block_iterator_t *it)
{
  chop_sunrpc_it_next (it);  /* Shut up the "unused function" warning.  */
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_sunrpc_it_next (chop_block_iterator_t *it)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_sunrpc_close (chop_block_store_t *store)
{
  chop_sunrpc_remote_block_store_t *remote = (chop_sunrpc_remote_block_store_t *)store;

  if (remote->rpc_client)
    {
      int *ret;

      /* We're assuming that the server-side implementation of `close' does
	 also perform the operation performed by `sync'.  */
      ret = close_0 (NULL, remote->rpc_client);
      if ((!ret) || (*ret))
	{
	  if (ret)
	    chop_log_printf (&remote->log, "close RPC failed with %i", *ret);
	  else
	    chop_log_printf (&remote->log, "close RPC failed");

	  return CHOP_STORE_ERROR;
	}

      if (remote->rpc_client)
	{
	  clnt_destroy (remote->rpc_client);
	  remote->rpc_client = NULL;
	}
    }

  return 0;
}

static errcode_t
chop_sunrpc_sync (chop_block_store_t *store)
{
  int *ret;
  chop_sunrpc_remote_block_store_t *remote = (chop_sunrpc_remote_block_store_t *)store;

  ret = sync_0 (NULL, remote->rpc_client);
  if ((!ret) || (*ret))
    {
      if (ret)
	chop_log_printf (&remote->log, "sync RPC failed with %i", *ret);
      else
	chop_log_printf (&remote->log, "sync RPC failed");

      return CHOP_STORE_ERROR;
    }

  return 0;
}
