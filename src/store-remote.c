#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/serializable.h>

#include <chop/block_rstore.h>

CHOP_DECLARE_RT_CLASS (remote_block_store, block_store,
		       CLIENT *rpc_client;);

CHOP_DEFINE_RT_CLASS (remote_block_store, block_store,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL  /* No serializer/deserializer */);





static errcode_t chop_remote_read_block  (struct chop_block_store *,
					  const chop_block_key_t *,
					  chop_buffer_t *, size_t *);

static errcode_t chop_remote_write_block (struct chop_block_store *,
					  const chop_block_key_t *,
					  const char *, size_t);

static errcode_t chop_remote_close (struct chop_block_store *);

static errcode_t chop_remote_sync (struct chop_block_store *);


errcode_t
chop_remote_block_store_open (const char *host, const char *protocol,
			      chop_block_store_t *store)
{
  static const char generic_hello_arg[] = "libchop's remote block store client";
  int *granted;
  char *hello_arg;
  chop_remote_block_store_t *remote = (chop_remote_block_store_t *)store;

  remote->rpc_client = clnt_create (host, BLOCK_STORE_PROGRAM,
				    BLOCK_STORE_VERSION,
				    protocol);
  if (!remote->rpc_client)
    {
      clnt_pcreateerror (host);  /* FIXME */
      return -1;
    }

  hello_arg = (char *)generic_hello_arg;
  granted = say_hello_0 (&hello_arg, remote->rpc_client);
  if ((!granted) || (!*granted))
    {
      if (!granted)
	clnt_perror (remote->rpc_client, "`hello' call failed");
      else
	clnt_perror (remote->rpc_client,
		     "remote host didn't wanna say `hello'");

      clnt_destroy (remote->rpc_client);
      remote->rpc_client = NULL;
      return -1;
    }

  store->read_block = chop_remote_read_block;
  store->write_block = chop_remote_write_block;
  store->close = chop_remote_close;
  store->sync = chop_remote_sync;

  return 0;
}

static errcode_t
chop_remote_read_block (chop_block_store_t *store,
			const chop_block_key_t *key,
			chop_buffer_t *buffer, size_t *read)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_remote_write_block (chop_block_store_t *store,
			 const chop_block_key_t *key,
			 const char *buffer, size_t size)
{
  return CHOP_ERR_NOT_IMPL;
}

static errcode_t
chop_remote_close (chop_block_store_t *store)
{
  chop_remote_block_store_t *remote = (chop_remote_block_store_t *)store;

  clnt_destroy (remote->rpc_client);
  remote->rpc_client = NULL;

  return 0;
}

static errcode_t
chop_remote_sync (chop_block_store_t *store)
{
  return CHOP_ERR_NOT_IMPL;
}
