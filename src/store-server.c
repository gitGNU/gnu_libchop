
/* Server-side stubs.  */

#include <rpc/svc.h>
#include <chop/block-server.h>

#include "../rpc/block_rstore.h"


CHOP_RPC_HANDLER (int, char *,
		  chop_block_server_say_hello_handler) = NULL;
CHOP_RPC_HANDLER (int, chop_rblock_key_t,
		  chop_block_server_block_exists_handler) = NULL;
CHOP_RPC_HANDLER (int, block_store_write_block_args,
		  chop_block_server_write_block_handler) = NULL;
CHOP_RPC_HANDLER (block_store_read_block_ret, chop_rblock_key_t,
		  chop_block_server_read_block_handler) = NULL;
CHOP_RPC_HANDLER (int, void,
		  chop_block_server_sync_handler) = NULL;
CHOP_RPC_HANDLER (int, void,
		  chop_block_server_close_handler) = NULL;


/* Rewrite the generated code so that we can pass pointers to the handler
   functions as we want, avoiding name clashes, etc.  */
#define block_store_program_0 chop_block_server_process_request
#define say_hello_0_svc       chop_block_server_say_hello_handler
#define block_exists_0_svc    chop_block_server_block_exists_handler
#define write_block_0_svc     chop_block_server_write_block_handler
#define read_block_0_svc      chop_block_server_read_block_handler
#define sync_0_svc            chop_block_server_sync_handler
#define close_0_svc           chop_block_server_close_handler

#include "block_rstore_svc.c"


#if 0
void
chop_block_server_wait_for_requests (SVCXPRT *transport,
				     unsigned timeout_usec)
{
  int err;
  struct timeval timeout;
  fd_set fds, zero_fds;

  FD_ZERO (&fds);
  FD_SET (transport->xp_sock, &fds);
  FD_ZERO (&zero_fds);

  timeout.tv_usec = timeout_usec % 1000000;
  timeout.tv_sec  = timeout_usec / 1000000;
  err = select (transport->xp_sock, &zero_fds, &zero_fds, &timeout);
}
#endif

