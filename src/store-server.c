
/* Server-side stubs.  */

#include <rpc/svc.h>
#include <chop/block-server.h>

#include "../rpc/block_rstore.h"


chop_rpc_handler_t chop_block_server_say_hello_handler = NULL;
chop_rpc_handler_t chop_block_server_block_exists_handler = NULL;
chop_rpc_handler_t chop_block_server_write_block_handler = NULL;

/* Rewrite the generated code so that we can pass pointers to the handler
   functions as we want, avoiding name clashes, etc.  */
#define block_store_program_0 chop_block_server_process_request
#define say_hello_0_svc       chop_block_server_say_hello_handler
#define block_exists_0_svc    chop_block_server_block_exists_handler
#define write_block_0_svc     chop_block_server_write_block_handler

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

