#ifndef __CHOP_BLOCK_SERVER_H__
#define __CHOP_BLOCK_SERVER_H__

/* RPC block server stubs.  This is provided as part of
   `libchop-block-server'.  */

#include <rpc/svc.h>
#include <chop/block_rstore.h>


/* The rpcgen-generated server-side stub.  This is the function that should
   be passed to SVC_REGITER.  */
extern void chop_block_server_process_request (struct svc_req *rqstp,
					       register SVCXPRT *transp);


typedef char *(* chop_rpc_handler_t) (char *, struct svc_req *);

/* These are the RPC handlers that shall be defined by the user before
   SVC_REGISTER is called.  */
extern chop_rpc_handler_t chop_block_server_say_hello_handler;
extern chop_rpc_handler_t chop_block_server_block_exists_handler;
extern chop_rpc_handler_t chop_block_server_write_block_handler;

#endif
