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


/* Define an RPC handler hook (function pointer).  */
#define CHOP_RPC_HANDLER(_ret, _arg, _name) \
  _ret *(* _name) (_arg *, struct svc_req *)

/* These are the RPC handlers that shall be defined by the user before
   SVC_REGISTER is called.  */
extern CHOP_RPC_HANDLER (int, char *, chop_block_server_say_hello_handler);
extern CHOP_RPC_HANDLER (int, chop_rblock_key_t,
			 chop_block_server_block_exists_handler);
extern CHOP_RPC_HANDLER (int, block_store_write_block_args,
			 chop_block_server_write_block_handler);
extern CHOP_RPC_HANDLER (block_store_read_block_ret, chop_rblock_key_t,
			 chop_block_server_read_block_handler);
extern CHOP_RPC_HANDLER (int, void,
			 chop_block_server_sync_handler);
extern CHOP_RPC_HANDLER (int, void,
			 chop_block_server_close_handler);

#endif
