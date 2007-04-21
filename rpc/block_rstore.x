/* It really looks like -*- C -*- .  */

/* Sun/ONC RPC interface definition for the remote block store.  */


/* Variable-size octet arrays for block keys and block contents.  */
typedef opaque chop_rblock_key_t<>;
typedef opaque chop_rblock_content_t<>;

struct block_store_write_block_args
{
  chop_rblock_key_t key;
  chop_rblock_content_t block;
};

struct block_store_read_block_ret
{
  int status;
  chop_rblock_content_t block;
};



program BLOCK_STORE_PROGRAM
{
  version BLOCK_STORE_VERSION
    {
      /* Say hello to the server.  Return zero if further discussion was
	 rejected.  */
      int
      SAY_HELLO (string) = 0;

      /* Asks whether the block referred to by the given key exists.  Return
	 zero if it doesn't, non-zero if it does.  */
      int
      BLOCK_EXISTS (chop_rblock_key_t) = 1;

      /* Write the block with the given key and contents.  Return zero on
	 success.  */
      int
      WRITE_BLOCK (block_store_write_block_args) = 2;

      /* Query the block referred to by the given key.  On success STATUS is
	 set to zero.  On failure, STATUS is set to non-zero and the value of
	 BLOCK may be discarded.  */
      block_store_read_block_ret
      READ_BLOCK (chop_rblock_key_t) = 3;

      /* Sync the block store.  */
      int
      SYNC () = 4;

      /* Say goodbye to the block store.  This also calls `sync', hence the
	 return code.  */
      int
      CLOSE () = 5;
    } = 0;
} = 70000;
