/* It really looks like -*- C -*- .  */


typedef char chop_rblock_key_t<>;
typedef char chop_rblock_content_t<>;

struct block_store_write_block_args
{
  chop_rblock_key_t key;
  chop_rblock_content_t block;
};


program BLOCK_STORE_PROGRAM
{
  version BLOCK_STORE_VERSION
    {
      int
      SAY_HELLO (string) = 0;

      int
      BLOCK_EXISTS (chop_rblock_key_t) = 1;

      int
      WRITE_BLOCK (block_store_write_block_args) = 2;
    } = 0;
} = 70000;
