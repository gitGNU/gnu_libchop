/* -*- C -*- */

struct block_store_say_hello_args
{
  string message<>;
};

struct block_store_block_exists_arg
{
  string key<>;
};

struct block_store_write_block_args
{
  string key<>;
  string block<>;
};


program BLOCK_STORE_PROGRAM
{
  version BLOCK_STORE_VERSION
    {
      int
      SAY_HELLO (block_store_say_hello_args) = 0;

      int
      BLOCK_EXISTS (block_store_block_exists_arg) = 0;

      int
      WRITE_BLOCK (block_store_write_block_args) = 0;
    } = 0;
} = 70000;
