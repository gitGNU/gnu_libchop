/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2012  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

/* Sun/ONC RPC interface definition for the remote block store.  */


/* Variable-size octet arrays for block keys and block contents.  */
typedef opaque chop_rblock_key_t<>;
typedef opaque chop_rblock_content_t<>;

/* Array of booleans.  */
typedef opaque chop_rbooleans_t<>;

/* Array of keys.  */
typedef chop_rblock_key_t chop_rblock_keys_t<>;


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

      /* Asks whether the blocks referred to by the given keys exist.  Return
	 an array of the same size with zero if the corresponding key has no
	 associated block, and non-zero otherwise.  */
      chop_rbooleans_t
      BLOCKS_EXIST (chop_rblock_keys_t) = 1;

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
      SYNC (void) = 4;

      /* Say goodbye to the block store.  This also calls `sync', hence the
	 return code.  */
      int
      CLOSE (void) = 5;
    } = 1;

} = 70000;


/*
   Local Variables:
   mode: c
   coding: utf-8
   End:
 */
