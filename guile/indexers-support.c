/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
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

/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <chop/chop-config.h>

#include <errno.h>
#include <assert.h>

#ifdef DEBUG
# include <stdio.h>
#endif


static inline chop_error_t
chop_tree_indexer_open_alloc (size_t keys_per_block,
			      chop_indexer_t **indexer)
{
  chop_error_t err;

  *indexer = gwrap_chop_malloc (&chop_tree_indexer_class);

  err = chop_tree_indexer_open (keys_per_block, *indexer);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *indexer,
				     &chop_tree_indexer_class);
      *indexer = NULL;
    }

  return err;
}

static inline chop_error_t
chop_indexer_index_blocks_alloc  (chop_indexer_t *indexer,
				  chop_chopper_t *input,
				  chop_block_indexer_t *block_indexer,
				  chop_block_store_t *datastore,
				  chop_block_store_t *metadatastore,
				  chop_index_handle_t **handle)
{
  chop_error_t err;
  const chop_class_t *handle_class;

  handle_class = chop_block_indexer_index_handle_class (block_indexer);
  *handle = gwrap_chop_malloc (handle_class);

  err = chop_indexer_index_blocks (indexer, input, block_indexer,
				   datastore, metadatastore, *handle);

  /* The CHOP_STREAM_END is somewhat the "normal" case so we just return 0
     here so that `indexer-index-blocks' ends up only returning the index
     handle.  */
  err = ((err == CHOP_STREAM_END) ? 0 : err);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *handle,
				     handle_class);
      *handle = NULL;
    }

  return err;
}

static inline chop_error_t
chop_indexer_fetch_stream_alloc (chop_indexer_t *indexer,
				 const chop_index_handle_t *handle,
				 chop_block_fetcher_t *fetcher,
				 chop_block_store_t *datastore,
				 chop_block_store_t *metadatastore,
				 chop_stream_t **stream)
{
  chop_error_t err;
  const chop_class_t *stream_class;

  stream_class = chop_indexer_stream_class (indexer);
  *stream = gwrap_chop_malloc (stream_class);

  err = chop_indexer_fetch_stream (indexer, handle, fetcher,
				   datastore, metadatastore, *stream);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *stream,
				     stream_class);
      *stream = NULL;
    }

  return err;
}


static inline chop_error_t
chop_ascii_serialize_index_tuple_alloc (chop_index_handle_t *index,
					chop_indexer_t *indexer,
					chop_block_indexer_t *block_indexer,
					char **serial)
{
  chop_error_t err;
  chop_buffer_t buffer;

  err = chop_buffer_init (&buffer, 200);
  if (err)
    return err;

  err = chop_ascii_serialize_index_tuple (index, indexer, block_indexer,
					  &buffer);
  if (err)
    return err;

  *serial = scm_malloc (chop_buffer_size (&buffer));
  memcpy (*serial, chop_buffer_content (&buffer), chop_buffer_size (&buffer));

  chop_buffer_return (&buffer);

  return err;
}

static inline chop_error_t
chop_ascii_deserialize_index_tuple_alloc (const char *serial,
					  chop_index_handle_t **index,
					  chop_indexer_t **indexer,
					  chop_block_fetcher_t **fetcher,
					  unsigned *bytes_read)
{
  chop_error_t err;
  size_t offset = 0;
  const chop_class_t *indexer_class, *fetcher_class, *handle_class;

  err = chop_ascii_deserialize_index_tuple_s1 (serial, strlen (serial),
					       &indexer_class,
					       &fetcher_class, &handle_class,
					       &offset);
  if (err)
    return err;

  *index = gwrap_chop_malloc (handle_class);
  *indexer = gwrap_chop_malloc (indexer_class);
  *fetcher = gwrap_chop_malloc (fetcher_class);

  *bytes_read = 0;
  err = chop_ascii_deserialize_index_tuple_s2 (serial + offset,
					       strlen (serial) - offset,
					       indexer_class,
					       fetcher_class, handle_class,
					       *indexer, *fetcher, *index,
					       bytes_read);
  *bytes_read += offset;

  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *index,
				     handle_class);
      gwrap_chop_free_uninitialized ((chop_object_t *) *fetcher,
				     fetcher_class);
      gwrap_chop_free_uninitialized ((chop_object_t *) *indexer,
				     indexer_class);
      *index = NULL;
      *fetcher = NULL;
      *indexer = NULL;
      return err;
    }

  return err;
}

