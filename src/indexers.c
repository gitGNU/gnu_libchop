/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2011, 2013  Ludovic Courtès <ludo@gnu.org>
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

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/chop.h>
#include <chop/block-indexers.h>
#include <chop/indexers.h>

#include <string.h>

/* Define CHOP_INDEXER_CLASS.  */
CHOP_DEFINE_RT_CLASS (indexer, object,
		      NULL, NULL, /* No constructor/destructor */
		      NULL, NULL, /* No copy/equalp */
		      NULL, NULL  /* No serializer/deserializer */);


chop_error_t
chop_ascii_serialize_index_tuple (const chop_index_handle_t *index,
				  const chop_indexer_t *indexer,
				  const chop_block_indexer_t *block_indexer,
				  chop_buffer_t *buffer)
{
#define APPEND_COLON(_buf)			\
  err = chop_buffer_append ((_buf), ":", 1);	\
  if (err)					\
    goto finish;

  chop_error_t err;
  chop_buffer_t ascii_object;
  chop_block_fetcher_t *block_fetcher;
  const chop_class_t *fetcher_class, *indexer_class;
  const chop_class_t *index_class;
  const char *class_name;

  /* Instantiate a block fetcher corresponding to BLOCK_INDEXER.  */
  fetcher_class = chop_block_indexer_fetcher_class (block_indexer);

  block_fetcher = chop_class_alloca_instance (fetcher_class);
  err = chop_block_indexer_initialize_fetcher (block_indexer,
					       block_fetcher);
  if (err)
    return err;

  if (!chop_class_inherits (fetcher_class, &chop_block_fetcher_class))
    return CHOP_INVALID_ARG;

  err = chop_buffer_init (&ascii_object, 60);
  if (err)
    return err;

  /* Serialize the three class names.  */
  indexer_class = chop_object_get_class ((chop_object_t *)indexer);
  class_name = chop_class_name (indexer_class);
  err = chop_buffer_push (buffer, class_name, strlen (class_name));
  if (err)
    goto finish;

  APPEND_COLON (buffer);

  class_name = chop_class_name (fetcher_class);
  err = chop_buffer_append (buffer, class_name, strlen (class_name));
  if (err)
    goto finish;

  APPEND_COLON (buffer);

  index_class = chop_object_get_class ((chop_object_t *)index);
  class_name = chop_class_name (index_class);
  err = chop_buffer_append (buffer, class_name, strlen (class_name));
  if (err)
    goto finish;

  APPEND_COLON (buffer);

  /* Serialize the objects themselves, separated by a colon.  */
  err = chop_object_serialize ((chop_object_t *)indexer,
			       CHOP_SERIAL_ASCII,
			       &ascii_object);
  if (err)
    goto finish;

  err = chop_buffer_append (buffer, chop_buffer_content (&ascii_object),
			    /* don't append the trailing `\0' */
			    chop_buffer_size (&ascii_object)
			    ? chop_buffer_size (&ascii_object) - 1
			    : 0);
  if (err)
    goto finish;

  APPEND_COLON (buffer);

  chop_buffer_clear (&ascii_object);
  err = chop_object_serialize ((chop_object_t *)block_fetcher,
			       CHOP_SERIAL_ASCII,
			       &ascii_object);
  if (err)
    goto finish;

  err = chop_buffer_append (buffer, chop_buffer_content (&ascii_object),
			    /* don't append the trailing `\0' */
			    chop_buffer_size (&ascii_object)
			    ? chop_buffer_size (&ascii_object) - 1
			    : 0);
  if (err)
    goto finish;

  APPEND_COLON (buffer);

  chop_buffer_clear (&ascii_object);
  err = chop_object_serialize ((chop_object_t *)index, CHOP_SERIAL_ASCII,
			       &ascii_object);
  if (err)
    goto finish;

  err = chop_buffer_append (buffer, chop_buffer_content (&ascii_object),
			    chop_buffer_size (&ascii_object));
  if (err)
    goto finish;

 finish:
  chop_object_destroy ((chop_object_t *)block_fetcher);
  chop_buffer_return (&ascii_object);

  return err;
#undef APPEND_COLON
}

chop_error_t
chop_ascii_deserialize_index_tuple_s1 (const char *buffer, size_t size,
				       const chop_class_t **indexer_class,
				       const chop_class_t **fetcher_class,
				       const chop_class_t **handle_class,
				       size_t *bytes_read)
{
  char *colon, *class_name;
  const char *start = buffer;

  /* Read the indexer class name.  */
  colon = strchr (buffer, ':');
  if (!colon)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  class_name = alloca (colon - buffer + 1);
  strncpy (class_name, buffer, colon - buffer);
  class_name[colon - buffer] = '\0';

  *indexer_class = chop_class_lookup (class_name);
  if (!*indexer_class)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  if (!chop_class_inherits (*indexer_class,
			    &chop_indexer_class))
    return CHOP_INVALID_ARG;

  /* Read the block fetcher class name.  */
  buffer = colon + 1;
  colon = strchr (buffer, ':');
  if (!colon)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  class_name = alloca (colon - buffer + 1);
  strncpy (class_name, buffer, colon - buffer);
  class_name[colon - buffer] = '\0';

  *fetcher_class = chop_class_lookup (class_name);
  if (!*fetcher_class)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  if (!chop_class_inherits (*fetcher_class,
			    &chop_block_fetcher_class))
    return CHOP_INVALID_ARG;

  /* Read the index handle class name.  */
  buffer = colon + 1;
  colon = strchr (buffer, ':');
  if (!colon)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  class_name = alloca (colon - buffer + 1);
  strncpy (class_name, buffer, colon - buffer);
  class_name[colon - buffer] = '\0';

  *handle_class = chop_class_lookup (class_name);
  if (!*handle_class)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  if (!chop_class_inherits (*handle_class,
			    &chop_index_handle_class))
    return CHOP_INVALID_ARG;

  *bytes_read = colon - start + 1;

  return 0;
}

chop_error_t
chop_ascii_deserialize_index_tuple_s2 (const char *buffer, size_t size,
				       const chop_class_t *indexer_class,
				       const chop_class_t *fetcher_class,
				       const chop_class_t *index_class,
				       chop_indexer_t *indexer,
				       chop_block_fetcher_t *fetcher,
				       chop_index_handle_t *handle,
				       size_t *bytes_read)
{
  chop_error_t err;
  size_t count;
  char *colon;

  /* Assuming that the serialized fetcher and the serialized handle are
     separated by a colon.  This means that they must not themself contain a
     colon.  */
  colon = strchr (buffer, ':');
  if (!colon)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  /* Deserialize the indexer.  */
  err = chop_object_deserialize ((chop_object_t *)indexer,
				 indexer_class, CHOP_SERIAL_ASCII,
				 buffer, colon - buffer, &count);
  if (err)
    return err;

  if (count != colon - buffer)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  *bytes_read = colon - buffer + 1;
  buffer = colon + 1;

  /* Deserialize the fetcher.  */
  colon = strchr (buffer, ':');
  if (!colon)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  err = chop_object_deserialize ((chop_object_t *)fetcher,
				 fetcher_class, CHOP_SERIAL_ASCII,
				 buffer, colon - buffer, &count);
  if (err)
    return err;

  if (count != colon - buffer)
    return CHOP_DESERIAL_CORRUPT_INPUT;

  *bytes_read += colon - buffer + 1;

  /* Deserialize the index handle.  */
  err = chop_object_deserialize ((chop_object_t *)handle,
				 index_class, CHOP_SERIAL_ASCII,
				 colon + 1, size - *bytes_read,
				 &count);
  if (err)
    return err;

  *bytes_read += count;

  return 0;
}
