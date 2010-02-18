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
   themselves.

   arch-tag: 55b0ce3e-83aa-4802-8311-b42b2d32395a
   */

#include <chop/chop-config.h>

#include <stdlib.h>
#include <errno.h>
#include <assert.h>


/* Constructors.  */

static inline chop_error_t
chop_zlib_zip_filter_init_alloc (int compression, size_t input_size,
				 chop_filter_t **filter)
{
  chop_error_t err;

  *filter =
    gwrap_chop_malloc ((chop_class_t *) &chop_zlib_zip_filter_class);

  err = chop_zlib_zip_filter_init (compression, input_size, *filter);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) *filter,
	 (chop_class_t *) &chop_zlib_zip_filter_class);
      *filter = NULL;
    }

  return err;
}

static inline chop_error_t
chop_zlib_unzip_filter_init_alloc (size_t input_size,
				   chop_filter_t **filter)
{
  chop_error_t err;

  *filter =
    gwrap_chop_malloc ((chop_class_t *) &chop_zlib_unzip_filter_class);

  err = chop_zlib_unzip_filter_init (input_size, *filter);
  if (err)
    {
      gwrap_chop_free_uninitialized
	((chop_object_t *) *filter,
	 (chop_class_t *) &chop_zlib_unzip_filter_class);
      *filter = NULL;
    }

  return err;
}


/* Generic zip/unzip filters.  */

static chop_error_t
chop_generic_zip_filter_open_alloc (const char *class_nickname,
				    int compression_level, size_t input_size,
				    chop_filter_t **filter)
{
  chop_error_t err;
  char *class_name;
  chop_zip_filter_class_t *klass;

  class_name = (char *) alloca (strlen (class_nickname) + 20);
  strcpy (class_name, class_nickname);
  strcat (class_name, "_zip_filter");

  klass = (chop_zip_filter_class_t *) chop_class_lookup (class_name);
  if ((!klass) ||
      (!chop_object_is_a ((chop_object_t *) klass,
			  &chop_zip_filter_class_class)))
    err = CHOP_ERR_NOT_FOUND;
  else
    {
      *filter = gwrap_chop_malloc ((chop_class_t *) klass);
      err = chop_zip_filter_generic_open (klass, compression_level,
					  input_size, *filter);
      if (err)
	gwrap_chop_free_uninitialized ((chop_object_t *) *filter,
				       (chop_class_t *) klass);
    }

  return err;
}

static chop_error_t
chop_generic_unzip_filter_open_alloc (const char *class_nickname,
				      size_t input_size,
				      chop_filter_t **filter)
{
  chop_error_t err;
  char *class_name;
  chop_unzip_filter_class_t *klass;

  class_name = (char *) alloca (strlen (class_nickname) + 20);
  strcpy (class_name, class_nickname);
  strcat (class_name, "_unzip_filter");

  klass = (chop_unzip_filter_class_t *) chop_class_lookup (class_name);
  if ((!klass) ||
      (!chop_object_is_a ((chop_object_t *) klass,
			  &chop_unzip_filter_class_class)))
    err = CHOP_ERR_NOT_FOUND;
  else
    {
      *filter = gwrap_chop_malloc ((chop_class_t *) klass);
      err = chop_unzip_filter_generic_open (klass, input_size,
					    *filter);
      if (err)
	gwrap_chop_free_uninitialized ((chop_object_t *) *filter,
				       (chop_class_t *) klass);
    }

  return err;
}
