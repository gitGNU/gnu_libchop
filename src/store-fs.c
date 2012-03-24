/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2010, 2011, 2012  Ludovic Courtès <ludo@gnu.org>

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

/* The file system as a key-value database.  This is similar to Git's object
   store on the file system.  */

#include <chop/chop-config.h>

#include <chop/chop.h>
#include <chop/stores.h>
#include <chop/buffers.h>
#include <chop/logs.h>
#include <alloca.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <libgen.h>
#include <assert.h>

#include <full-read.h>
#include <full-write.h>


/* Class definitions.  */

CHOP_DECLARE_RT_CLASS_WITH_METACLASS (fs_block_store, block_store,
				      file_based_store_class,
				      int dir_fd;
				      int eventually_close;);

static chop_error_t chop_fs_close (chop_block_store_t *);
static chop_error_t chop_fs_next_block (chop_block_iterator_t *it);


/* A generic open method, common to all file-based block stores.  */
static chop_error_t
chop_fs_generic_open (const chop_class_t *class,
		      const char *file, int open_flags, mode_t mode,
		      chop_block_store_t *store)
{
  chop_error_t err;

  if ((chop_file_based_store_class_t *) class != &chop_fs_block_store_class)
    return CHOP_INVALID_ARG;

  err = mkdir (file, mode | S_IXUSR);
  if (err == 0 || errno == EEXIST)
    {
      int dir_fd;

      dir_fd = open (file, O_RDONLY | O_DIRECTORY);
      if (dir_fd < 0)
	err = errno;
      else
	err = chop_fs_store_open (dir_fd, 1, store);
    }
  else
    err = errno;

  return err;
}

static void
fss_dtor (chop_object_t *object)
{
  chop_fs_block_store_t *fs =
    (chop_fs_block_store_t *) object;

  if (fs->dir_fd >= 0)
    chop_fs_close (&fs->block_store);
}

CHOP_DEFINE_RT_CLASS_WITH_METACLASS (fs_block_store, block_store,
				     file_based_store_class,

				     /* metaclass inits */
				     .generic_open = chop_fs_generic_open,

				     NULL, fss_dtor,
				     NULL, NULL, /* No copy/equalp */
				     NULL, NULL  /* No serial/deserial */);


/* Iterators.  */

CHOP_DECLARE_RT_CLASS (fs_block_iterator, block_iterator,
		       int top_dir_fd;
		       int subdir_fd;
		       char subdir_name[3];
		       DIR *top_dir;
		       DIR *subdir;
		       struct dirent top_entry;
		       struct dirent sub_entry;)

static chop_error_t
fsi_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_fs_block_iterator_t *fsit = (chop_fs_block_iterator_t *) object;

  fsit->block_iterator.next = chop_fs_next_block;

  fsit->top_dir_fd = fsit->subdir_fd = -1;
  fsit->top_dir = fsit->subdir = NULL;

  return 0;
}

static void
fsi_dtor (chop_object_t *object)
{
  chop_fs_block_iterator_t *fsit = (chop_fs_block_iterator_t *) object;

  /* Close the directories.  The associated file descriptors get closed at
     the same time.  */

  if (fsit->top_dir != NULL)
    closedir (fsit->top_dir);
  if (fsit->subdir != NULL)
    closedir (fsit->subdir);
}

CHOP_DEFINE_RT_CLASS (fs_block_iterator, block_iterator,
		      fsi_ctor, fsi_dtor,
		      NULL, NULL,
		      NULL, NULL);


/* Set NAME to the relative file name for KEY.  NAME must be twice the size
   of KEY plus 2 bytes (for the `/' and `\0'.)  */
static void
block_file_name (const chop_block_key_t *key, char *name)
{
  char buffer[chop_block_key_size (key) * 2 + 1];

  chop_buffer_to_base32_string (chop_block_key_buffer (key),
				chop_block_key_size (key),
				buffer);
  assert (strlen (buffer) > 2);
  assert (strlen (buffer) < chop_block_key_size (key) * 2 + 1);

  memcpy (name, buffer, 2);
  strcpy (&name[2], "/");
  strcat (name, &buffer[2]);
}

/* Return true if NAME is "." or "..".  */
static inline bool
dot_or_dot_dot (const char *name)
{
  return (name[0] == '.'
	  && (name[1] == '\0'
	      || (name[1] == '.' && name[2] == '\0')));
}

static chop_error_t
chop_fs_block_exists (chop_block_store_t *store,
		      const chop_block_key_t *key,
		      int *exists)
{
  chop_error_t err;
  struct stat stat;
  chop_fs_block_store_t *fs =
    (chop_fs_block_store_t *) store;
  char file_name[chop_block_key_size (key) * 2 + 2];

  block_file_name (key, file_name);
  err = fstatat (fs->dir_fd, file_name, &stat, 0);

  if (err == 0)
    *exists = 1;
  else
    {
      if (errno == ENOENT)
	*exists = 0, err = 0;
      else
	*exists = 0, err = errno;
    }

  return err;
}

static chop_error_t
chop_fs_read_block (chop_block_store_t *store,
		    const chop_block_key_t *key,
		    chop_buffer_t *buffer,
		    size_t *size)
{
  chop_error_t err;
  chop_fs_block_store_t *fs =
    (chop_fs_block_store_t *) store;
  char file_name[chop_block_key_size (key) * 2 + 2];
  int fd;

  *size = 0;

  block_file_name (key, file_name);
  fd = openat (fs->dir_fd, file_name, O_RDONLY);

  if (fd < 0)
    {
      if (errno == ENOENT)
	err = CHOP_STORE_BLOCK_UNAVAIL;
      else
	err = errno;
    }
  else
    {
      size_t count, total = 0;

      do
	{
	  char data[1024];

	  count = full_read (fd, data, sizeof (data));
	  if (count > 0)
	    {
	      chop_buffer_append (buffer, data, count);
	      total += count;
	    }
	}
      while (count > 0);

      if (errno != 0)
	err = errno;
      else
	err = 0, *size = total;

      close (fd);
    }

  return err;
}

static chop_error_t
chop_fs_write_block (chop_block_store_t *store,
		     const chop_block_key_t *key,
		     const char *block, size_t size)
{
  int fd;
  bool new_dir = false;
  chop_error_t err = 0;
  char file_name[chop_block_key_size (key) * 2 + 2];
  char *dir_name;
  chop_fs_block_store_t *fs =
    (chop_fs_block_store_t *) store;

  block_file_name (key, file_name);
  dir_name = dirname (strdupa (file_name));

 try:
  /* Speculate that DIR_NAME already exists.  In practice, this is the case
     most of the time when a store is populated since there are only 1024
     possible values for DIR_NAME.  */
  fd = openat (fs->dir_fd, file_name, O_CREAT | O_WRONLY,
	       S_IRUSR | S_IWUSR);
  if (fd >= 0)
    {
      size_t count;

      if (!new_dir)
	/* Overwrite the existing block, if any.  */
	ftruncate (fd, 0);

      count = full_write (fd, block, size);

      if (count < size)
	err = errno;
      else
	err = 0;

      close (fd);
    }
  else if (errno == ENOENT)
    {
      /* DIR_NAME doesn't exist yet.  */
      new_dir = true;
      err = mkdirat (fs->dir_fd, dir_name, S_IRWXU);
      if (err == 0)
	goto try;
      else if (errno == EEXIST)
	/* DIR_NAME was created in the meantime.  */
	goto try;
      else
	err = errno;
    }
  else
    err = errno;

  return err;
}

static chop_error_t
chop_fs_delete_block (chop_block_store_t *store,
		      const chop_block_key_t *key)
{
  chop_error_t err;
  chop_fs_block_store_t *fs =
    (chop_fs_block_store_t *) store;
  char file_name[chop_block_key_size (key) * 2 + 2];

  block_file_name (key, file_name);
  err = unlinkat (fs->dir_fd, file_name, 0);

  if (err != 0)
    {
      if (errno == ENOENT)
	err = CHOP_STORE_BLOCK_UNAVAIL;
      else
	err = errno;
    }
  else
    {
      /* Try to remove the directory containing FILE_NAME.  */
      char *dir_name;

      dir_name = dirname (file_name);
      err = unlinkat (fs->dir_fd, dir_name, AT_REMOVEDIR);

      if (err != 0)
	{
	  if (errno == ENOTEMPTY || errno == EEXIST)
	    /* There are other entries in this directory.  */
	    err = 0;
	  else
	    err = errno;
	}
    }

  return err;
}

static void
free_key (char *key, void *unused)
{
  free (key);
}

static chop_error_t
chop_fs_first_block (chop_block_store_t *store,
		     chop_block_iterator_t *it)
{
  chop_error_t err;
  chop_fs_block_iterator_t *fsit = (chop_fs_block_iterator_t *) it;

  err = chop_object_initialize ((chop_object_t *) it,
				&chop_fs_block_iterator_class);
  if (err == 0)
    {
      struct dirent *result;
      chop_fs_block_store_t *fs = (chop_fs_block_store_t *) store;

      fsit->top_dir_fd = dup (fs->dir_fd);
      if (fsit->top_dir_fd < 0)
	{
	  err = errno;
	  goto error;
	}

      fsit->top_dir = fdopendir (fsit->top_dir_fd);
      if (fsit->top_dir == NULL)
	{
	  err = errno;
	  goto error;
	}

      do
	{
	  err = readdir_r (fsit->top_dir, &fsit->top_entry, &result);
	  if (result == NULL)
	    {
	      err = CHOP_STORE_END;
	      goto error;
	    }
	  if (err != 0)
	    {
	      err = errno;
	      goto error;
	    }
	}
      while (dot_or_dot_dot (fsit->top_entry.d_name));

      memcpy (fsit->subdir_name, fsit->top_entry.d_name, 2);
      fsit->subdir_name[2] = '\0';

      fsit->subdir_fd = openat (fsit->top_dir_fd, fsit->top_entry.d_name,
				O_DIRECTORY | O_RDONLY);
      if (fsit->subdir_fd < 0)
	{
	  err = errno;
	  goto error;
	}

      fsit->subdir = fdopendir (fsit->subdir_fd);
      if (fsit->subdir == NULL)
	{
	  err = errno;
	  goto error;
	}

      do
	{
	  err = readdir_r (fsit->subdir, &fsit->sub_entry, &result);
	  if (result == NULL)
	    {
	      err = CHOP_STORE_END;
	      goto error;
	    }
	  if (err != 0)
	    {
	      err = errno;
	      goto error;
	    }
	}
      while (dot_or_dot_dot (fsit->sub_entry.d_name));

      fsit->block_iterator.nil = 0;

      /* We have an entry, so compute the corresponding key and store it in
	 IT.  */
      char base32[2 + fsit->sub_entry.d_reclen + 1];
      strcpy (base32, fsit->subdir_name);
      memcpy (base32 + 2, fsit->sub_entry.d_name, fsit->sub_entry.d_reclen);
      base32[2 + fsit->sub_entry.d_reclen] = '\0';

      char *raw;
      const char *end;
      size_t key_size;
      raw = malloc (sizeof base32);
      if (raw == NULL)
	return ENOMEM;
      key_size = chop_base32_string_to_buffer (base32, sizeof base32 - 1,
					       raw, &end);

      chop_block_key_init (&fsit->block_iterator.key,
			   raw, key_size, free_key, NULL);

      return 0;
    }

  return err;

 error:
  chop_object_destroy ((chop_object_t *) it);
  return err;
}

static chop_error_t
chop_fs_next_block (chop_block_iterator_t *it)
{
  chop_error_t err;
  struct dirent *result;
  chop_fs_block_iterator_t *fsit = (chop_fs_block_iterator_t *) it;

 retry:
  do
    {
      err = readdir_r (fsit->subdir, &fsit->sub_entry, &result);

      if (result == NULL)
	{
	  /* We're done with this sub-directory; jump to the next one.  */
	  closedir (fsit->subdir);
	  fsit->subdir_fd = -1;
	  fsit->subdir = NULL;

	  do
	    {
	      err = readdir_r (fsit->top_dir, &fsit->top_entry, &result);
	      if (result == NULL)
		{
		  err = CHOP_STORE_END;
		  fsit->block_iterator.nil = 1;
		}
	      else if (err != 0)
		err = errno;
	    }
	  while (err == 0 && dot_or_dot_dot (fsit->top_entry.d_name));

	  if (err == 0)
	    {
	      memcpy (fsit->subdir_name, fsit->top_entry.d_name, 2);

	      fsit->subdir_fd = openat (fsit->top_dir_fd, fsit->top_entry.d_name,
					O_DIRECTORY | O_RDONLY);
	      if (fsit->subdir_fd < 0)
		err = errno;
	      else
		{
		  fsit->subdir = fdopendir (fsit->subdir_fd);
		  if (fsit->subdir == NULL)
		    err = errno;
		  else
		    /* We entered the next sub-directory, so try again.  */
		    goto retry;
		}
	    }
	}
      else if (err != 0)
	err = errno;
    }
  while (err == 0 && dot_or_dot_dot (fsit->sub_entry.d_name));

  if (err == 0)
    {
      /* We have an entry, so compute the corresponding key and store it in
	 IT.  */
      char base32[2 + fsit->sub_entry.d_reclen + 1];
      strcpy (base32, fsit->subdir_name);
      memcpy (base32 + 2, fsit->sub_entry.d_name, fsit->sub_entry.d_reclen);
      base32[2 + fsit->sub_entry.d_reclen] = '\0';

      char *raw;
      const char *end;
      size_t key_size;
      raw = malloc (sizeof base32);
      if (raw == NULL)
	return ENOMEM;
      key_size = chop_base32_string_to_buffer (base32, sizeof base32 - 1,
					       raw, &end);

      chop_block_key_init (&fsit->block_iterator.key,
			   raw, key_size, free_key, NULL);
    }

  return err;
}

static chop_error_t
chop_fs_sync (chop_block_store_t *store)
{
  return 0;
}

static chop_error_t
chop_fs_close (chop_block_store_t *store)
{
  chop_fs_block_store_t *fs =
    (chop_fs_block_store_t *) store;

  if (fs->eventually_close && fs->dir_fd >= 0)
    close (fs->dir_fd);

  fs->dir_fd = -1;

  return 0;
}


chop_error_t
chop_fs_store_open (int dir_fd, int eventually_close,
		    chop_block_store_t *store)
{
  chop_error_t err;
  char *log_name;
  chop_fs_block_store_t *fs =
    (chop_fs_block_store_t *)store;

  log_name = alloca (10);
  snprintf (log_name, 10, "fs/%i", dir_fd);

  err = chop_object_initialize ((chop_object_t *) store,
				(chop_class_t *) &chop_fs_block_store_class);
  if (err)
    return err;

  store->name = chop_strdup (log_name,
			     (chop_class_t *) &chop_fs_block_store_class);
  store->iterator_class = &chop_fs_block_iterator_class;
  store->block_exists = chop_fs_block_exists;
  store->read_block = chop_fs_read_block;
  store->write_block = chop_fs_write_block;
  store->delete_block = chop_fs_delete_block;
  store->first_block = chop_fs_first_block;
  store->close = chop_fs_close;
  store->sync = chop_fs_sync;

  fs->dir_fd = dir_fd;
  fs->eventually_close = eventually_close;

  return 0;
}
