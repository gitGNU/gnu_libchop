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

#include <chop/chop-config.h>

#include <alloca.h>

#include <chop/logs.h>

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>


static chop_error_t
log_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_log_t *log = (chop_log_t *)object;

  log->attached = 0;
  log->fd = 1;
  log->eventually_close = 0;
  log->printf = NULL;
  log->data = NULL;
  log->dtor = NULL;
  log->copy_ctor = NULL;

  log->name = NULL;

  return 0;
}

static chop_error_t
log_copy (const chop_object_t *s, chop_object_t *d)
{
  chop_log_t *source, *dest;

  source = (chop_log_t *)s;
  dest = (chop_log_t *)d;

  /* XXX: This is quite approximative.  */
  chop_log_mimic (dest, source, 0);

  return 0;
}

static void
log_dtor (chop_object_t *object)
{
  chop_log_t *log = (chop_log_t *)object;

  chop_log_close (log);

  if (log->name)
    chop_free (log->name, &chop_log_class);

  log->name = NULL;
  log->attached = 0;
  log->fd = 0;
}

CHOP_DEFINE_RT_CLASS (log, object,
		      log_ctor, log_dtor,
		      log_copy, NULL,
		      NULL, NULL);


chop_error_t
chop_log_init (const char *name, chop_log_t *log)
{
  chop_error_t err;
  char *log_name;

  log_name = chop_strdup (name, &chop_log_class);
  if (!log_name)
    return ENOMEM;

  err = chop_object_initialize ((chop_object_t *)log, &chop_log_class);
  if (err)
    {
      chop_free (log_name, &chop_log_class);
      return err;
    }

  log->name = log_name;

  return 0;
}

chop_error_t
chop_log_set_name (chop_log_t *log, const char *name)
{
  if (log->name)
    chop_free (log->name, &chop_log_class);

  log->name = chop_strdup (name, &chop_log_class);
  if (!log->name)
    return ENOMEM;

  return 0;
}

void
chop_log_mimic (chop_log_t *log, chop_log_t *parent, int takeover)
{
  log->attached = parent->attached;
  log->fd = parent->fd;

  if (parent->copy_ctor)
    {
      /* This should initialize the PRINTF, DATA and DTOR fields, perhaps
	 copying them.  */
      parent->copy_ctor (log, parent);
    }
  else if (parent->eventually_close)
    {
      log->printf = parent->printf;
      log->data = parent->data;
      log->dtor = takeover ? parent->dtor : NULL;
      parent->dtor = takeover ? NULL : parent->dtor;
      log->copy_ctor = NULL;

      if (takeover)
	{
	  log->eventually_close = 1;
	  log->dtor = parent->dtor;
	  parent->eventually_close = 0;
	}
      else
	log->eventually_close = 0;
    }
  else
    {
      log->eventually_close = 0;
      log->dtor = NULL;
      log->copy_ctor = parent->copy_ctor;
      log->printf = parent->printf;
      log->data = parent->data;
    }
}

void
chop_log_builtin_printf (chop_log_t *log, const char *fmt, va_list ap)
{
  size_t log_name_len, fmt_len;
  char *new_fmt, *str = NULL;

  if (!log->attached)
    return;

  /* Modify the message format */
  log_name_len = strlen (log->name);
  fmt_len = strlen (fmt);
  new_fmt = alloca (log_name_len + 2 + fmt_len + 1 + 1);
  if (!new_fmt)
    return;

  strcpy (new_fmt, log->name);
  strcpy (new_fmt + log_name_len, ": ");
  strcpy (new_fmt + log_name_len + 2, fmt);
  strcpy (new_fmt + log_name_len + 2 + fmt_len, "\n");

  vasprintf (&str, new_fmt, ap);

  if (!str)
    return;

  write (log->fd, str, strlen (str));

  /* Free the string allocated by `vasprintf ()'.  */
  free (str);
}
