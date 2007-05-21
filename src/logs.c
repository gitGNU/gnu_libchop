#include <alloca.h>

#include <chop/logs.h>

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>


static errcode_t
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

static errcode_t
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
    free (log->name);

  log->name = NULL;
  log->attached = 0;
  log->fd = 0;
}

CHOP_DEFINE_RT_CLASS (log, object,
		      log_ctor, log_dtor,
		      log_copy, NULL,
		      NULL, NULL);


errcode_t
chop_log_init (const char *name, chop_log_t *log)
{
  errcode_t err;
  char *log_name;

  log_name = strdup (name);
  if (!log_name)
    return ENOMEM;

  err = chop_object_initialize ((chop_object_t *)log, &chop_log_class);
  if (err)
    {
      free (log_name);
      return err;
    }

  log->name = log_name;

  return 0;
}

errcode_t
chop_log_set_name (chop_log_t *log, const char *name)
{
  if (log->name)
    free (log->name);

  log->name = strdup (name);
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

  free (str);
}
