#include <chop/logs.h>

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

errcode_t
chop_log_init (const char *name, chop_log_t *log)
{
  log->attached = 0;
  log->fd = 1;
  log->eventually_close = 0;
  log->printf = NULL;
  log->data = NULL;

  log->name = strdup (name);
  if (!log->name)
    return ENOMEM;

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
  if (parent->eventually_close)
    {
      if (takeover)
	{
	  log->eventually_close = 1;
	  parent->eventually_close = 0;
	}
      else
	log->eventually_close = 0;
    }
  else
    log->eventually_close = 0;

  log->printf = parent->printf;
  log->data = parent->data;
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
