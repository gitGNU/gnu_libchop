#ifndef __CHOP_LOGS_H__
#define __CHOP_LOGS_H__

/* Message logging, used for debugging purposes.

   This is designed in such a way that the default implementation (where
   messages are written to a file descriptor) can be made somewhat efficient
   while still allowing user extensions (e.g. logging to memory).  When a log
   is "detached", CHOP_LOG_PRINTF simply does nothing.  */

#include <chop/chop.h>
#include <unistd.h>
#include <stdlib.h>

typedef struct chop_log chop_log_t;

struct chop_log
{
  char *name;
  int attached;
  int eventually_close;
  int fd;
  void (* printf) (struct chop_log *, const char *, va_list);
  void *data;
};



/* Initialize LOG with name NAME.  */
extern errcode_t chop_log_init (const char *name, chop_log_t *log);

/* Close LOG.  */
static __inline__ void chop_log_close (chop_log_t *__log)
{
  if (!__log->attached)
    return;
  if (__log->eventually_close)
    close (__log->fd);
  if (__log->name)
    free (__log->name);
  __log->name = NULL;
  __log->attached = 0;
  __log->fd = 0;
}

/* Detach LOG from any associated file descriptor or user-provided logging
   method.  */
static __inline__ void chop_log_detach (chop_log_t *__log)
{
  if (!__log->attached)
    return;
  if ((!__log->printf) && (__log->eventually_close))
    close (__log->fd);
  __log->attached = 0;
  __log->fd = 0;
  __log->printf = NULL;
  __log->data = NULL;
}

/* Attach LOG to file descriptor FD.  If EVENTUALLY_CLOSE is non-zero, then
   eventually close FD when LOG is closed or detached.  */
static __inline__ void chop_log_attach (chop_log_t *__log, int __fd,
					int __eventually_close)
{
  chop_log_detach (__log);
  __log->fd = __fd;
  __log->eventually_close = __eventually_close;
  __log->attached = 1;
}

/* Attach LOG to the user-provided logging method PRINTF with specific data
   DATA.  */
static __inline__ void
chop_log_attach_to_user (chop_log_t *__log,
			 void (* __printf) (chop_log_t *, const char *,
					    va_list),
			 void *__data)
{
  chop_log_detach (__log);
  __log->printf = __printf;
  __log->data = __data;
  __log->attached = 1;
}

/* Return user-provided data for LOG.  */
static __inline__ void *
chop_log_user_data (chop_log_t *__log)
{
  return (__log->data);
}

/* Return the name of LOG.  */
static __inline__ const char *chop_log_name (const chop_log_t *__log)
{
  return (__log->name);
}

/* Change the name of LOG to NAME.  */
extern errcode_t chop_log_set_name (chop_log_t *log, const char *name);

/* Return non-zero if LOG is attached.  */
static __inline__ int chop_log_attached (const chop_log_t *__log)
{
  return (__log->attached);
}

/* Have CHILD mimic PARENT.  If TAKEOVER is true, the CHILD becomes
   responsible of closing the underlying "store".  */
extern void chop_log_mimic (chop_log_t *child, chop_log_t *parent,
			    int takeover);

/* The default logging implementation: it writes messages to LOG's file
   descriptor.  */
extern void chop_log_builtin_printf (chop_log_t *__log,
				     const char *__fmt, va_list);

/* Write a formatted message onto LOG.  */
static __inline__ void chop_log_printf (chop_log_t *__log,
					const char *__fmt, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 2, 3)))
#endif
     ;

static __inline__ void chop_log_printf (chop_log_t *__log,
					const char *__fmt, ...)
{
  va_list __ap;

  if (!__log->attached)
    return;

  va_start (__ap, __fmt);
  if (__log->printf)
    __log->printf (__log, __fmt, __ap);
  else
    chop_log_builtin_printf (__log, __fmt, __ap);
  va_end (__ap);
}

#endif
