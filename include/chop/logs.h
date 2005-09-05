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

_CHOP_BEGIN_DECLS

typedef struct chop_log chop_log_t;

typedef void (* chop_log_user_logger_t) (chop_log_t *, const char *,
					 va_list);
typedef void (* chop_log_dtor_t) (chop_log_t *);


struct chop_log
{
  char *name;
  int attached;
  int eventually_close;
  int fd;

  chop_log_user_logger_t printf;
  void *data;
  chop_log_dtor_t dtor;
};



/* Initialize LOG with name NAME.  */
extern errcode_t chop_log_init (const char *name, chop_log_t *log);


/* Detach LOG from any associated file descriptor or user-provided logging
   method.  */
static __inline__ void chop_log_detach (chop_log_t *__log)
{
  if ((__log->attached) && (__log->eventually_close))
    {
      if (__log->printf)
	{
	  if (__log->dtor)
	    __log->dtor (__log);
	}
      else
	close (__log->fd);
    }

  __log->attached = 0;
  __log->fd = 0;
  __log->printf = NULL;
  __log->data = NULL;
  __log->dtor = NULL;
}

/* Close LOG.  */
static __inline__ void chop_log_close (chop_log_t *__log)
{
  chop_log_detach (__log);

  if (__log->name)
    free (__log->name);

  __log->name = NULL;
  __log->attached = 0;
  __log->fd = 0;
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
   DATA.  Upon closing or detaching LOG, if DTOR is not NULL then it will be
   called.  */
static __inline__ void
chop_log_attach_to_user (chop_log_t *__log,
			 chop_log_user_logger_t __printf,
			 void *__data,
			 chop_log_dtor_t __dtor)
{
  chop_log_detach (__log);
  __log->printf = __printf;
  __log->data = __data;
  __log->dtor = __dtor;
  __log->attached = 1;
}

/* Return user-provided data for LOG.  */
static __inline__ void *
chop_log_user_data (chop_log_t *__log)
{
  return (__log->data);
}

/* If LOG is attached to a user logger, then return it;  otherwise, return
   NULL.  */
static __inline__ chop_log_user_logger_t
chop_log_user_logger (chop_log_t *__log)
{
  return (__log->printf);
}


/* Return the name of LOG.  */
static __inline__ const char *
chop_log_name (const chop_log_t *__log)
{
  return (__log->name);
}

/* Change the name of LOG to NAME.  */
extern errcode_t chop_log_set_name (chop_log_t *log, const char *name);

/* Return non-zero if LOG is attached.  */
static __inline__ int
chop_log_attached (const chop_log_t *__log)
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

#ifdef __GNUC__

/* Since variadic functions cannot be inlined, we use a GCC variadic macro to
   make sure that this function doesn't imply any function when LOG is not
   attached.  */
#define chop_log_printf(__log, __args...)	\
do						\
{						\
  if ((__log)->attached)			\
    _chop_log_printf ((__log), __args);		\
}						\
while (0)

#else

/* Always make a function call.  */
#define _chop_log_printf chop_log_printf

#endif

/* Write a formatted message onto LOG.  */
static void _chop_log_printf (chop_log_t *__log,
			      const char *__fmt, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 2, 3)))
#endif
     ;

/* The following function cannot actually be inlined.  */
static __inline__ void _chop_log_printf (chop_log_t *__log,
					 const char *__fmt, ...)
{
  va_list __ap;

#ifndef __GNUC__
  if (!__log->attached)
    return;
#endif

  va_start (__ap, __fmt);
  if (__log->printf)
    __log->printf (__log, __fmt, __ap);
  else
    chop_log_builtin_printf (__log, __fmt, __ap);
  va_end (__ap);
}

#undef _chop_log_printf

_CHOP_END_DECLS

#endif
