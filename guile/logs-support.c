/* Support functions for the log interface wrapping.  */

#define _GNU_SOURCE 1
#include <stdio.h>

static __inline__ void
_chop_log_destroy (chop_log_t *log)
{
  chop_log_close (log);
}

static __inline__ void
chop_log_attach_to_port (chop_log_t *log, SCM port)
{
  int fd;

  scm_fsync (port);
  fd = scm_to_int (scm_fileno (port));
  chop_log_attach (log, fd, 0);
}

static void
scm_log_printf (chop_log_t *log, const char *fmt, va_list args)
{
  char *str;
  SCM   proc = (SCM)chop_log_user_data (log);

#ifdef __GNU_LIBRARY__
  vasprintf (&str, fmt, args);
#else
# error "You should consider using the GNU libc."
#endif

  scm_call_1 (proc, scm_take_locale_string (str));
}

static void
scm_log_dtor (chop_log_t *log)
{
  scm_gc_unprotect_object ((SCM)chop_log_user_data (log));
}

static __inline__ void
chop_log_attach_to_scheme_user (chop_log_t *log, SCM proc)
{
  /* Protect PROC against garbage collection.  We'll unprotect it in
     `scm_log_dtor ()', that is, when LOG gets detached.  */
  scm_gc_protect_object (proc);

  chop_log_attach_to_user (log, scm_log_printf, (void *)proc,
			   scm_log_dtor);
}