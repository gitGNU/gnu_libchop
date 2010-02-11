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

/* Support functions for the log interface wrapping.  */

#include <chop/chop-config.h>

#include <stdlib.h>
#include <stdio.h>

#include "core-support.h"


static inline void
chop_log_attach_to_port (chop_log_t *log, SCM port)
#define FUNC_NAME "log-attach-to-port"
{
  int fd;

  /* XXX: This doesn't work with on-file ports. */
  SCM_VALIDATE_OPFPORT (2, port);

  /* Not all file output ports are syncable (e.g., stdout is not) so let's
     avoid it.  */
  /* scm_fsync (port); */
  fd = scm_to_int (scm_fileno (port));
  chop_log_attach (log, fd, 0);
}
#undef FUNC_NAME


static void
scm_log_printf (chop_log_t *log, const char *fmt, va_list args)
{
  char *str;
  SCM   proc = (SCM)chop_log_user_data (log);

  vasprintf (&str, fmt, args);

  scm_call_1 (proc, scm_take_locale_string (str));
}

static void
scm_log_dtor (chop_log_t *log)
{
  scm_gc_unprotect_object ((SCM)chop_log_user_data (log));
}

static void
scm_log_copy_ctor (chop_log_t *child, const chop_log_t *parent)
{
  chop_log_user_logger_t logger;
  SCM proc;

  logger = chop_log_user_logger (parent);
  proc = (SCM)chop_log_user_data (parent);

  /* Increment the ref count (or equivalently) of PROC.  */
  scm_gc_protect_object (proc);

  chop_log_attach_to_user (child, scm_log_printf, (void *)proc,
			   scm_log_dtor, scm_log_copy_ctor);
}

static inline void
chop_log_attach_to_scheme_user (chop_log_t *log, SCM proc)
{
  /* Protect PROC against garbage collection.  We'll unprotect it in
     `scm_log_dtor ()', that is, when LOG gets detached.  */
  scm_gc_protect_object (proc);

  chop_log_attach_to_user (log, scm_log_printf, (void *)proc,
			   scm_log_dtor, scm_log_copy_ctor);
}
