#ifndef __CHOP_TESTSUITE_H__
#define __CHOP_TESTSUITE_H__

/* Support functions for libchop's test suite.  */

#include <chop/chop-config.h>
#include <chop/chop.h>

#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <string.h>

#include <sys/time.h>
#include <time.h>

#ifdef HAVE_STDARG_H
# include <stdarg.h>
#endif

#ifdef __GNUC__
# define CHOP_TEST_PRINTF(x, y)  __attribute__ ((format (printf, x, y)))
#else
# define CHOP_TEST_PRINTF(x, y)
#endif


static const char *_test_program_name = NULL;


/* The basics.  */

static inline void
test_init (const char *prog_name)
{
  _test_program_name = prog_name;
  setvbuf (stdout, NULL, _IONBF, 0);
}

/* Initialize PRNG seed.  */
static inline void
test_init_random_seed (void)
{
  struct timeval tv;

  gettimeofday (&tv, NULL);
  srandom (tv.tv_sec);
}

/* Write SIZE poorly random bytes into buffer.  */
static inline void
test_randomize_input (char *buffer, size_t size)
{
  char *p;

  for (p = buffer; p < buffer + size; p++)
    *p = random ();
}



static void test_stage (const char *, ...)
  _CHOP_UNUSED CHOP_TEST_PRINTF (1, 2);
static void test_stage_intermediate (const char *, ...)
  _CHOP_UNUSED CHOP_TEST_PRINTF (1, 2);

static void
test_stage (const char *fmt, ...)
{
  char *msg;
  va_list ap;

  msg = alloca (strlen (fmt) + 50);
  strcpy (msg, "* testing ");
  strcat (msg, fmt);
  strcat (msg, "...  ");

  va_start (ap, fmt);
  vfprintf (stdout, msg, ap);
  va_end (ap);
}

static void
test_stage_intermediate (const char *fmt, ...)
{
  char *msg;
  va_list ap;

  msg = alloca (strlen (fmt) + 50);
  strcpy (msg, fmt);
  strcat (msg, "...  ");

  va_start (ap, fmt);
  vfprintf (stdout, msg, ap);
  va_end (ap);
}

static inline void
test_stage_result (int result)
{
  if (result)
    fprintf (stdout, "ok\n");
  else
    fprintf (stdout, "FAILED\n");
}


/* Debugging messages.  */

static void test_debug (const char *fmt, ...)
  _CHOP_UNUSED CHOP_TEST_PRINTF (1, 2);

static inline int
test_debug_mode (void)
{
  return (getenv ("CHOP_DEBUG") != NULL);
}

static void
test_debug (const char *fmt, ...)
{
  va_list ap;

  if (test_debug_mode ())
    {
      size_t fmt_len;
      char *new_fmt;

      fmt_len = strlen (fmt);
      new_fmt = alloca (fmt_len + 2);
      memcpy (new_fmt, fmt, fmt_len);
      new_fmt[fmt_len] = '\n';
      new_fmt[fmt_len + 1] = '\0';

      va_start (ap, fmt);
      vfprintf (stderr, new_fmt, ap);
      va_end (ap);
    }
}


/* Assertions.  */

static inline void
test_assertion_failed (const char *file, unsigned line, const char *expr)
{
  fprintf (stderr, "\n%s:%u: assertion failed: %s\n",
	   file, line, expr);
  abort ();
}

#define _TEST_STRINGIFY(_z) # _z
#define TEST_STRINGIFY(_x) _TEST_STRINGIFY (_x)

#define test_assert(_expr)						\
do									\
{									\
  if (!(_expr))								\
    test_assertion_failed (__FILE__, __LINE__, TEST_STRINGIFY (_expr));	\
}									\
while (0)

#define test_check_errcode(_err, _string)			\
do								\
{								\
  if ((_err))							\
    {								\
      com_err (_test_program_name, (_err), "while " _string);	\
      exit (1);							\
    }								\
}								\
while (0)


#endif
