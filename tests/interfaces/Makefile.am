check_PROGRAMS = filters stores choppers cipher block-indexers indexers

if HAVE_GPERF
check_PROGRAMS += class-lookup
endif

TESTS = $(check_PROGRAMS)

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include	\
	      -I$(top_srcdir)/lib     -I$(top_builddir)/lib	\
	      -I$(top_srcdir)/tests/include
AM_CFLAGS = -Wall -g
LIBS = $(top_builddir)/src/libchop.la		\
       $(top_builddir)/lib/libgnu.la

distclean-local:
	rm -f ,,t-*.db
