check_PROGRAMS = filters stores choppers cipher block-indexers indexers

if HAVE_GPERF
check_PROGRAMS += class-lookup
endif

TESTS = $(check_PROGRAMS)

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/tests/include	\
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib
AM_CFLAGS = -Wall -g
LDADD = -L$(top_builddir)/src -lchop		\
	-L$(top_builddir)/lib -lgnu
