check_PROGRAMS = filter-zlib stream-filtered chopper-anchor-based \
		 stream-indexing
TESTS = $(check_PROGRAMS)

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/tests/include
AM_CFLAGS = -O0 -g -Wall -fno-strict-aliasing
LDADD = -L$(top_srcdir)/src -lchop
