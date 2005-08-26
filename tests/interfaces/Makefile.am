check_PROGRAMS = filters stores choppers cipher

if HAVE_GPERF
check_PROGRAMS += class-lookup
endif

TESTS = $(check_PROGRAMS)

CPPFLAGS += -I$(top_srcdir)/include
CFLAGS += -Wall -g
LDADD = -L$(top_srcdir)/src -lchop
