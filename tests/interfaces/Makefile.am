check_PROGRAMS = filters stores

if HAVE_GPERF
check_PROGRAMS += class-lookup
endif

CPPFLAGS += -I$(top_srcdir)/include
CFLAGS += -Wall -g
LDADD = -L$(top_srcdir)/src -lchop
