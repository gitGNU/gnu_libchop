
bin_PROGRAMS = chop-archiver chop-store-list		\
               chop-show-anchors chop-show-similarities	\
	       chop-block-server chop-store-convert
noinst_PROGRAMS = chop-file
bin_SCRIPTS = chop-rcs.sh
EXTRA_DIST = avahi-publish.c

AM_CPPFLAGS = -I$(srcdir)/src
LDADD = -L../src -lchop
chop_block_server_LDADD = $(LDADD) -lchop-block-server

AM_CPPFLAGS += -I../include
AM_CFLAGS = -O0 -g -Wall -fno-strict-aliasing
