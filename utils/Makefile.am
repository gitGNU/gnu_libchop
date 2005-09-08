
bin_PROGRAMS = chop-file chop-archiver chop-store-list	\
               chop-show-anchors chop-show-similarities	\
	       chop-block-server chop-store-convert
bin_SCRIPTS = chop-rcs.sh

AM_CPPFLAGS = -I$(srcdir)/src
LDADD = -L../src -lchop
chop_block_server_LDADD = $(LDADD) -lchop-block-server

AM_CPPFLAGS += -I../include
AM_CFLAGS = -O0 -g -Wall
