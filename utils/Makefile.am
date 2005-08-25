
bin_PROGRAMS = chop-file chop-archiver chop-store-list-entries	\
               chop-show-anchors chop-show-similarities		\
	       chop-block-server
bin_SCRIPTS = chop-rcs.sh

CPPFLAGS += -I$(srcdir)/src
LDADD = -L../src -lchop -lcom_err -lgdbm
chop_block_server_LDADD = $(LDADD) -lchop-block-server

CPPFLAGS += -I../include
CFLAGS += -O0 -g -Wall
