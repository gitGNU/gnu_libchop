
bin_PROGRAMS = chop-archiver chop-store-list		\
               chop-show-anchors chop-show-similarities	\
	       chop-block-server chop-store-convert	\
	       chop-store-discover
noinst_PROGRAMS = chop-file chop-dbus-block-server
bin_SCRIPTS = chop-rcs.sh
EXTRA_DIST = avahi-publish.c

AM_CPPFLAGS = -I../include
AM_CFLAGS = -O0 -g -Wall -fno-strict-aliasing

AM_CPPFLAGS += -I$(srcdir)/src
LDADD = -L../src -lchop
chop_block_server_LDADD = -lchop-block-server -lchop $(AVAHI_LIBS) $(LDADD)
chop_block_server_CPPFLAGS = $(AM_CPPFLAGS) $(AVAHI_CPPFLAGS)
chop_dbus_block_server_LDADD = -lchop $(AVAHI_LIBS)
chop_dbus_block_server_CPPFLAGS = $(AM_CPPFLAGS) $(AVAHI_CPPFLAGS)

chop_store_discover_LDADD = -lchop-store-browsers $(LDADD)
