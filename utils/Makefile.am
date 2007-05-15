
bin_PROGRAMS = chop-archiver chop-store-list		\
               chop-show-anchors chop-show-similarities	\
	       chop-block-server chop-store-convert	\
	       chop-store-discover
noinst_PROGRAMS = chop-file
dist_bin_SCRIPTS = chop-rcs.sh
EXTRA_DIST = gnutls-init-params.c gnutls-helper.h

AM_CPPFLAGS = -I../include
AM_CFLAGS = -O0 -g -Wall -fno-strict-aliasing

AM_CPPFLAGS += -I$(srcdir)/src
LDADD = -L$(top_builddir)/src -lchop


chop_archiver_SOURCES = chop-archiver.c

chop_block_server_SOURCES = chop-block-server.c
if HAVE_GNUTLS
chop_block_server_SOURCES += gnutls-helper.c
else
EXTRA_DIST += gnutls-helper.c
endif
chop_block_server_LDADD = -lchop-block-server -lchop $(AVAHI_LIBS) $(LDADD)
chop_block_server_CPPFLAGS = $(AM_CPPFLAGS) $(AVAHI_CPPFLAGS)

if HAVE_GNUTLS
bin_PROGRAMS += chop-openpgp-tool

chop_openpgp_tool_LDADD = $(LIBGNUTLS_LIBS) $(LIBGNUTLS_EXTRA_LIBS) \
			  -L$(top_builddir)/src -lchop
endif

if HAVE_DBUS
# The experimental D-BUS-based block server.

noinst_PROGRAMS += chop-dbus-block-server
chop_dbus_block_server_LDADD = -lchop $(AVAHI_LIBS)
chop_dbus_block_server_CPPFLAGS = $(AM_CPPFLAGS) $(AVAHI_CPPFLAGS)
endif

chop_store_discover_LDADD = -lchop-store-browsers $(LDADD)
