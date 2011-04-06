# libchop -- a utility library for distributed storage and data backup
# Copyright (C) 2008, 2010, 2011  Ludovic Courtès <ludo@gnu.org>
# Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)
#
# Libchop is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Libchop is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with libchop.  If not, see <http://www.gnu.org/licenses/>.

EXTRA_DIST =

bin_PROGRAMS = chop-archiver chop-store-list		\
               chop-show-anchors chop-show-similarities	\
	       chop-block-server chop-store-convert

if HAVE_AVAHI

bin_PROGRAMS += chop-store-discover
chop_store_discover_LDADD = -lchop-store-browsers $(LDADD)

else !HAVE_AVAHI

EXTRA_DIST += chop-store-discover.c

endif !HAVE_AVAHI

noinst_PROGRAMS = chop-file
bin_SCRIPTS = chop-vcs
EXTRA_DIST += gnutls-init-params.c gnutls-helper.h zip-helper.c

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include	\
	      -I$(top_srcdir)/lib     -I$(top_builddir)/lib

LDADD = -L$(top_builddir)/src -lchop -L$(top_builddir)/lib -lgnu


chop_archiver_SOURCES = chop-archiver.c

chop_block_server_SOURCES = chop-block-server.c
if HAVE_GNUTLS
chop_block_server_SOURCES += gnutls-helper.c
else
EXTRA_DIST += gnutls-helper.c
endif
chop_block_server_LDADD = -lchop-block-server -lchop $(LDADD)
chop_block_server_CPPFLAGS = $(AM_CPPFLAGS)

if HAVE_GNUTLS
bin_PROGRAMS += chop-openpgp-tool

chop_openpgp_tool_CFLAGS =					\
  $(AM_CFLAGS) $(LIBGNUTLS_CFLAGS) $(LIBGNUTLS_EXTRA_CFLAGS)
chop_openpgp_tool_LDADD = $(LIBGNUTLS_LIBS) $(LIBGNUTLS_EXTRA_LIBS) \
			  -L$(top_builddir)/src -lchop
endif

if HAVE_DBUS
# The experimental D-BUS-based block server.

noinst_PROGRAMS += chop-dbus-block-server
chop_dbus_block_server_LDADD = -lchop $(AVAHI_LIBS)
chop_dbus_block_server_CPPFLAGS = $(AM_CPPFLAGS)
endif
