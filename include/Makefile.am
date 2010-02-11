# libchop -- a utility library for distributed storage
# Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
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


nobase_include_HEADERS = chop/block-indexers.h	\
			 chop/block-server.h	\
			 chop/block_rstore.h	\
			 chop/buffers.h		\
			 chop/chop-config.h	\
			 chop/chop-errors.h	\
			 chop/chop.h		\
			 chop/choppers.h	\
			 chop/filters.h		\
			 chop/hash.h		\
			 chop/cipher.h		\
			 chop/indexers.h	\
			 chop/logs.h		\
			 chop/objects.h		\
			 chop/store-stats.h	\
			 chop/store-browsers.h	\
			 chop/stores.h		\
			 chop/streams.h

if HAVE_GNUTLS
nobase_include_HEADERS += chop/sunrpc-tls.h chop/store-sunrpc-tls.h
else
nobase_noinst_HEADERS   = chop/sunrpc-tls.h chop/store-sunrpc-tls.h
endif

# Built in `src/'.
BUILT_SOURCES = chop/block_rstore.h chop/chop-errors.h
