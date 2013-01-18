/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010, 2013  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

#include <chop/chop-config.h>

#include <chop/chop.h>
#include <chop/block-indexers.h>

CHOP_DEFINE_RT_CLASS (block_indexer, object,
		      NULL, NULL,
		      NULL, NULL,
		      NULL, NULL);

CHOP_DEFINE_RT_CLASS (block_fetcher, object,
		      NULL, NULL,
		      NULL, NULL,
		      NULL, NULL);

CHOP_DEFINE_RT_CLASS (index_handle, object,
		      NULL, NULL,
		      NULL, NULL,
		      NULL, NULL);

/* arch-tag: ae556de6-c0ce-4cfc-98e0-1683dc5d60fc
 */
