/* libchop -- a utility library for distributed storage and data backup
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
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

/* Convenience macro for mapping libgcrypt's enums into libchop's.  */

#define MAKE_ENUM_MAPPING_FUNCTIONS(_type, _last, _map, _maptype)	\
									\
const char *								\
chop_ ## _type  ## _name (chop_ ## _type ## _t method)			\
{									\
  if ((int)method > ((int)(_last)))					\
    return NULL;							\
									\
  return ((_map)[(int)method].name);					\
}									\
									\
int									\
chop_ ## _type ## _gcrypt_name (chop_ ## _type ## _t method)		\
{									\
  if ((int)method > ((int)(_last)))					\
    return 0;								\
									\
  return ((_map)[(int)method].gcrypt_name);				\
}									\
									\
chop_error_t								\
chop_ ## _type ## _lookup (const char *name,				\
			   chop_ ## _type ## _t *method)		\
{									\
  const _maptype *p;							\
  for (p = (_map); p->name != NULL; p++)				\
    {									\
      if (!strcasecmp (p->name, name))					\
	break;								\
    }									\
									\
  if (p->name)								\
    {									\
      *method = p->chop_name;						\
      return 0;								\
    }									\
									\
  return CHOP_ERR_NOT_FOUND;						\
}
