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
errcode_t								\
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
