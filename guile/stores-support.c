/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <stdlib.h>
#include <errno.h>
#include <assert.h>

static __inline__ chop_block_store_t *
chop_dummy_block_store_open_alloc (const char *name)
{
  chop_block_store_t *store;

  store = malloc (chop_class_instance_size (&chop_dummy_block_store_class));
  if (!store)
    return NULL;

  chop_dummy_block_store_open (name, store);

  return store;
}

static __inline__ chop_block_store_t *
chop_dummy_proxy_block_store_open_alloc (const char *name,
					 chop_block_store_t *backend)
{
  chop_block_store_t *store;

  store = malloc (chop_class_instance_size (&chop_dummy_block_store_class));
  if (!store)
    return NULL;

  chop_dummy_proxy_block_store_open (name, backend, store);

  return store;
}

static __inline__ chop_block_store_t *
chop_smart_block_store_open_alloc (chop_block_store_t *backend)
{
  chop_block_store_t *store;

  store = malloc (chop_class_instance_size (&chop_smart_block_store_class));
  if (!store)
    return NULL;

  chop_smart_block_store_open (backend, store);

  return store;
}


static __inline__ errcode_t
chop_file_based_store_open_alloc (const char *class_nickname,
				  const char *file, int open_flags,
				  mode_t mode,
				  chop_block_store_t **store)
{
  errcode_t err;
  char *class_realname;
  const chop_class_t *class;

  class_realname = alloca (strlen (class_nickname) + 20);
  strcpy (class_realname, class_nickname);
  strcat (class_realname, "_block_store");

  class = chop_class_lookup (class_realname);
  if (!class)
    return CHOP_ERR_NOT_FOUND;

  if (chop_object_get_class ((chop_object_t *)class)
      != &chop_file_based_store_class_class)
    return CHOP_INVALID_ARG;

  *store = malloc (chop_class_instance_size (class));
  if (!*store)
    return ENOMEM;

  err = chop_file_based_store_open ((chop_file_based_store_class_t *)class,
				    file, open_flags, mode,
				    *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}


static __inline__ errcode_t
chop_gdbm_block_store_open_alloc (const char *name, size_t block_size,
				  int open_flags, mode_t mode,
				  chop_block_store_t **store)
{
  errcode_t err;

  *store = malloc
    (chop_class_instance_size ((chop_class_t *)&chop_gdbm_block_store_class));
  if (!*store)
    return ENOMEM;

  err = chop_gdbm_store_open (name, block_size, open_flags, mode, NULL,
			      *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_tdb_block_store_open_alloc (const char *name, int hash_size,
				 int open_flags, mode_t mode,
				 chop_block_store_t **store)
{
  errcode_t err;

  *store = malloc
    (chop_class_instance_size ((chop_class_t *)&chop_tdb_block_store_class));
  if (!*store)
    return ENOMEM;

  err = chop_tdb_store_open (name, hash_size, 0, open_flags, mode,
			     *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_remote_block_store_open_alloc (const char *host, const char *protocol,
				    chop_block_store_t **store)
{
  errcode_t err;

  *store = malloc (chop_class_instance_size (&chop_remote_block_store_class));
  if (!*store)
    return ENOMEM;

  err = chop_remote_block_store_open (host, protocol, *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static void
chop_store_close_dealloc (chop_block_store_t *store)
{
  if (store)
    {
      chop_store_close (store);
      free (store);
    }
}

static __inline__ errcode_t
chop_store_read_block_alloc_u8vector (chop_block_store_t *store,
				      const chop_block_key_t *key,
				      SCM *result)
{
  errcode_t err;
  size_t size;
  chop_buffer_t buffer;

  chop_buffer_init (&buffer, 0);

  err = chop_store_read_block (store, key, &buffer, &size);
  if (err)
    {
      chop_buffer_return (&buffer);
      *result = SCM_BOOL_F;

      /* When a block wasn't found in the underlying store, simply return
	 #f.  */
      return ((err == CHOP_STORE_BLOCK_UNAVAIL) ? 0 : err);
    }

  assert (size == chop_buffer_size (&buffer));
  if (!size)
    *result = SCM_BOOL_F;
  else
    {
      char *block = malloc (size);
      if (block)
	{
	  memcpy (block, chop_buffer_content (&buffer), size);
	  *result = scm_take_u8vector (block, size);
	}
      else
	{
	  err = ENOMEM;
	  *result = SCM_BOOL_F;
	}
    }

  return err;
}


/* Support for writing block stores in Guile Scheme.  */

CHOP_DECLARE_RT_CLASS (scheme_block_store, block_store,
		       SCM read_block;
		       SCM write_block;
		       SCM block_exists;
		       SCM delete_block;
		       SCM first_key;
		       SCM next_key;
		       SCM close;
		       SCM sync;);

static SCM guile_chop_store_type = SCM_BOOL_F;

static errcode_t
scm_store_block_exists (chop_block_store_t *store,
			const chop_block_key_t *key, int *exists)
{
  *exists = 0;
  return CHOP_ERR_NOT_IMPL;  /* FIXME */
}

static errcode_t
scm_store_read_block (chop_block_store_t *store,
		      const chop_block_key_t *key,
		      chop_buffer_t *buffer, size_t *read)
{
  errcode_t err;
  chop_scheme_block_store_t *scm_store;

  scm_store = (chop_scheme_block_store_t *)store;
  if (scm_procedure_p (scm_store->read_block) == SCM_BOOL_T)
    {
      SCM s_key, s_block, s_store;

      s_store = gw_wcp_assimilate_ptr (store, guile_chop_store_type);
      s_key = scm_take_u8vector (chop_block_key_buffer (key),
				 chop_block_key_size (key));
      s_block = scm_call_2 (scm_store->read_block, s_store, s_key);
      if (scm_u8vector_p (s_block) == SCM_BOOL_T)
	{
	  scm_t_array_handle handle;
	  const char *block;
	  size_t size;
	  ssize_t inc;

	  err = 0;
	  block = scm_u8vector_elements (s_block, &handle, &size, &inc);
	  *read = size;
	  if (inc == 1)
	    err = chop_buffer_push (buffer, block, size);
	  else
	    {
	      const char *p;

	      chop_buffer_clear (buffer);
	      for (p = block; size; size--, p += inc)
		{
		  err = chop_buffer_append (buffer, p, 1);
		  if (err)
		    break;
		}
	    }
	  scm_array_handle_release (&handle);
	}
      else
	/* FIXME:  We could consider other error conditions too.  */
	err = CHOP_STORE_BLOCK_UNAVAIL;
    }
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

static errcode_t
scm_store_write_block (chop_block_store_t *store,
		       const chop_block_key_t *key,
		       const char *buffer, size_t size)
{
  errcode_t err;
  chop_scheme_block_store_t *scm_store;

  scm_store = (chop_scheme_block_store_t *)store;
  if (scm_procedure_p (scm_store->write_block) == SCM_BOOL_T)
    {
      SCM s_key, s_content, s_store, s_result;
      char *key_cpy, *buf_cpy;

      key_cpy = scm_malloc (chop_block_key_size (key));
      memcpy (key_cpy, chop_block_key_buffer (key),
	      chop_block_key_size (key));
      buf_cpy = scm_malloc (size);
      memcpy (buf_cpy, buffer, size);

      s_store = gw_wcp_assimilate_ptr (store, guile_chop_store_type);
      s_key = scm_take_u8vector (key_cpy, chop_block_key_size (key));
      s_content = scm_take_u8vector (buf_cpy, size);
      s_result = scm_call_3 (scm_store->write_block, s_store,
			     s_key, s_content);
      if (s_result == SCM_BOOL_F)
	err = CHOP_STORE_ERROR;
      else
	err = 0;
    }
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}

static errcode_t
scm_store_delete_block (chop_block_store_t *store,
			const chop_block_key_t *key)
{
  return CHOP_ERR_NOT_IMPL;  /* FIXME */
}

static errcode_t
scm_store_first_key (chop_block_store_t *store,
		     chop_block_key_t *key)
{
  return CHOP_ERR_NOT_IMPL;  /* FIXME */
}

static errcode_t
scm_store_next_key (chop_block_store_t *store,
		    const chop_block_key_t *key,
		    chop_block_key_t *next)
{
  return CHOP_ERR_NOT_IMPL;  /* FIXME */
}

static errcode_t
scm_store_close (chop_block_store_t *store)
{
  errcode_t err;
  chop_scheme_block_store_t *scm_store;

  scm_store = (chop_scheme_block_store_t *)store;
  if (scm_procedure_p (scm_store->close) == SCM_BOOL_T)
    {
      SCM s_store, s_result;

      s_store = gw_wcp_assimilate_ptr (store, guile_chop_store_type);
      s_result = scm_call_1 (scm_store->close, s_store);
      if (s_result == SCM_BOOL_F)
	err = CHOP_STORE_ERROR;
      else
	err = 0;
    }
  else
    err = CHOP_ERR_NOT_IMPL;

  scm_gc_unprotect_object (scm_store->block_exists);
  /* FIXME:  Finish */

  return err;
}

static errcode_t
scm_store_sync (chop_block_store_t *store)
{
  errcode_t err;
  chop_scheme_block_store_t *scm_store;

  scm_store = (chop_scheme_block_store_t *)store;
  if (scm_procedure_p (scm_store->sync) == SCM_BOOL_T)
    {
      SCM s_store, s_result;

      s_store = gw_wcp_assimilate_ptr (store, guile_chop_store_type);
      s_result = scm_call_1 (scm_store->sync, s_store);
      if (s_result == SCM_BOOL_F)
	err = CHOP_STORE_ERROR;
      else
	err = 0;
    }
  else
    err = CHOP_ERR_NOT_IMPL;

  return err;
}




static __inline__ chop_block_store_t *
chop_make_scheme_block_store (SCM read_block, SCM write_block,
			      SCM block_exists, SCM delete_block,
			      SCM first_key, SCM next_key,
			      SCM sync, SCM close)
{
  chop_scheme_block_store_t *store;

  store = malloc (sizeof (chop_scheme_block_store_t));
  if (!store)
    return NULL;

  chop_object_initialize ((chop_object_t *)store,
			  &chop_scheme_block_store_class);

#define SET_METHOD(_name)				\
  store->block_store. _name = scm_store_ ## _name;	\
  scm_gc_protect_object (_name);			\
  store-> _name = _name;

  SET_METHOD (block_exists);
  SET_METHOD (read_block);
  SET_METHOD (write_block);
  SET_METHOD (delete_block);
  SET_METHOD (first_key);
  SET_METHOD (next_key);
  SET_METHOD (close);
  SET_METHOD (sync);

#undef SET_METHOD

  if (guile_chop_store_type == SCM_BOOL_F)
    {
      /* Find the module type object `<store>' in the `(chop stores)'
	 module.  This is then used in conjunction with
	 `gw_wcp_assimilate_ptr ()'.  Yes, this is somewhat ugly.  (XXX)  */
      SCM module_name, module;

      module = scm_c_resolve_module ("chop stores");
      guile_chop_store_type = scm_c_module_lookup (module, "<store>");
      guile_chop_store_type = scm_variable_ref (guile_chop_store_type);
    }

  return ((chop_block_store_t *)store);
}

static void
scheme_block_store_ctor (chop_object_t *object,
			 const chop_class_t *class)
{
  chop_scheme_block_store_t *store;

  store = (chop_scheme_block_store_t *)object;
  store->block_store.read_block = NULL;
  store->block_store.write_block = NULL;
  store->block_store.close = NULL;
  store->block_store.sync = NULL;
}

CHOP_DEFINE_RT_CLASS (scheme_block_store, block_store,
		      scheme_block_store_ctor, NULL,
		      NULL, NULL);

