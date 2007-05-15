/* Contructors with a functional style that perform memory allocation by
   themselves.

   FIXME: This should be generated automatically by G-Wrap, for instance by
   adding an `allocate-value-cg' method (counterpart of `destroy-value-cg')
   for `<gw-value>' objects.  */

#include <stdlib.h>
#include <errno.h>
#include <assert.h>

static __inline__ chop_block_store_t *
chop_dummy_block_store_open_alloc (const char *name)
{
  chop_block_store_t *store;

  store =
    scm_malloc (chop_class_instance_size (&chop_dummy_block_store_class));

  chop_dummy_block_store_open (name, store);

  return store;
}

static __inline__ chop_block_store_t *
chop_dummy_proxy_block_store_open_alloc (const char *name,
					 chop_block_store_t *backend)
{
  chop_block_store_t *store;

  store =
    scm_malloc (chop_class_instance_size (&chop_dummy_block_store_class));

  chop_dummy_proxy_block_store_open (name, backend, store);

  return store;
}

static __inline__ chop_block_store_t *
chop_smart_block_store_open_alloc (chop_block_store_t *backend)
{
  chop_block_store_t *store;

  store =
    scm_malloc (chop_class_instance_size (&chop_smart_block_store_class));

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

  *store = scm_malloc (chop_class_instance_size (class));

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

  *store =
    scm_malloc
    (chop_class_instance_size ((chop_class_t *)&chop_gdbm_block_store_class));

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

  *store = scm_malloc
    (chop_class_instance_size ((chop_class_t *)&chop_tdb_block_store_class));

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
chop_sunrpc_block_store_open_alloc (const char *host, unsigned port,
				    const char *protocol,
				    chop_block_store_t **store)
{
  errcode_t err;

  *store =
    scm_malloc (chop_class_instance_size (&chop_sunrpc_block_store_class));

  err = chop_sunrpc_block_store_open (host, port, protocol, *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_sunrpc_tls_block_store_open_alloc (const char *host, unsigned port,
					const char *pubkey_file,
					const char *privkey_file,
					chop_block_store_t **store)
{
  errcode_t err;

  *store =
    scm_malloc (chop_class_instance_size (&chop_sunrpc_block_store_class));

  err = chop_sunrpc_tls_block_store_simple_open (host, port,
						 pubkey_file, privkey_file,
						 *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_store_read_block_alloc_u8vector (chop_block_store_t *store,
				      const chop_block_key_t *key,
				      SCM *result)
{
  errcode_t err;
  size_t size;
  chop_buffer_t buffer;

  err = chop_buffer_init (&buffer, 0);
  if (err)
    return err;

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
      scm_t_uint8 *block = (scm_t_uint8 *)scm_malloc (size);

      memcpy (block, chop_buffer_content (&buffer), size);
      *result = scm_take_u8vector (block, size);
    }

  chop_buffer_return (&buffer);

  return err;
}


static __inline__ errcode_t
chop_filtered_store_open_alloc (chop_filter_t *input, chop_filter_t *output,
				chop_block_store_t *backend,
				int close_backend,
				chop_block_store_t **store)
{
  errcode_t err;

  *store =
    scm_malloc (chop_class_instance_size (&chop_filtered_block_store_class));

  err = chop_filtered_store_open (input, output, backend,
				  /* Never destroy BACKEND: this is the GC's
				     job.  At most, close it when *STORE gets
				     closed.  */
				  close_backend
				  ? CHOP_PROXY_EVENTUALLY_CLOSE
				  : CHOP_PROXY_LEAVE_AS_IS,
				  *store);
  if (err)
    {
      free (*store);
      *store = NULL;
    }

  return err;
}


/* Block iterators.  */

static __inline__ errcode_t
chop_store_first_block_alloc (chop_block_store_t *store,
			      chop_block_iterator_t **it)
{
  errcode_t err;
  const chop_class_t *it_class;

  it_class = chop_store_iterator_class (store);
  if (!it_class)
    {
      *it = NULL;
      return CHOP_ERR_NOT_IMPL;
    }

  *it = scm_malloc (chop_class_instance_size (it_class));

  err = chop_store_first_block (store, *it);
  if (err)
    {
      free (*it);
      *it = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_block_iterator_key_check_non_nil (chop_block_iterator_t *it,
				       chop_block_key_t *key)
{
  const chop_block_key_t *it_key;

  if (chop_block_iterator_is_nil (it))
    return CHOP_INVALID_ARG;

  it_key = chop_block_iterator_key (it);

  /* KEY will be soon destroyed (in the glue code) so we can safely re-use
     the data of IT_KEY here.  */
  chop_block_key_init (key, (char *)chop_block_key_buffer (it_key),
		       chop_block_key_size (it_key),
		       NULL, NULL);

  return 0;
}



/* Support for writing block stores in Guile Scheme.  */

/* Definition of a Scheme block store.  Since instances of this class contain
   SCM objects, we choose CHOP_HYBRID_SCHEME_CLASS_CLASS as its metaclass.
   This allows to specify a `mark' method for instances of this class.  */
CHOP_DECLARE_RT_CLASS_WITH_METACLASS (scheme_block_store, block_store,
				      hybrid_scheme_class /* metaclass */,

				      /* the one and only SMOB created for
					 this object */
				      SCM this_smob;

				      SCM read_block;
				      SCM write_block;
				      SCM block_exists;
				      SCM delete_block;
				      SCM first_block;
				      SCM it_next;
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

      s_store = scm_store->this_smob;
      s_key = scm_take_u8vector ((scm_t_uint8 *)chop_block_key_buffer (key),
				 chop_block_key_size (key));
      s_block = scm_call_2 (scm_store->read_block, s_store, s_key);
      if (scm_u8vector_p (s_block) == SCM_BOOL_T)
	{
	  scm_t_array_handle handle;
	  const scm_t_uint8 *block;
	  size_t size;
	  ssize_t inc;

	  err = 0;
	  block = scm_u8vector_elements (s_block, &handle, &size, &inc);
	  *read = size;
	  if (inc == 1)
	    err = chop_buffer_push (buffer, (char *)block, size);
	  else
	    {
	      const scm_t_uint8 *p;

	      chop_buffer_clear (buffer);
	      for (p = block; size; size--, p += inc)
		{
		  err = chop_buffer_append (buffer, (char *)p, 1);
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

      s_store = scm_store->this_smob;
      s_key = scm_take_u8vector ((scm_t_uint8 *)key_cpy,
				 chop_block_key_size (key));
      s_content = scm_take_u8vector ((scm_t_uint8 *)buf_cpy, size);
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
scm_store_first_block (chop_block_store_t *store,
		       chop_block_iterator_t *it)
{
  return CHOP_ERR_NOT_IMPL;  /* FIXME */
}

#if 0
static errcode_t
scm_store_it_next (chop_block_iterator_t *it)
{
  return CHOP_ERR_NOT_IMPL;  /* FIXME */
}
#endif

static errcode_t
scm_store_close (chop_block_store_t *store)
{
  errcode_t err;
  chop_scheme_block_store_t *scm_store;

  scm_store = (chop_scheme_block_store_t *)store;
  if (scm_procedure_p (scm_store->close) == SCM_BOOL_T)
    {
      SCM s_store, s_result;

      s_store = scm_store->this_smob;
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

      s_store = scm_store->this_smob;
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

#ifdef DEBUG
# include <stdio.h>
#endif

static SCM
scm_store_mark (chop_object_t *object)
{
  chop_scheme_block_store_t *scm_store;

  scm_store = (chop_scheme_block_store_t *)object;

#ifdef DEBUG
  fprintf (stderr, "%s: marking scm block store %p\n",
	   __FUNCTION__, scm_store);
#endif

#define DO_MARK(_s)				\
  if (scm_store-> _s != SCM_BOOL_F)		\
    scm_gc_mark (scm_store-> _s);

  DO_MARK (block_exists);
  DO_MARK (read_block);
  DO_MARK (write_block);
  DO_MARK (delete_block);
  DO_MARK (first_block);
  DO_MARK (it_next);
  DO_MARK (close);

#undef DO_MARK

  if (scm_store->sync != SCM_BOOL_F)
    return (scm_store->sync);

  return SCM_BOOL_F;
}


static __inline__ SCM
chop_make_scheme_block_store (SCM read_block, SCM write_block,
			      SCM block_exists, SCM delete_block,
			      SCM first_block, SCM it_next,
			      SCM sync, SCM close)
{
  SCM s_store = SCM_BOOL_F;
  chop_scheme_block_store_t *store;

  store = scm_malloc (sizeof (chop_scheme_block_store_t));

  chop_object_initialize ((chop_object_t *)store,
			  (chop_class_t *)&chop_scheme_block_store_class);

#define SET_METHOD(_name)				\
  store->block_store. _name = scm_store_ ## _name;	\
  scm_gc_protect_object (_name);			\
  store-> _name = _name;

  SET_METHOD (block_exists);
  SET_METHOD (read_block);
  SET_METHOD (write_block);
  SET_METHOD (delete_block);
  SET_METHOD (first_block);
  /* SET_METHOD (it_next); */
  store->it_next = SCM_BOOL_F;
  SET_METHOD (close);
  SET_METHOD (sync);

#undef SET_METHOD

  if (guile_chop_store_type == SCM_BOOL_F)
    {
      /* Find the module type object `<store>' in the `(chop stores)'
	 module.  This is then used in conjunction with
	 `gw_wcp_assimilate_ptr ()'.  Yes, this is somewhat ugly.  (XXX)  */
      SCM module;

      module = scm_c_resolve_module ("chop stores");
      guile_chop_store_type = scm_c_module_lookup (module, "<store>");
      guile_chop_store_type = scm_variable_ref (guile_chop_store_type);
    }

  /* We need to make sure that only one SMOB is created for this C object, so
     that it doesn't get freed more than once.  For this reason, we create
     the SMOB here, keep track of it in STORE->THIS_SMOB, and then use it
     directly in `scm_store_* ()' method wrappers.  */
  s_store = gw_wcp_assimilate_ptr (store, guile_chop_store_type);
  store->this_smob = s_store;

  return (s_store);
}

static errcode_t
sbs_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_scheme_block_store_t *store;

  store = (chop_scheme_block_store_t *)object;
  store->block_store.iterator_class = NULL; /* FIXME: Not implemented.  */
  store->block_store.read_block = NULL;
  store->block_store.write_block = NULL;
  store->block_store.close = NULL;
  store->block_store.sync = NULL;

  return 0;
}

static void
sbs_dtor (chop_object_t *object)
{
  chop_scheme_block_store_t *store;

  store = (chop_scheme_block_store_t *)object;
  store->read_block = SCM_BOOL_F;
  store->write_block = SCM_BOOL_F;
#ifdef DEBUG
  fprintf (stderr, "%s: freed Scheme block store @%p\n",
	   __FUNCTION__, store);
#endif
}

/* Define the class and specify the MARK method that should be used for
   instances of this class.  */
CHOP_DEFINE_RT_CLASS_WITH_METACLASS (scheme_block_store, block_store,
				     hybrid_scheme_class /* metaclass */,

				     /* metaclass inits */
				     .mark = scm_store_mark,

				     sbs_ctor, sbs_dtor,
				     NULL, NULL,
				     NULL, NULL);

