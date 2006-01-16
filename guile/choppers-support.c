/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <errno.h>
#include <assert.h>


static __inline__ errcode_t
chop_fixed_size_chopper_open_alloc (chop_stream_t *input,
				    size_t block_size, int pad_blocks,
				    chop_chopper_t **chopper)
{
  errcode_t err;

  *chopper =
    scm_malloc (chop_class_instance_size ((chop_class_t *)&chop_fixed_size_chopper_class));
  if (!*chopper)
    return ENOMEM;

  err = chop_fixed_size_chopper_init (input, block_size, pad_blocks, *chopper);
  if (err)
    {
      free (*chopper);
      *chopper = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_anchor_based_chopper_open_alloc (chop_stream_t *input,
				      size_t window_size,
				      unsigned long magic_fpr_mask,
				      chop_chopper_t **chopper)
{
  errcode_t err;

  *chopper =
    scm_malloc (chop_class_instance_size ((chop_class_t *)&chop_anchor_based_chopper_class));
  if (!*chopper)
    return ENOMEM;

  err = chop_anchor_based_chopper_init (input, window_size, magic_fpr_mask,
					*chopper);
  if (err)
    {
      free (*chopper);
      *chopper = NULL;
    }

  return err;
}

static __inline__ errcode_t
chop_chopper_generic_open_alloc (const char *class_nickname,
				 chop_stream_t *input,
				 unsigned long typical_block_size,
				 chop_chopper_t **chopper)
{
  errcode_t err;
  char *class_realname;
  const chop_class_t *class;

  class_realname = alloca (strlen (class_nickname) + 20);
  strcpy (class_realname, class_nickname);
  strcat (class_realname, "_chopper");

  class = chop_class_lookup (class_realname);
  if (!class)
    return CHOP_ERR_NOT_FOUND;

  if (chop_object_get_class ((chop_object_t *)class)
      != &chop_chopper_class_class)
    return CHOP_INVALID_ARG;

  *chopper = scm_malloc (chop_class_instance_size (class));
  if (!*chopper)
    return ENOMEM;

  err = chop_chopper_generic_open ((chop_chopper_class_t *)class, input,
				   typical_block_size, *chopper);
  if (err)
    {
      free (*chopper);
      *chopper = NULL;
      return err;
    }

  return err;
}


static __inline__ errcode_t
chop_chopper_read_block_alloc_u8vector (chop_chopper_t *chopper,
					SCM *result)
{
  errcode_t err;
  size_t size;
  chop_buffer_t buffer;

  chop_buffer_init (&buffer, 0);

  err = chop_chopper_read_block (chopper, &buffer, &size);
  if (err)
    {
      chop_buffer_return (&buffer);
      *result = SCM_BOOL_F;

      return err;
    }

  assert (size == chop_buffer_size (&buffer));
  if (size)
    {
      unsigned char *block = (unsigned char *)scm_malloc (size);

      memcpy (block, chop_buffer_content (&buffer), size);
      *result = scm_take_u8vector (block, size);
    }
  else
    *result = SCM_BOOL_F;

  chop_buffer_return (&buffer);

  return err;
}
