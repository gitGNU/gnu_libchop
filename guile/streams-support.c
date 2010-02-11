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

/* Contructors with a functional style that perform memory allocation by
   themselves.  */

#include <chop/chop-config.h>

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

static errcode_t
chop_file_stream_open_alloc (const char *path, chop_stream_t **stream)
{
  errcode_t err;

  *stream = gwrap_chop_malloc (&chop_file_stream_class);

  err = chop_file_stream_open (path, *stream);
  if (err)
    gwrap_chop_free_uninitialized ((chop_object_t *) *stream,
				   &chop_file_stream_class);

  return err;
}

static chop_stream_t *
chop_mem_stream_open_alloc (SCM u8vector)
{
  chop_stream_t *stream = NULL;
  const scm_t_uint8 *elements;
  scm_t_uint8 *elements_copy;
  scm_t_array_handle handle;
  size_t size;
  ssize_t increment;

  elements = scm_u8vector_elements (u8vector, &handle, &size, &increment);
  if (increment != 1)
    /* Lazyness... */
    goto end;

  elements_copy = (scm_t_uint8 *)scm_malloc (size);
  memcpy (elements_copy, elements, size);

  stream = gwrap_chop_malloc (&chop_mem_stream_class);

  /* ELEMENTS_COPY will be automatically freed with `free ()' when STREAM is
     closed.  */
  chop_mem_stream_open ((char *)elements_copy, size, free, stream);

 end:
  scm_array_handle_release (&handle);

  return stream;
}

static errcode_t
chop_filtered_stream_open_alloc (chop_stream_t *backend,
				 chop_filter_t *filter,
				 int close_backend,
				 chop_stream_t **stream)
{
  errcode_t err;

  *stream = gwrap_chop_malloc (&chop_filtered_stream_class);
  err = chop_filtered_stream_open (backend,
				   /* Never destroy BACKEND: this is the GC's
				      job.  At most, close it when *STORE
				      gets closed.  */
				   close_backend
				   ? CHOP_PROXY_EVENTUALLY_CLOSE
				   : CHOP_PROXY_LEAVE_AS_IS,
				   filter, 0, *stream);
  if (err)
    {
      gwrap_chop_free_uninitialized ((chop_object_t *) *stream,
				     &chop_filtered_stream_class);
      *stream = NULL;
    }

  return err;
}


/* Return the `<stream>' WCT.  */
static inline SCM
gwrap_chop_stream_wct (void)
{
  static SCM guile_chop_stream_wct = SCM_BOOL_F;

  if (CHOP_EXPECT_FALSE (guile_chop_stream_wct == SCM_BOOL_F))
    {
      /* Find the `<chop-class>' WCT.  */
      SCM module;

      module = scm_c_resolve_module ("chop streams");
      guile_chop_stream_wct = scm_c_module_lookup (module, "<stream>");
      guile_chop_stream_wct = scm_variable_ref (guile_chop_stream_wct);
      guile_chop_stream_wct = scm_gc_protect_object (guile_chop_stream_wct);
    }

  return guile_chop_stream_wct;
}


/* Streams as Scheme ports.  */

/* The port type.  */
static scm_t_bits stream_port_type;

/* GC hint when allocating buffers.  */
static const char stream_port_gc_hint[] = "chop-stream";

/* Size of the internal buffer of stream ports.  */
#define CHOP_SCM_STREAM_PORT_BUFFER_SIZE   4096

/* Return the libchop stream associated with PORT.  */
#define CHOP_SCM_PORT_STREAM(_port)		\
  SCM_PACK (SCM_STREAM (_port))


/* Mark the libchop stream associated with PORT.  */
static SCM
mark_stream_port (SCM port)
{
  return (CHOP_SCM_PORT_STREAM (port));
}

static size_t
free_stream_port (SCM port)
{
  scm_t_port *c_port;

  /* Free the input buffer of PORT.  */
  c_port = SCM_PTAB_ENTRY (port);
  scm_gc_free (c_port->read_buf, c_port->read_buf_size,
	       stream_port_gc_hint);

  return 0;
}


/* Fill in the input buffer of PORT.  */
static int
fill_stream_port_input (SCM port)
#define FUNC_NAME "fill_stream_port_input"
{
  int chr;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);

  if (CHOP_EXPECT_TRUE (c_port->read_pos >= c_port->read_end))
    {
      SCM stream;
      errcode_t err;
      chop_stream_t *c_stream;
      size_t c_read;

      stream = CHOP_SCM_PORT_STREAM (port);
      c_stream = gw_wcp_get_ptr (stream);

      err = chop_stream_read (c_stream, (char *) c_port->read_buf,
			      c_port->read_buf_size, &c_read);
      if (CHOP_EXPECT_TRUE (err == 0))
	{
	  c_port->read_pos = c_port->read_buf;
	  c_port->read_end = c_port->read_buf + c_read;
	  chr = (int) *c_port->read_buf;
	}
      else if (err == CHOP_STREAM_END)
	chr = EOF;
      else
	{
	  chr = EOF;
	  scm_throw (scm_from_locale_symbol ("chop-error"),
		     scm_list_1 (scm_from_long (err)));
	}
    }
  else
    chr = (int) *c_port->read_pos;

  return chr;
}

/* Write SIZE octets from DATA to PORT.  */
static void
write_to_stream_port (SCM port, const void *data, size_t size)
{
  /* Streams are read-only.  */
  scm_throw (scm_from_locale_symbol ("chop-error"),
	     scm_list_1 (scm_from_long (CHOP_ERR_NOT_IMPL)));
}

/* Return a new stream port for C_STREAM.  */
static inline SCM
make_stream_port (SCM stream)
{
  SCM port = SCM_BOOL_F;

  if (CHOP_EXPECT_TRUE (gw_wcp_is_of_type_p (gwrap_chop_stream_wct (),
					     stream)))
    {
      scm_t_port *c_port;
      unsigned char *c_port_buf;
      const unsigned long mode_bits = SCM_OPN | SCM_RDNG;

      c_port_buf =
	(unsigned char *) scm_gc_malloc (CHOP_SCM_STREAM_PORT_BUFFER_SIZE,
					 stream_port_gc_hint);

      /* Create a new port.  */
      port = scm_new_port_table_entry (stream_port_type);
      c_port = SCM_PTAB_ENTRY (port);

      /* Mark PORT as open, readable and writable (hmm, how elegant...).  */
      SCM_SET_CELL_TYPE (port, stream_port_type | mode_bits);

      /* Associate it with STREAM.  */
      SCM_SETSTREAM (port, SCM_UNPACK (stream));

      c_port->read_pos = c_port->read_end = c_port->read_buf = c_port_buf;
      c_port->read_buf_size = CHOP_SCM_STREAM_PORT_BUFFER_SIZE;

      c_port->write_buf = c_port->write_pos = &c_port->shortbuf;
      c_port->write_buf_size = 1;
    }
  else
    scm_wrong_type_arg ("stream->port", 1, stream);

  return (port);
}


/* Create the stream port type.  */
static inline void
chop_scm_init_stream_port_type (void)
{
  stream_port_type =
    scm_make_port_type ("chop-stream-port",
			fill_stream_port_input,
			write_to_stream_port);
  scm_set_port_mark (stream_port_type, mark_stream_port);
  scm_set_port_free (stream_port_type, free_stream_port);
}
