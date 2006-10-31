/* Support for Sun/ONC RPC over TLS.  */

/* Most of the code here is adapted from the implementation of RPC for TCP by
   Sun as found in the GNU C library (e.g., version 2.3.5), specifically the
   files `svc_tcp.c' and `clnt_tcp.c' in the `sunrpc' directory.  The
   copyright notice found in these files is reproduced below.

   Implementing new RPC server/client classes (resp. the `SVCXPRT' and
   `CLIENT' objects) is not really documented.  However, since the RPC code
   is old and hasn't changed significantly over the past decades, this
   RPC/TLS code should still run fine in the forthcoming future.  */

/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */


#include <stdio.h>
#include <unistd.h>
#include <rpc/rpc.h>
#include <errno.h>
#include <stdlib.h>

#include <gnutls/gnutls.h>

#include <chop/sunrpc-tls.h>

/* The awful hack.  */
static int gnutls_initialized = 0;



#ifdef DEBUG_TLS
extern int _gnutls_log_level;
extern void (* _gnutls_log_func) (int, const char *);

static void
log_gnutls (int level, const char *str)
{
  fprintf (stderr, "gnutls(%i): %s\n", level, str);
}

# define ENABLE_GNUTLS_LOGGING()		\
        _gnutls_log_level = 3;			\
        _gnutls_log_func = log_gnutls;

#else /* !DEBUG_TLS */

# define ENABLE_GNUTLS_LOGGING()     do { } while (0)

#endif /* !DEBUG_TLS */

#define ENSURE_GNUTLS_INITIALIZED()		\
do						\
  {						\
    if (!gnutls_initialized)			\
      {						\
	gnutls_initialized = 1;			\
	gnutls_global_init ();			\
        ENABLE_GNUTLS_LOGGING ();		\
      }						\
  }						\
while (0)


/* TLS helper functions.  */
static int readtls (gnutls_session_t session, char *buf, int len);
static int writetls (gnutls_session_t session, char *buf, int len);



/* Server-side.  */

/*
 * Ops vector for TCP/IP based rpc service handle
 */
static bool_t svctls_recv (SVCXPRT *, struct rpc_msg *);
static enum xprt_stat svctls_stat (SVCXPRT *);
static bool_t svctls_getargs (SVCXPRT *, xdrproc_t, caddr_t);
static bool_t svctls_reply (SVCXPRT *, struct rpc_msg *);
static bool_t svctls_freeargs (SVCXPRT *, xdrproc_t, caddr_t);
static void svctls_destroy (SVCXPRT *);

typedef enum
  {
    SVCTLS_TYPE_UNDEFINED = 0,
    SVCTLS_TYPE_RENDEZVOUS = 1,
    SVCTLS_TYPE_CONNECTION = 2
  }
svctls_type_t;

static const struct xp_ops svctls_op =
{
  svctls_recv,
  svctls_stat,
  svctls_getargs,
  svctls_reply,
  svctls_freeargs,
  svctls_destroy
};

/*
 * Ops vector for TCP/IP rendezvous handler
 */
static bool_t rendezvous_request (SVCXPRT *, struct rpc_msg *);
static enum xprt_stat rendezvous_stat (SVCXPRT *);
static void svctls_rendezvous_abort (void) __attribute__ ((__noreturn__));

/* This function makes sure abort() relocation goes through PLT
   and thus can be lazy bound.  */
static void
svctls_rendezvous_abort (void)
{
  abort ();
};

static const struct xp_ops svctls_rendezvous_op =
{
  rendezvous_request,
  rendezvous_stat,
  (bool_t (*) (SVCXPRT *, xdrproc_t, caddr_t)) svctls_rendezvous_abort,
  (bool_t (*) (SVCXPRT *, struct rpc_msg *)) svctls_rendezvous_abort,
  (bool_t (*) (SVCXPRT *, xdrproc_t, caddr_t)) svctls_rendezvous_abort,
  svctls_destroy
};

static int svc_readtls (char*, char *, int);
static int svc_writetls (char *, char *, int);
static SVCXPRT *makefd_xprt (svctls_session_initializer_t, void *,
			     int, u_int, u_int, gnutls_session_t *);

struct tcp_rendezvous
  {				/* kept in xprt->xp_p1 */
    u_int sendsize;
    u_int recvsize;

    svctls_session_initializer_t initializer;
    void *initializer_data;

    svctls_authorizer_t authorizer;
    void *authorizer_data;
  };

struct tcp_conn
  {				/* kept in xprt->xp_p1 */
    enum xprt_stat strm_stat;
    u_long x_id;
    XDR xdrs;
    char verf_body[MAX_AUTH_BYTES];
    gnutls_session_t session;
  };


void
svctls_init_if_needed (void)
{
  ENSURE_GNUTLS_INITIALIZED ();
}

SVCXPRT *
svctls_create (svctls_session_initializer_t initializer, void *init_data,
	       svctls_authorizer_t authorizer, void *auth_data,
	       int sock,
	       u_int sendsize, u_int recvsize)
{
  SVCXPRT *xprt;
  struct tcp_rendezvous *r;

  ENSURE_GNUTLS_INITIALIZED ();

  if (sock == RPC_ANYSOCK)
    /* FIXME: Handle this.  */
    abort ();

  xprt = (SVCXPRT *) malloc (sizeof (SVCXPRT));
  r = (struct tcp_rendezvous *) malloc (sizeof (*r));
  if ((r == NULL) || (xprt == NULL))
    {
      (void) fputs ("svctls_create: out of memory\n", stderr);
      if (xprt)
	free (xprt);
      if (r)
	free (r);

      return NULL;
    }

  r->sendsize = sendsize;
  r->recvsize = recvsize;
  r->initializer = initializer;
  r->initializer_data = init_data;
  r->authorizer = authorizer;
  r->authorizer_data = auth_data;

  xprt->xp_p1 = (caddr_t) r;
  xprt->xp_p2 = (caddr_t) SVCTLS_TYPE_RENDEZVOUS;
  xprt->xp_verf = _null_auth;
  xprt->xp_ops = &svctls_rendezvous_op;
  xprt->xp_port = 0; /* unknown */
  xprt->xp_sock = sock;
  xprt->xp_addrlen = 0;
  memset (&xprt->xp_raddr, 0, sizeof (xprt->xp_raddr));
  xprt_register (xprt);

  return xprt;
}


static SVCXPRT *
makefd_xprt (svctls_session_initializer_t initializer, void *init_data,
	     int fd,
	     u_int sendsize, u_int recvsize,
	     gnutls_session_t *session)
{
  int err;
  SVCXPRT *xprt;
  struct tcp_conn *cd;

  /* Invoke the user-provided TLS session maker.  */
  if (initializer (session, init_data))
    {
      (void)fputs ("svc_tls: makefd_xprt: TLS session initializer failed\n",
		   stderr);
      return NULL;
    }

  xprt = (SVCXPRT *) malloc (sizeof (SVCXPRT));
  cd = (struct tcp_conn *) malloc (sizeof (struct tcp_conn));
  if (xprt == (SVCXPRT *) NULL || cd == NULL)
    {
      (void)fputs ("svc_tls: makefd_xprt: out of memory\n", stderr);

      if (xprt)
	free (xprt);
      if (xprt)
	free (cd);

      return NULL;
    }
  cd->strm_stat = XPRT_IDLE;
  cd->session = *session;

  gnutls_transport_set_ptr (*session, (gnutls_transport_ptr_t)fd);
  err = gnutls_handshake (*session);
  if (err)
    {
      fprintf (stderr, "svc_tls: server-side TLS handshake failed: %s\n",
	       gnutls_strerror (err));

      free (cd);
      free (xprt);
      gnutls_deinit (*session);
      close (fd);

      return NULL;
    }
  /* printf ("server-side TLS handshake succeeded\n"); */

  xdrrec_create (&(cd->xdrs), sendsize, recvsize,
		 (caddr_t) xprt, svc_readtls, svc_writetls);
  xprt->xp_p2 = (caddr_t) SVCTLS_TYPE_CONNECTION;
  xprt->xp_p1 = (caddr_t) cd;
  xprt->xp_verf.oa_base = cd->verf_body;
  xprt->xp_ops = &svctls_op;	/* truly deals with calls */
  xprt->xp_port = 0;		/* this is a connection, not a rendezvouser */
  xprt->xp_sock = fd;
  xprt->xp_addrlen = 0;
  memset (&xprt->xp_raddr, 0, sizeof (xprt->xp_raddr));
  xprt_register (xprt);

  return xprt;
}

static bool_t
rendezvous_request (SVCXPRT *xprt, struct rpc_msg *errmsg)
{
  int sock;
  struct tcp_rendezvous *r;
  struct sockaddr_in addr;
  socklen_t len;
  gnutls_session_t session;

  r = (struct tcp_rendezvous *) xprt->xp_p1;
again:
  len = sizeof (struct sockaddr_in);
  if ((sock = accept (xprt->xp_sock, (struct sockaddr *) &addr, &len)) < 0)
    {
      if (errno == EINTR)
	goto again;
      return FALSE;
    }
  /*
   * make a new transporter (re-uses xprt)
   */
  xprt = makefd_xprt (r->initializer, r->initializer_data,
		      sock, r->sendsize, r->recvsize,
		      &session);
  if (xprt)
    {
      memcpy (&xprt->xp_raddr, &addr, sizeof (addr));
      xprt->xp_addrlen = len;

      if (r->authorizer)
	{
	  /* Perform an application-level authorization.  */
	  int authorized;

	  authorized = r->authorizer (session, r->authorizer_data);
	  if (!authorized)
	    {
	      /* Authorization denied.  */
	      svc_destroy (xprt);
	      xprt = NULL;
	    }
	}
    }

  return FALSE;		/* there is never an rpc msg to be processed */
}

static enum xprt_stat
rendezvous_stat (SVCXPRT *xprt)
{
  return XPRT_IDLE;
}

static void
svctls_destroy (SVCXPRT *xprt)
{
  xprt_unregister (xprt);
  (void) close (xprt->xp_sock);

  if ((svctls_type_t)xprt->xp_p2 == SVCTLS_TYPE_RENDEZVOUS)
    {
      /* a rendezvouser socket */
      struct tcp_rendezvous *r = (struct tcp_rendezvous *) xprt->xp_p1;

      xprt->xp_port = 0;

      free (r);
    }
  else
    {
      /* an actual connection socket */
      struct tcp_conn *cd = (struct tcp_conn *) xprt->xp_p1;

      gnutls_deinit (cd->session);
      XDR_DESTROY (&(cd->xdrs));

      free (cd);
    }

  free (xprt);
}

static int
svc_readtls (char *xprtptr, char *buf, int len)
{
  SVCXPRT *xprt = (SVCXPRT *)xprtptr;
  struct tcp_conn *cd = (struct tcp_conn *) xprt->xp_p1;

  return (readtls (cd->session, buf, len));
}

static int
svc_writetls (char *xprtptr, char * buf, int len)
{
  SVCXPRT *xprt = (SVCXPRT *)xprtptr;
  struct tcp_conn *cd = (struct tcp_conn *) xprt->xp_p1;

  return (writetls (cd->session, buf, len));
}


static enum xprt_stat
svctls_stat (SVCXPRT *xprt)
{
  struct tcp_conn *cd =
  (struct tcp_conn *) (xprt->xp_p1);

  if (cd->strm_stat == XPRT_DIED)
    return XPRT_DIED;
  if (!xdrrec_eof (&(cd->xdrs)))
    return XPRT_MOREREQS;
  return XPRT_IDLE;
}

static bool_t
svctls_recv (SVCXPRT *xprt, struct rpc_msg *msg)
{
  struct tcp_conn *cd = (struct tcp_conn *) (xprt->xp_p1);
  XDR *xdrs = &(cd->xdrs);

  xdrs->x_op = XDR_DECODE;
  (void) xdrrec_skiprecord (xdrs);
  if (xdr_callmsg (xdrs, msg))
    {
      cd->x_id = msg->rm_xid;
      return TRUE;
    }
  cd->strm_stat = XPRT_DIED;	/* XXXX */
  return FALSE;
}

static bool_t
svctls_getargs (SVCXPRT *xprt, xdrproc_t xdr_args, caddr_t args_ptr)
{
  return ((*xdr_args) (&(((struct tcp_conn *)
			  (xprt->xp_p1))->xdrs), args_ptr));
}

static bool_t
svctls_freeargs (SVCXPRT *xprt, xdrproc_t xdr_args, caddr_t args_ptr)
{
  XDR *xdrs = &(((struct tcp_conn *) (xprt->xp_p1))->xdrs);

  xdrs->x_op = XDR_FREE;
  return ((*xdr_args) (xdrs, args_ptr));
}

static bool_t
svctls_reply (SVCXPRT *xprt, struct rpc_msg *msg)
{
  struct tcp_conn *cd = (struct tcp_conn *) (xprt->xp_p1);
  XDR *xdrs = &(cd->xdrs);
  bool_t stat;

  xdrs->x_op = XDR_ENCODE;
  msg->rm_xid = cd->x_id;
  stat = xdr_replymsg (xdrs, msg);
  (void) xdrrec_endofrecord (xdrs, TRUE);
  return stat;
}



/* Client support, stolen from Glibc's `clnt_tcp.c'.  */

/*
 * clnt_tcp.c, Implements a TCP/IP based, client side RPC.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * TCP based RPC supports 'batched calls'.
 * A sequence of calls may be batched-up in a send buffer.  The rpc call
 * return immediately to the client even though the call was not necessarily
 * sent.  The batching occurs if the results' xdr routine is NULL (0) AND
 * the rpc timeout value is zero (see clnt.h, rpc).
 *
 * Clients should NOT casually batch calls that in fact return results; that is,
 * the server side should be aware that a call is batched and not produce any
 * return message.  Batched calls that produce many result messages can
 * deadlock (netlock) the client and the server....
 *
 * Now go hang yourself.
 */

#include <rpc/pmap_clnt.h>

extern u_long _create_xid (void);

#define MCALL_MSG_SIZE 24

struct ct_data
  {
    gnutls_session_t ct_session;

    bool_t ct_closeit;
    struct timeval ct_wait;
    bool_t ct_waitset;		/* wait set by clnt_control? */
    struct rpc_err ct_error;
    char ct_mcall[MCALL_MSG_SIZE];	/* marshalled callmsg */
    u_int ct_mpos;		/* pos after marshal */
    XDR ct_xdrs;
  };

static int clnt_readtls (char *, char *, int);
static int clnt_writetls (char *, char *, int);

static enum clnt_stat clnttls_call (CLIENT *, u_long, xdrproc_t, caddr_t,
				    xdrproc_t, caddr_t, struct timeval);
static void clnttls_abort (void);
static void clnttls_geterr (CLIENT *, struct rpc_err *);
static bool_t clnttls_freeres (CLIENT *, xdrproc_t, caddr_t);
static bool_t clnttls_control (CLIENT *, int, char *);
static void clnttls_destroy (CLIENT *);

static struct clnt_ops tcp_ops =
{
  clnttls_call,
  clnttls_abort,
  clnttls_geterr,
  clnttls_freeres,
  clnttls_destroy,
  clnttls_control
};

void
clnttls_init_if_needed (void)
{
  ENSURE_GNUTLS_INITIALIZED ();
}

/* Create a client handle to a TLS connection.  We assume that SESSION is
   already usable, i.e., that all its parameters have been set and that the
   handshake has already been performed successfully.  */
CLIENT *
clnttls_create (gnutls_session_t session, u_long prog, u_long vers,
		u_int sendsz, u_int recvsz)
{
  CLIENT *client;
  struct ct_data *ct;
  struct rpc_msg call_msg;

  ENSURE_GNUTLS_INITIALIZED ();

  client = (CLIENT *) malloc (sizeof (*client));
  ct = (struct ct_data *) malloc (sizeof (*ct));
  if ((client == NULL) || (ct == NULL))
    {
      struct rpc_createerr *ce = &get_rpc_createerr ();

      (void) fputs ("clnttls_create: out of memory\n", stderr);
      ce->cf_stat = RPC_SYSTEMERROR;
      ce->cf_error.re_errno = ENOMEM;
      goto fooy;
    }

  /*
   * Set up private data struct
   */
  ct->ct_wait.tv_usec = 0;
  ct->ct_waitset = FALSE;

  /* We're assuming SESSION denotes a client session.  */
  ct->ct_session = session;


  /*
   * Initialize call message
   */
  call_msg.rm_xid = _create_xid ();
  call_msg.rm_direction = CALL;
  call_msg.rm_call.cb_rpcvers = RPC_MSG_VERSION;
  call_msg.rm_call.cb_prog = prog;
  call_msg.rm_call.cb_vers = vers;

  /*
   * pre-serialize the static part of the call msg and stash it away
   */
  xdrmem_create (&(ct->ct_xdrs), ct->ct_mcall, MCALL_MSG_SIZE,
		 XDR_ENCODE);
  if (!xdr_callhdr (&(ct->ct_xdrs), &call_msg))
    {
      if (ct->ct_closeit)
	gnutls_deinit (ct->ct_session);

      goto fooy;
    }
  ct->ct_mpos = XDR_GETPOS (&(ct->ct_xdrs));
  XDR_DESTROY (&(ct->ct_xdrs));

  /*
   * Create a client handle which uses xdrrec for serialization
   * and authnone for authentication.
   */
  xdrrec_create (&(ct->ct_xdrs), sendsz, recvsz,
		 (caddr_t) ct, clnt_readtls, clnt_writetls);
  client->cl_ops = &tcp_ops;
  client->cl_private = (caddr_t) ct;
  client->cl_auth = authnone_create ();
  return client;

fooy:
  /*
   * Something goofed, free stuff and barf
   */
  if (ct)
    free (ct);
  if (client)
    free (client);

  return ((CLIENT *) NULL);
}


static enum clnt_stat
clnttls_call (CLIENT *h, u_long proc, xdrproc_t xdr_args, caddr_t args_ptr,
	      xdrproc_t xdr_results, caddr_t results_ptr,
	      struct timeval timeout)
{
  struct ct_data *ct = (struct ct_data *) h->cl_private;
  XDR *xdrs = &(ct->ct_xdrs);
  struct rpc_msg reply_msg;
  u_long x_id;
  u_int32_t *msg_x_id = (u_int32_t *) (ct->ct_mcall);	/* yuk */
  bool_t shipnow;
  int refreshes = 2;

  if (!ct->ct_waitset)
    {
      ct->ct_wait = timeout;
    }

  shipnow =
    (xdr_results == (xdrproc_t) 0 && ct->ct_wait.tv_sec == 0
     && ct->ct_wait.tv_usec == 0) ? FALSE : TRUE;

call_again:
  xdrs->x_op = XDR_ENCODE;
  ct->ct_error.re_status = RPC_SUCCESS;
  x_id = ntohl (--(*msg_x_id));
  if ((!XDR_PUTBYTES (xdrs, ct->ct_mcall, ct->ct_mpos)) ||
      (!XDR_PUTLONG (xdrs, (long *) &proc)) ||
      (!AUTH_MARSHALL (h->cl_auth, xdrs)) ||
      (!(*xdr_args) (xdrs, args_ptr)))
    {
      if (ct->ct_error.re_status == RPC_SUCCESS)
	ct->ct_error.re_status = RPC_CANTENCODEARGS;
      (void) xdrrec_endofrecord (xdrs, TRUE);
      return (ct->ct_error.re_status);
    }
  if (!xdrrec_endofrecord (xdrs, shipnow))
    return ct->ct_error.re_status = RPC_CANTSEND;
  if (!shipnow)
    return RPC_SUCCESS;
  /*
   * Hack to provide rpc-based message passing
   */
  if (ct->ct_wait.tv_sec == 0 && ct->ct_wait.tv_usec == 0)
    {
      return ct->ct_error.re_status = RPC_TIMEDOUT;
    }


  /*
   * Keep receiving until we get a valid transaction id
   */
  xdrs->x_op = XDR_DECODE;
  while (TRUE)
    {
      reply_msg.acpted_rply.ar_verf = _null_auth;
      reply_msg.acpted_rply.ar_results.where = NULL;
      reply_msg.acpted_rply.ar_results.proc = (xdrproc_t)xdr_void;
      if (!xdrrec_skiprecord (xdrs))
	return (ct->ct_error.re_status);
      /* now decode and validate the response header */
      if (!xdr_replymsg (xdrs, &reply_msg))
	{
	  if (ct->ct_error.re_status == RPC_SUCCESS)
	    continue;
	  return ct->ct_error.re_status;
	}
      if ((u_int32_t) reply_msg.rm_xid == (u_int32_t) x_id)
	break;
    }

  /*
   * process header
   */
  _seterr_reply (&reply_msg, &(ct->ct_error));
  if (ct->ct_error.re_status == RPC_SUCCESS)
    {
      if (!(*xdr_results) (xdrs, results_ptr))
	{
	  if (ct->ct_error.re_status == RPC_SUCCESS)
	    ct->ct_error.re_status = RPC_CANTDECODERES;
	}
      /* free verifier ... */
      if (reply_msg.acpted_rply.ar_verf.oa_base != NULL)
	{
	  xdrs->x_op = XDR_FREE;
	  (void) xdr_opaque_auth (xdrs,
				  &(reply_msg.acpted_rply.ar_verf));
	}
    }				/* end successful completion */
  else
    {
      /* maybe our credentials need to be refreshed ... */
      if (refreshes--)
	goto call_again;
    }				/* end of unsuccessful completion */
  return ct->ct_error.re_status;
}

static void
clnttls_geterr (CLIENT *h, struct rpc_err *errp)
{
  struct ct_data *ct =
  (struct ct_data *) h->cl_private;

  *errp = ct->ct_error;
}

static bool_t
clnttls_freeres (cl, xdr_res, res_ptr)
     CLIENT *cl;
     xdrproc_t xdr_res;
     caddr_t res_ptr;
{
  struct ct_data *ct = (struct ct_data *) cl->cl_private;
  XDR *xdrs = &(ct->ct_xdrs);

  xdrs->x_op = XDR_FREE;
  return (*xdr_res) (xdrs, res_ptr);
}

static void
clnttls_abort ()
{
  abort ();
}

static bool_t
clnttls_control (CLIENT *cl, int request, char *info)
{
  struct ct_data *ct = (struct ct_data *) cl->cl_private;


  switch (request)
    {
    case CLSET_FD_CLOSE:
      ct->ct_closeit = TRUE;
      break;
    case CLSET_FD_NCLOSE:
      ct->ct_closeit = FALSE;
      break;
    case CLSET_TIMEOUT:
      ct->ct_wait = *(struct timeval *) info;
      ct->ct_waitset = TRUE;
      break;
    case CLGET_TIMEOUT:
      *(struct timeval *) info = ct->ct_wait;
      break;
    case CLGET_SERVER_ADDR:
      /* *(struct sockaddr_in *) info = ct->ct_addr; */
      return FALSE;
      break;
    case CLGET_FD:
      *(int *)info = -1; /* XXX: We could get that from GNUtls? */
      return FALSE;
    case CLGET_XID:
      /*
       * use the knowledge that xid is the
       * first element in the call structure *.
       * This will get the xid of the PREVIOUS call
       */
      *(u_long *)info = ntohl (*(u_long *)ct->ct_mcall);
      break;
    case CLSET_XID:
      /* This will set the xid of the NEXT call */
      *(u_long *)ct->ct_mcall =  htonl (*(u_long *)info - 1);
      /* decrement by 1 as clnttls_call() increments once */
    case CLGET_VERS:
      /*
       * This RELIES on the information that, in the call body,
       * the version number field is the fifth field from the
       * begining of the RPC header. MUST be changed if the
       * call_struct is changed
       */
      *(u_long *)info = ntohl (*(u_long *)(ct->ct_mcall +
					   4 * BYTES_PER_XDR_UNIT));
      break;
    case CLSET_VERS:
      *(u_long *)(ct->ct_mcall + 4 * BYTES_PER_XDR_UNIT)
	= htonl (*(u_long *)info);
      break;
    case CLGET_PROG:
      /*
       * This RELIES on the information that, in the call body,
       * the program number field is the  field from the
       * begining of the RPC header. MUST be changed if the
       * call_struct is changed
       */
      *(u_long *)info = ntohl(*(u_long *)(ct->ct_mcall +
					  3 * BYTES_PER_XDR_UNIT));
      break;
    case CLSET_PROG:
      *(u_long *)(ct->ct_mcall + 3 * BYTES_PER_XDR_UNIT)
	= htonl(*(u_long *)info);
      break;
    /* The following are only possible with TI-RPC */
    case CLGET_RETRY_TIMEOUT:
    case CLSET_RETRY_TIMEOUT:
    case CLGET_SVC_ADDR:
    case CLSET_SVC_ADDR:
    case CLSET_PUSH_TIMOD:
    case CLSET_POP_TIMOD:
    default:
      return FALSE;
    }
  return TRUE;
}


static void
clnttls_destroy (CLIENT *h)
{
  struct ct_data *ct =
  (struct ct_data *) h->cl_private;

  if (ct->ct_closeit)
    {
      gnutls_bye (ct->ct_session, GNUTLS_SHUT_RDWR);
      gnutls_deinit (ct->ct_session);
    }

  XDR_DESTROY (&(ct->ct_xdrs));
  free (ct);
  free (h);
}

/*
 * Interface between xdr serializer and the TLS connection.
 * Behaves like the system calls, read & write, but keeps some error state
 * around for the rpc level.
 */
static int
clnt_readtls (char *ctptr, char *buf, int len)
{
  struct ct_data *ct = (struct ct_data *)ctptr;
  /* FIXME: We don't honor timeouts!  */
  int milliseconds = (ct->ct_wait.tv_sec * 1000) +
    (ct->ct_wait.tv_usec / 1000);

  len = readtls (ct->ct_session, buf, len);
  switch (len)
    {
    case 0:
      /* premature eof */
      ct->ct_error.re_errno = ECONNRESET;
      ct->ct_error.re_status = RPC_CANTRECV;
      len = -1;			/* it's really an error */
      break;

    case -1:
      ct->ct_error.re_errno = errno;
      ct->ct_error.re_status = RPC_CANTRECV;
      break;
    }

  return len;
}

static int
clnt_writetls (char *ctptr, char *buf, int len)
{
  int i, cnt;
  struct ct_data *ct = (struct ct_data*)ctptr;

  for (cnt = len; cnt > 0; cnt -= i, buf += i)
    {
      if ((i = writetls (ct->ct_session, buf, cnt)) == -1)
	{
	  ct->ct_error.re_errno = errno;
	  ct->ct_error.re_status = RPC_CANTSEND;
	  return -1;
	}
    }

  return len;
}



/* Generic TLS helper code.  */

/* Read data from the TLS connection.  Any error is fatal and the connection
   is closed.  (And a read of zero bytes is a half closed stream =>
   error.) */
static int
readtls (gnutls_session_t session, char *buf, int len)
{
  int ret;
  unsigned rehandshake_requests = 0;

 do_recv:
  ret = gnutls_record_recv (session, buf, len);
  if (ret == 0)
    /* Connection closed.  */
    return -1;

  switch (ret)
    {
    case GNUTLS_E_AGAIN:
    case GNUTLS_E_INTERRUPTED:
      goto do_recv;

    case GNUTLS_E_REHANDSHAKE:
      /* Service the re-handshake request.  The peer might abuse this and
	 cause a DoS attack so we try to prevent against this.  */
      if (rehandshake_requests++ > 2)
	{
	  (void)fputs ("rpc/tls recv: too many rehandshake requests, aborting\n",
		       stderr);
	  return -1;
	}

      ret = gnutls_rehandshake (session);
      if (ret)
	{
	  fprintf (stderr, "rpc/tls recv: rehandshake failed: %s\n",
		   gnutls_strerror (ret));
	  return -1;
	}

      goto do_recv;

    default:
      if (ret < 0)
	{
	  /* Received corrupt data.  Closing connection.  */
	  fprintf (stderr, "rpc/tls recv failed: %s\n", gnutls_strerror (ret));
	  return -1;
	}
    }

  return ret;
}

/* Writes data to the TLS connection.  Any error is fatal and the connection
   is closed.  */
static int
writetls (gnutls_session_t session, char * buf, int len)
{
  int ret;

 do_send:
  ret = gnutls_record_send (session, buf, len);
  if ((ret == GNUTLS_E_AGAIN) || (ret == GNUTLS_E_INTERRUPTED))
    goto do_send;

  if (ret < 0)
    {
      /* Something went wrong.  */
      fprintf (stderr, "rpc/tls send failed: %s\n", gnutls_strerror (ret));
      return -1;
    }

  return ret;
}

/* Vaguely inspired by Glibc's version.  */
unsigned long
_create_xid (void)
{
  static int is_initialized = 0;
  unsigned long res;

  if (!is_initialized)
    {
      struct timeval now;

      gettimeofday (&now, (struct timezone *) 0);
      srand (now.tv_sec ^ now.tv_usec);
      is_initialized = 1;
    }

  res = (unsigned long)random ();

  return res;
}

/* arch-tag: d830fa28-40da-4beb-a0e3-af54677c9e40
 */
