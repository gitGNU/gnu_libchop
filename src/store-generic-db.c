/* Template for the GDBM and TDB block stores.  */

#ifndef DB_TYPE
# error "This file is meant to be included in some other source file."
#endif


#ifndef CONCAT4
# define _CONCAT4(_x, _y, _z, _p) _x ## _y ## _z ## _p
# define CONCAT4(_a, _b, _c, _d)  _CONCAT4 (_a, _b, _c, _d)
#endif

#define DB_READ_BLOCK_METHOD   CONCAT4 (chop_, DB_TYPE, _, read_block)
#define DB_WRITE_BLOCK_METHOD  CONCAT4 (chop_, DB_TYPE, _, write_block)
#define DB_SYNC_METHOD         CONCAT4 (chop_, DB_TYPE, _, sync)
#define DB_CLOSE_METHOD        CONCAT4 (chop_, DB_TYPE, _, close)

#define DB_STORE_TYPE          CONCAT4 (chop_, DB_TYPE, _, block_store_t)



static errcode_t
DB_READ_BLOCK_METHOD (chop_block_store_t *store,
		      const chop_block_key_t *key, chop_buffer_t *buffer,
		      size_t *size)
{
  errcode_t err;
  DB_DATA_TYPE db_key, db_content;
  DB_STORE_TYPE *gdbm = (DB_STORE_TYPE *)store;

  CHOP_KEY_TO_DB (&db_key, key);

  DB_READ (gdbm->db, db_key, &db_content);
  if (!db_content.dptr)
    {
      *size = 0;
      return CHOP_STORE_BLOCK_UNAVAIL;
    }

  err = chop_buffer_push (buffer, db_content.dptr, db_content.dsize);
  *size = db_content.dsize;

  free (db_content.dptr);

  return err;
}

static errcode_t
DB_WRITE_BLOCK_METHOD (chop_block_store_t *store,
		       const chop_block_key_t *key,
		       const char *buffer, size_t size)
{
  int err;
  DB_STORE_TYPE *gdbm = (DB_STORE_TYPE *)store;
  DB_DATA_TYPE db_key, db_content;

  CHOP_KEY_TO_DB (&db_key, key);
  db_content.dptr = (char *)buffer;
  db_content.dsize = size;

  err = DB_WRITE (gdbm->db,
		  db_key, db_content,
		  DB_WRITE_REPLACE_FLAG /* FIXME: Really? */);
  if (err)
    return CHOP_STORE_ERROR;

  return 0;
}

#ifdef __GNUC__
# define _CHOP_UNUSED __attribute__ ((__unused__))
#else
# define _CHOP_UNUSED
#endif

static errcode_t
DB_SYNC_METHOD (chop_block_store_t *store)
{
  DB_STORE_TYPE *gdbm _CHOP_UNUSED = (DB_STORE_TYPE *)store;

  DB_SYNC (gdbm->db);

  return 0;
}

#undef _CHOP_UNUSED

static errcode_t
DB_CLOSE_METHOD (chop_block_store_t *store)
{
  DB_STORE_TYPE *gdbm = (DB_STORE_TYPE *)store;

  /* `db_close ()' calls `db_sync ()' */
  DB_CLOSE (gdbm->db);

  return 0;
}
