struct chop_stream
{
  char *name;

  /* block size once broken */
  size_t block_size;

  /* common methods */
  errcode_t (* sbreak) (const struct chop_stream *, size_t, size_t *);
  errcode_t (* get_block) (const struct chop_stream *, size_t,
			   struct chop_block *);
  size_t (* preferred_block_size) (const struct chop_stream *);
  void (* close) (struct chop_stream *);

  /* placeholders */
  void *_u1;
  void *_u2;
  void *_u3;
  void *_u4;
  void *_u5;
};


struct chop_file_stream
{
  chop_stream_t stream;

  int    fd;
  size_t fsys_block_size;
  size_t size;
  void  *map;
};
