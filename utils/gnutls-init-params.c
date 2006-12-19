/* Template for the GNUtls parameter initialization and storage/retrieval
   from disk.  */

#if (!defined PARAMS_KIND) || (!defined PARAMS_T)
# error "a number of macros must be defined first."
#endif

#define CONCAT3(x, y, z)        x ## y ## z
#define INITIALIZE_PARAMS(kind) CONCAT3 (chop_tls_initialize_, kind, _params)
#define TOSTRING_(x) #x
#define TOSTRING(x)  TOSTRING_ (x)


errcode_t
INITIALIZE_PARAMS (PARAMS_KIND) (PARAMS_T *params,
				 const char *config_dir,
				 const char *filename)
{
  errcode_t err = 0;
  int file;

  PARAMS_INIT (params);
  if (open_config_file (config_dir, filename, O_RDONLY, &file))
    {
      size_t export_size;
      unsigned char *export;

      printf ("%s: initializing TLS `" TOSTRING (PARAMS_KIND)
	      "' key exchange parameters "
	      "(this may take a while)...\n", program_name);
      GENERATE_PARAMS (*params, PARAMS_BITS);

      export_size = PARAMS_BITS * 4;

      while (1)
	{
	  export = (unsigned char *) alloca (export_size);
	  err = PARAMS_EXPORT (*params, GNUTLS_X509_FMT_PEM,
			       export, &export_size);
	  if ((!err) || (err != GNUTLS_E_SHORT_MEMORY_BUFFER))
	    break;

	  export_size *= 2;
	}

      if (err)
	fprintf (stderr, "%s: error while exporting TLS `"
		 TOSTRING (PARAMS_KIND) "' parameters: %s\n",
		 program_name, gnutls_strerror (err));
      else
	{
	  err = open_config_file (config_dir, filename,
				  O_WRONLY | O_CREAT, &file);
	  if (err)
	    com_err (program_name, err, "while creating config file \"%s\"",
		     filename);
	  else
	    {
	      dump_to_file (file, (char *) export, export_size);
	      close (file);
	    }
	}
    }
  else
    {
      void *content;
      size_t size;

      err = file_content (file, &content, &size);
      if (err)
	{
	  com_err (program_name, err, "while reading config file \"%s\"",
		   filename);
	  return err;
	}
      else
	{
	  gnutls_datum_t datum;

	  datum.data = content;
	  datum.size = size;

	  err = PARAMS_IMPORT (*params, &datum, GNUTLS_X509_FMT_PEM);
	  free_file_content (content, size);

	  if (err)
	    {
	      fprintf (stderr, "%s: %s: while importing TLS "
		       TOSTRING (PARAMS_KIND) " parameters\n",
		       program_name, gnutls_strerror (err));
	      return CHOP_ERR_NOT_IMPL;
	    }
	}
    }

  return err;
}

#undef TOSTRING
#undef TOSTRING_
#undef INITIALIZE_PARAMS
#undef CONCAT3

/* arch-tag: fbc3f7ea-5be9-401c-950b-6587f22b3ef8
 */
