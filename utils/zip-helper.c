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

/* Helpers to deal with zip/unzip filter classes.  */


/* Look for zip/unzip filter classes nicknamed SHORT_NAME.  */
static void
get_zip_filter_classes (const char *short_name,
			const chop_zip_filter_class_t **zip_class,
			const chop_unzip_filter_class_t **unzip_class)
{
  if (short_name == NULL)
    {
      /* Default to `zlib'.  */
      *zip_class = &chop_zlib_zip_filter_class;
      *unzip_class = &chop_zlib_unzip_filter_class;
    }
  else
    {
      size_t len;
      char *zip_name, *unzip_name;

      len = strlen (short_name);
      zip_name = (char *) alloca (len + 50);
      unzip_name = (char *) alloca (len + 50);

      strcpy (zip_name, short_name);
      strcat (zip_name, "_zip_filter");
      strcpy (unzip_name, short_name);
      strcat (unzip_name, "_unzip_filter");

      *zip_class = (chop_zip_filter_class_t *) chop_class_lookup (zip_name);
      if ((!*zip_class)
	  || (!chop_object_is_a ((chop_object_t *) *zip_class,
				 &chop_zip_filter_class_class)))
	goto not_found;

      *unzip_class =
	(chop_unzip_filter_class_t *) chop_class_lookup (unzip_name);
      if ((!*unzip_class)
	  || (!chop_object_is_a ((chop_object_t *) *unzip_class,
				 &chop_unzip_filter_class_class)))
	goto not_found;

      return;

    not_found:
      {
	const char *which;

	which = (*zip_class == NULL) ? zip_name : unzip_name;
	fprintf (stderr, "%s: zip/unzip classes not found\n",
		 which);
	exit (1);
      }
    }
}

/* arch-tag: d018f0d9-90f4-47cb-a53b-0fae9d33eae4
 */
