#!/bin/sh
# libchop -- a utility library for distributed storage and data backup
# Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
# Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)
#
# Libchop is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Libchop is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with libchop.  If not, see <http://www.gnu.org/licenses/>.

# Everyone writes his own revision control system, why not me?
# This simple RCS uses `chop-archiver' and relies on content-based
# addressing like Monotone, GIT, Bazaar 2, GNU Arch 2, etc.

[ $# -lt 1 ] && \
  echo "Usage: `basename $0` COMMAND [OPTIONS] [FILES]" && \
  exit 1

chop_rcs_version="0.1"
chop_archiver="./chop-archiver"

metadata_dir=".chop-rcs"
tmp_file="${metadata_dir}/{tmp}"
tree_root="${metadata_dir}/tree-root"
file_list="${metadata_dir}/files"
prev_revision="${metadata_dir}/previous-revision"
prev_revision_cache="${metadata_dir}/previous-revision-cache"
current_revision_cache="${metadata_dir}/current-revision-cache"

# An arbitrary limit.
max_directory_depth=14

find_tree_root ()
{
  dir="."
  i=0
  while [ ${i} -lt ${max_directory_depth} ]
  do
    if [ -f "${dir}/${tree_root}" ]; then
      tree_root_val="`cat ${dir}/${tree_root}`"
      return 0
    fi
    dir="${dir}/.."
    i="`expr ${i} + 1`"
  done
  return 1
}

initialize ()
{
  if ! find_tree_root; then
    echo "error: not in a chop-rcs directory"
    exit 1
  fi
  touch "${tree_root_val}/${prev_revision_cache}"
}

# Create a revision in the file and dump it to the standard output.
# The revision displayed is a list of filename/index associations.
create_revision ()
{
  for file in `cat "${tree_root_val}/${file_list}"`
  do
    ( echo -n "${file} " && \
      ( ${chop_archiver} --archive "${file}" )) || \
    echo "`basename $0`: ${file}: failed to commit" >&2
  done
}
	

case "$1" in
    help) echo "`basename $0` ${chop_rcs_version}"
          echo ""
	  echo "Yet another revision control system (RCS), just for the fun of it!"
	  echo "Implemented as a shell script that uses \`chop-archiver' to do the"
	  echo "actual job."
	  echo
	  echo "Recognized commands:"
	  echo
	  cat "$0" | \
	  grep -e '^[[:space:]]\+\([a-z]\+\))' | \
	  sed -e's/^[[:space:]]\+\([a-z-]\+\)).*$/  \1/g'
	  echo
	  ;;

    init) mkdir ${metadata_dir}
          echo "`pwd`" > ${tree_root}
          echo "none" > ${prev_revision}
	  ;;

    tree-root)
	  initialize
	  echo "${tree_root_val}"
	  exit 0
	  ;;

    inventory)
          initialize
	  cat "${file_list}"
	  exit 0
	  ;;

    add)  initialize
          shift
          for file in $@
	  do
	    if [ -f "$file" ]; then
	      echo "`pwd`/$file" >> "$file_list"
	    else
	      echo "`basename $0`: $file: not found" >&2
	    fi
	  done
	  cat "$file_list" | sort | uniq > "${tmp_file}"
	  mv "${tmp_file}" "${file_list}"
	  ;;

    commit)
         initialize
	 root="${tree_root_val}"

         create_revision > "${root}/${current_revision_cache}"
	 cp "${root}/${prev_revision_cache}" "${root}/${tmp_file}" && \
	 cat "${root}/${current_revision_cache}" | sort | \
	 uniq > "${root}/${prev_revision_cache}"

	 ${chop_archiver} --archive "${root}/${prev_revision_cache}" \
	 > "${root}/${prev_revision}" && \
	 diff -uBb "${root}/${tmp_file}" "${root}/${prev_revision_cache}" && \
	 rm "${root}/${current_revision_cache}" && \
	 rm "${root}/${tmp_file}"
	 exit 1
	 ;;

    changes)
         initialize
	 root="${tree_root_val}"

	 if [ ! -f "${root}/${prev_revision_cache}" ]; then
	   echo "error: no previous revision"
	   exit 1
	 fi
	 create_revision > "${root}/${tmp_file}"
	 diff -uBb "${root}/${prev_revision_cache}" "${root}/${tmp_file}"
	 rm "${root}/${tmp_file}"
	 exit 0
	 ;;
         
esac
