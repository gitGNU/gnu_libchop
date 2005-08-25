#!/bin/sh
#
# Everyone writes his own revision control system, why not me?
# This simple RCS uses `chop-archiver' and relies on content-based
# addressing like Monotone, GIT, Bazaar 2, GNU Arch 2, etc.

[ $# -lt 1 ] && \
  echo "Usage: `basename $0` COMMAND [OPTIONS] [FILES]" && \
  exit 1

chop_rcs_version="0.1"
chop_archiver="./chop-archiver"

metadata_dir=".chop-rcs"
file_list="${metadata_dir}/files"
tmp_file="${metadata_dir}/{tmp}"
prev_revision="${metadata_dir}/previous-revision"
prev_revision_cache="${metadata_dir}/previous-revision-cache"
current_revision_cache="${metadata_dir}/current-revision-cache"

case "$1" in
    help) echo "`basename $0` ${chop_rcs_version}"
          echo ""
	  echo "Yet another revision control system (RCS), just for the fun of it!"
	  echo "Implemented as a shell script that uses \`chop-archiver' to do the"
	  echo "actual job."
	  ;;

    init) mkdir ${metadata_dir}
          echo "none" > ${prev_revision}
	  ;;

    add)  shift
          for file in $@
	  do
	    if [ -f "$file" ]; then
	      echo "$file" >> "$file_list"
	    else
	      echo "`basename $0`: $file: not found" >&2
	    fi
	  done
	  cat "$file_list" | sort | uniq > "${tmp_file}"
	  mv "${tmp_file}" "${file_list}"
	  ;;

    commit)
         : > ${current_revision_cache}
         for file in `cat "${file_list}"`
	 do
	   ( echo -n "${file} " && \
	     ( ${chop_archiver} --archive "${file}" )) \
	   >> ${current_revision_cache} || \
	   echo "`basename $0`: ${file}: failed to commit"
	 done
	 cp ${prev_revision_cache} ${tmp_file} && \
	 cat ${current_revision_cache} | sort | uniq > ${prev_revision_cache}
	 ${chop_archiver} --archive ${prev_revision_cache} \
	 > ${prev_revision} && \
	 diff -uBb ${tmp_file} ${prev_revision_cache} && \
	 rm ${current_revision_cache} && \
	 rm ${tmp_file}
	 ;;

    changes)
         # We could compute a list of file/handle and diff it with
	 # ${prev_revision_cache}.
esac
