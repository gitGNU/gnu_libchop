/* Continuous integration of libchop with Hydra/Nix.
   Copyright (C) 2011, 2012  Ludovic Courtès <ludo@gnu.org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

{ nixpkgs ? <nixpkgs>
, guile ? (import <nixpkgs> {}).guile
}:

let
  pkgs = import nixpkgs {};

  meta = {
    description = "libchop, tools & library for data backup and distributed storage";

    longDescription =
      '' Libchop is a set of utilities and library for data backup and
         distributed storage.  Its main application is chop-backup, an
         encrypted backup program that supports data integrity checks,
         versioning at little cost, distribution among several sites,
         selective sharing of stored data, adaptive compression, and more.
         The library itself, which chop-backup builds upon, implements
         storage techniques such as content-based addressing, content hash
         keys, Merkle trees, similarity detection, and lossless compression.
         It makes it easy to combine them in different ways.  The
         ‘chop-archiver’ and ‘chop-block-server’ tools, illustrated in the
         manual, provide direct access to these facilities from the command
         line.  It is written in C and has Guile (Scheme) bindings.
      '';

    homepage = http://nongnu.org/libchop/;
    license = "GPLv3+";

    maintainers = [ "Ludovic Courtès <ludo@gnu.org>" ];
  };

  buildOutOfSourceTree = true;
  succeedOnFailure = true;
  keepBuildDirectory = true;

  buildNativeInputsFrom = pkgs: [ guile ] ++
    (with pkgs; [ pkgconfig gperf ]);

  buildInputsFrom = pkgs:
    with pkgs;
     [ zlib bzip2 lzo
       libgcrypt
       gdbm db4 tdb
       gnutls libuuid
     ];

  jobs = {
    tarball =
      { libchopSrc ? { outPath = <libchop>; }
      , gnulib ? { outPath = <gnulib>; } }:

      pkgs.releaseTools.sourceTarball {
        name = "libchop-tarball";
        src = libchopSrc;
        buildNativeInputs = (buildNativeInputsFrom pkgs)
          ++ (with pkgs; [ git texinfo texLive gettext_0_18 cvs emacs ]);
        buildInputs = buildInputsFrom pkgs;

        preAutoconf =
          # Autopoint 0.18.1.1 announces itself as 0.18.1; work around it.
          '' sed -i configure.ac \
                 -e 's/AM_GNU_GETTEXT_VERSION(\[0\.18\.1\.1])/AM_GNU_GETTEXT_VERSION([0.18.1])/g'
          ''
          +
          '' mkdir -p gnulib
             cp -rv "${gnulib}/"* gnulib
             chmod -R 755 gnulib

             ./gnulib/gnulib-tool --update
          '';

        postDist =
          # Tell Hydra about our manual and release notes.
          '' make -C doc libchop.pdf libchop.html
             cp -rv doc/libchop.{pdf,html} "$out"

             emacs --batch --load=org.el --visit=NEWS \
                   --funcall org-export-as-html-batch
             cp -v NEWS.html "$out"

             cat >> $out/nix-support/hydra-build-products <<EOF
doc-pdf manual $out/libchop.pdf
doc manual $out/libchop.html index.html
doc release-notes $out/NEWS.html
EOF
          '';

        inherit meta succeedOnFailure keepBuildDirectory;
      };

    build =
      { tarball ? jobs.tarball {}
      , system ? builtins.currentSystem
      }:

      let pkgs = import nixpkgs { inherit system; }; in
      pkgs.releaseTools.nixBuild {
        name = "libchop";
        src = tarball;
        buildNativeInputs = buildNativeInputsFrom pkgs;
        buildInputs = buildInputsFrom pkgs;
        inherit meta succeedOnFailure keepBuildDirectory
          buildOutOfSourceTree;
      };

    coverage =
      { tarball ? jobs.tarball {}
      , system ? builtins.currentSystem
      }:

      let pkgs = import nixpkgs { inherit system; }; in
      pkgs.releaseTools.coverageAnalysis {
        name = "libchop";
        src = tarball;
        buildNativeInputs = buildNativeInputsFrom pkgs;
        buildInputs = buildInputsFrom pkgs;
        inherit meta succeedOnFailure keepBuildDirectory
          buildOutOfSourceTree;
      };
  };
in
  jobs
