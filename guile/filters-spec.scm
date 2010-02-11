;;; libchop -- a utility library for distributed storage and data backup
;;; Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
;;; Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)
;;;
;;; Libchop is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Libchop is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with libchop.  If not, see <http://www.gnu.org/licenses/>.

(define-module (filters-spec)
  #:use-module (core-spec)
  #:use-module (logs-spec)

  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13) ;; strings

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-filters-wrapset>))


;; the wrapset itself.

(define-class <chop-filters-wrapset> (<gw-guile-wrapset>)
  #:id 'filters
  #:dependencies '(standard core logs))



(define-method (global-declarations-cg (ws <chop-filters-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n"
	"#include <chop/filters.h>\n\n"
	"#include \"core-support.h\"\n"
	"#include \"filters-support.c\"\n\n"))




(define-method (initialize (ws <chop-filters-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop filters)) initargs))

  ;; error codes

  (let ((c->scm-enum (lambda (c-name)
		       (string->symbol
			(string-map (lambda (chr)
				      (if (char=? chr #\_)
					  #\- chr))
				    (string-downcase
				     (symbol->string c-name)))))))
    (for-each (lambda (errcode)
		(wrap-constant! ws
				#:name (symbol-append 'filter-error/
						      (c->scm-enum
						       errcode))
				#:type 'long
				#:value (string-append
					 "CHOP_FILTER_"
					 (symbol->string errcode))))
	    '(FULL EMPTY UNHANDLED_FAULT))

    (wrap-constant! ws
		    #:name 'filter-error/generic
		    #:type 'long
		    #:value "CHOP_FILTER_ERROR"
		    #:description "Generic filter error"))

  ;; filters

  (wrap-as-chop-object! ws
			#:name '<filter>
			#:c-type-name "chop_filter_t *"
			#:c-const-type-name "const chop_filter_t *")

  ;; constructors

  (wrap-function! ws
		  #:name 'zlib-zip-filter-init
		  #:c-name "chop_zlib_zip_filter_init_alloc"
		  #:returns '<errcode>
		  #:arguments '((int zlib-compression-level (default -1))
				(int input-size (default 0))
				((<filter> out) filter)))

  (wrap-function! ws
		  #:name 'zlib-unzip-filter-init
		  #:c-name "chop_zlib_unzip_filter_init_alloc"
		  #:returns '<errcode>
		  #:arguments '((int input-size (default 0))
				((<filter> out) filter)))

  ;; generic constructors

  (wrap-constant! ws
                  #:name 'zip-compression-level/default
                  #:type 'int
                  #:value "CHOP_ZIP_FILTER_DEFAULT_COMPRESSION"
                  #:description "The default zip compression level")

  (wrap-function! ws
		  #:name 'zip-filter-init
		  #:c-name "chop_generic_zip_filter_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) zip-type)
                                (int compression-level (default -1))
                                (int input-size        (default 0))
				((<filter> out)        filter)))

  (wrap-function! ws
		  #:name 'unzip-filter-init
		  #:c-name "chop_generic_unzip_filter_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) zip-type)
                                (int input-size        (default 0))
				((<filter> out)        filter)))

  ;; methods

  (wrap-function! ws
                  #:name 'filter-log
                  #:c-name "chop_filter_log"
                  #:returns '(<log> null-ok)
                  #:arguments '(((<filter> aggregated)  filter))
                  #:description "Return the log attached to @var{filter}."))


;; arch-tag: e5d3a1f4-e328-4cfe-b878-51afc1a2d4ea
