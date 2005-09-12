;;;; Copyright (C) 2005 Ludovic Courtès
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;; MA 02139, USA.
;;;;

(define-module (filters-spec)
  #:use-module (core-spec)
  #:use-module (stores-spec)

  #:use-module (oop goops)
  #:use-module (srfi srfi-1)

  #:use-module (g-wrap)
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
  #:dependencies '(standard core stores))



(define-method (global-declarations-cg (ws <chop-filters-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/stores.h>\n"
	"#include <chop/filters.h>\n\n"
	"#include \"filters-support.c\"\n\n"))




(define-method (initialize (ws <chop-filters-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop filters)) initargs))

  (wrap-as-wct! ws
		#:name '<filter>
		#:c-type-name "chop_filter_t *"
		#:c-const-type-name "const chop_filter_t *"
		#:destroy-value-function-name "chop_filter_dealloc")

  ;; constructors

  (wrap-function! ws
		  #:name 'zlib-zip-filter-init
		  #:c-name "chop_zlib_zip_filter_init_alloc"
		  #:returns '<errcode>
		  #:arguments '((int zlib-compression-level)
				(int input-size)
				((<filter> out) filter)))

  (wrap-function! ws
		  #:name 'zlib-unzip-filter-init
		  #:c-name "chop_zlib_unzip_filter_init_alloc"
		  #:returns '<errcode>
		  #:arguments '((int input-size)
				((<filter> out) filter)))

  (wrap-function! ws
		  #:name 'filtered-store-open
		  #:c-name "chop_filtered_store_open_alloc"
		  #:returns '<errcode>
		  #:arguments '((<filter> input-filter)
				(<filter> output-filter)
				(<store> backend)
				((<store> out) store))))


;; arch-tag: e5d3a1f4-e328-4cfe-b878-51afc1a2d4ea
