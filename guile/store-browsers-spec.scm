;;;; Copyright (C) 2006 Ludovic Court�s
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

(define-module (store-browsers-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (srfi srfi-1)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-store-browser-wrapset>))


;;;
;;; Wrapset.
;;;

(define-class <chop-store-browser-wrapset> (<gw-guile-wrapset>)
  #:id 'store-browsers
  #:dependencies '(standard core))



;;;
;;; Include files.
;;;

(define-method (global-declarations-cg (ws <chop-store-browser-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/store-browsers.h>\n\n"
	"#include \"core-support.h\"\n"
	"#include \"store-browsers-support.c\"\n\n"))



;;;
;;; Wrapping.
;;;

(define-method (initialize (ws <chop-store-browser-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop-store-browsers")

  (next-method ws (append '(#:module (chop store-browsers)) initargs))

  ;; error codes


  ;; types

  (wrap-as-chop-object! ws
			#:name '<store-browser>
			#:c-type-name "chop_store_browser_t *"
			#:c-const-type-name "const chop_store_browser_t *")

  ;; constructors

  (wrap-function! ws
		  #:name 'avahi-store-browser-open
		  #:c-name "chop_avahi_store_browser_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned null-ok) domain-name)
				(scm                           discovery-proc)
				(scm                           removal-proc)
				((<store-browser> out)         browser)))



  ;; methods

  (wrap-function! ws
		  #:name 'store-browser-iterate
		  #:returns '<errcode>
		  #:c-name "chop_store_browser_iterate"
		  #:arguments '(((<store-browser> caller-owned) browser)
				(unsigned-int                   msecs)))

  (wrap-function! ws
		  #:name 'store-browser-loop
		  #:returns '<errcode>
		  #:c-name "chop_store_browser_loop"
		  #:arguments '(((<store-browser> caller-owned) browser)))

)

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; End:

;;; arch-tag: 28abc61f-178f-43ce-aa2a-bbc0cdf09b49