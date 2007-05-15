;;; Copyright (C) 2007 Ludovic Courtès
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this software; see the file COPYING.  If not,
;;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;; MA 02139, USA.
;;;

(define-module (block-server-spec)
  #:use-module (core-spec)
  #:use-module (logs-spec)

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

(define-class <chop-block-server-wrapset> (<gw-guile-wrapset>)
  #:id 'block-server
  #:dependencies '(standard core logs))



;;;
;;; Include files.
;;;

(define-method (global-declarations-cg (ws <chop-block-server-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/block-server.h>\n\n"
	"#include \"core-support.h\"\n"
	"#include \"block-server-support.c\"\n\n"))



;;;
;;; Wrapping.
;;;

(define-method (initialize (ws <chop-block-server-wrapset>) initargs)

  (slot-set! ws 'shlib-path "libguile-chop-block-server")

  (next-method ws (append '(#:module (chop block-server)) initargs))

  ;; error codes


  ;; types

  (wrap-as-chop-object! ws
			#:name '<store-publisher>
			#:c-type-name "chop_store_publisher_t *"
			#:c-const-type-name "const chop_store_publisher_t *")

  ;; constructors

  (wrap-function! ws
		  #:name 'avahi-store-publisher-open
		  #:c-name "chop_avahi_store_publisher_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned)         service-name)
                                ((mchars caller-owned null-ok) host)
                                (unsigned-int                  port)
                                (bool                          use-tls?)
                                ((<input-buffer> null-ok)      openpgp-fingerprint)
				((<store-publisher> out)       publisher)))

  (wrap-function! ws
                  #:name 'avahi-store-publisher-log
                  #:c-name "chop_avahi_store_publisher_log"
                  #:returns '<log>
                  #:arguments '((<store-publisher>  avahi-publisher)))


  ;; methods

  (wrap-function! ws
		  #:name 'store-publisher-iterate
		  #:returns '<errcode>
		  #:c-name "chop_scm_store_publisher_iterate"
		  #:arguments '(((<store-publisher> caller-owned) publisher)
				(unsigned-int                     msecs)))

  (wrap-function! ws
		  #:name 'store-publisher-loop
		  #:returns '<errcode>
		  #:c-name "chop_scm_store_publisher_loop"
		  #:arguments '(((<store-publisher> caller-owned) publisher))))


;;; Local Variables:
;;; mode: scheme
;;; scheme-program-name: "guile"
;;; End:

;;; arch-tag: 632a7ff7-5dc4-4bfa-8f5f-09d00ed87543
