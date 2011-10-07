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

(define-module (store-browsers-spec)
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

(define-class <chop-store-browser-wrapset> (<gw-guile-wrapset>)
  #:id 'store-browsers
  #:dependencies '(standard core logs))



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

  (wrap-function! ws
                  #:name 'avahi-store-browser-log
                  #:c-name "chop_scm_avahi_store_browser_log"
                  #:returns '<log>
                  #:arguments '((<store-browser>  avahi-browser)))


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
