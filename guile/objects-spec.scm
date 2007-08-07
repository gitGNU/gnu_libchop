;;;; Copyright (C) 2007  Ludovic Courtès
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

(define-module (objects-spec)
  #:use-module (objects-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)

  ;; Imported for their side effects.
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things.
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-objects-wrapset>))


;; The wrapper itself.
(define-class <chop-objects-wrapset> (<gw-guile-wrapset>)
  #:id 'objects
  #:dependencies '(standard core))

(define-method (global-declarations-cg (ws <chop-objects-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/objects.h>\n"
	"#include \"objects-support.c\"\n\n"))


(define-method (initialize (ws <chop-objects-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")
  (next-method ws (append '(#:module (chop objects)) initargs))

  (wrap-as-wct! ws
                #:allowed-options '(caller-owned out)
                #:wcp-equal-predicate "gwrap_chop_object_equal"
                #:name '<chop-class>
                #:c-type-name "chop_class_t *"
                #:c-const-type-name "const chop_class_t *")

  (wrap-function! ws
                  #:name 'class-lookup
                  #:returns '<chop-class>
                  #:c-name "chop_class_lookup"
                  #:arguments '(((mchars caller-owned) class-name))
                  #:description "Return the class named @var{class-name}.")

  (wrap-function! ws
                  #:name 'class-name
                  #:returns '(mchars callee-owned)
                  #:c-name "chop_class_name"
                  #:arguments '((<chop-class> class))
                  #:description "Return the name of @var{class}.")

  (wrap-function! ws
                  #:name 'object-is-a?
                  #:returns 'bool
                  #:c-name "chop_scm_object_is_a"
                  #:arguments '((scm obj)
                                (<chop-class> class))))

;;; arch-tag: 2cd2fe41-15a1-4c31-ab75-2715e0e21fb1
