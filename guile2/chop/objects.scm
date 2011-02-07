;;; Copyright (C) 2010, 2011  Ludovic Courtès <ludo@gnu.org>
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

(define-module (chop objects)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop internal)
  #:use-module (ice-9 format)
  #:re-export (class?
               lookup-class)
  #:export (class-name
            class-parent
            class-inherits?

            object-class
            object-is-a?)
  #:re-export (object?))

(define (object-class obj)
  "Return the class of OBJ or #f if OBJ is not a libchop object."
  (and (object? obj)
       ;; XXX: Ugly hack: assume a pointer lives at slot 0.
       (register-libchop-object!
        (wrap-class (libchop-slot-ref "object" "class" '*
                                      (struct-ref obj 0))))))

(define (class-name c)
  "Return the name of class C."
  (pointer->string (libchop-slot-ref "class" "name" '*
                                     (unwrap-class c)
                                     "#include <chop/objects.h>")))

(define (class-parent c)
  "Return the parent of class C."
  (register-libchop-object!
   (wrap-class (libchop-slot-ref "class" "parent" '*
                                 (unwrap-class c)
                                 "#include <chop/objects.h>"))))

(define (class-inherits? c p)
  "Return #t if class C inherits from class P."
  (define root (lookup-class "class"))
  (let loop ((c c))
    (or (eq? c p)
        (if (eq? c root)
            #f
            (loop (class-parent c))))))

(define (object-is-a? o c)
  "Return #t if O is an instance of C."
  (class-inherits? (object-class o) c))
