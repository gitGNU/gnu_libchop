;;; Copyright (C) 2010  Ludovic Courtès <ludo@gnu.org>
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
  #:export (class?
            class-name
            class-parent
            class-inherits?
            lookup-class

            object-class
            object-is-a?)
  #:re-export (object?))

(eval-when (load eval compile)
  (define (print-class c p)
    (format p "#<chop-class ~a ~x (~x)>"
            (class-name c)
            (object-address c)
            (pointer-address (unwrap-class c)))))

(define-wrapped-pointer-type class?
  wrap-class unwrap-class print-class)

(define-compile-time-value %offset-of-class
  (c-offset-of "class" "chop_object_t"
               "#include <chop/objects.h>"
               `("-L" ,%libchop-libdir "-lchop")
               %libchop-cc
               %libchop-cppflags))

(define (object-class obj)
  (and (object? obj)
       ;; XXX: Ugly hack: assume a pointer lives at slot 0.
       (let ((ptr (make-pointer (+ (pointer-address (struct-ref obj 0))
                                   %offset-of-class))))
         (register-libchop-object! (wrap-class (dereference-pointer ptr))))))

(define-compile-time-value %offset-of-name
  (c-offset-of "name" "chop_class_t"
               "#include <chop/objects.h>"
               `("-L" ,%libchop-libdir "-lchop")
               %libchop-cc
               %libchop-cppflags))

(define (class-name c)
  "Return the name of class C."
  (let ((ptr (make-pointer (+ (pointer-address (unwrap-class c))
                              %offset-of-name))))
    (pointer->string (dereference-pointer ptr))))

(define-compile-time-value %offset-of-parent
  (c-offset-of "parent" "chop_class_t"
               "#include <chop/objects.h>"
               `("-L" ,%libchop-libdir "-lchop")
               %libchop-cc
               %libchop-cppflags))

(define (class-parent c)
  "Return the parent of class C."
  (let ((ptr (make-pointer (+ (pointer-address (unwrap-class c))
                              %offset-of-parent))))
    (register-libchop-object! (wrap-class (dereference-pointer ptr)))))

(define (class-inherits? c p)
  "Return #t if class C inherits from class P."
  (define root (lookup-class "class"))
  (let loop ((c c))
    (or (eq? c p)
        (if (eq? c root)
            #f
            (loop (pk 'parent (class-parent c)))))))

(define (object-is-a? o c)
  "Return #t if O is an instance of C."
  (class-inherits? (object-class o) c))

(define lookup-class
  (let ((f (libchop-function '* "class_lookup" ('*))))
    (lambda (name)
      "Return the class called NAME or #f if no such class exists."
      (let ((ptr (f (string->pointer name))))
        (if (null-pointer? ptr)
            #f
            (register-libchop-object! (wrap-class ptr)))))))
