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
            object-is-a?
            object=?
            serialize-object/ascii
            serialize-object/binary
            deserialize-object/ascii
            deserialize-object/binary
            error/deserial-too-short
            error/deserial-corrupt-input)
  #:re-export (object?))

(define-error-code error/deserial-too-short "CHOP_DESERIAL_TOO_SHORT")
(define-error-code error/deserial-corrupt-input "CHOP_DESERIAL_CORRUPT_INPUT")

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

(define (object=? o1 o2)
  "Return true if objects O1 and O2 are structurally equal."
  (or (eq? o1 o2)
      (let ((c1 (object-class o1))
            (c2 (object-class o2)))
        (and (eq? c1 c2)
             (let ((p (libchop-slot-ref "class" "equal" '*
                                        (unwrap-class c1)
                                        "#include <chop/objects.h>")))
               (if (null-pointer? p)
                   #f
                   (let ((o=? (pointer->procedure int p '(* *))))
                     (not (= 0 (o=? (unwrap-object c1 o1)
                                    (unwrap-object c2 o2)))))))))))


;;;
;;; Serialization.
;;;

(define %ascii
  (c-integer-value "CHOP_SERIAL_ASCII" "#include <chop/objects.h>"))

(define %binary
  (c-integer-value "CHOP_SERIAL_BINARY" "#include <chop/objects.h>"))

(define (%serialize-object o m)
  "Return the serialization of O according to M as a bytevector."
  (let* ((c (object-class o))
         (s (libchop-method (unwrap-class c)
                            "class" "serializer"
                            ('* int '*)
                            (includes "#include <chop/objects.h>")))
         (b (make-empty-buffer)))
    (s (struct-ref o 0) m b)
    (buffer->bytevector b)))

(define (serialize-object/ascii o)
  "Return an ASCII serialization of O as a string."
  (pointer->string ; to consume the trailing zero
   (bytevector->pointer (%serialize-object o %ascii))))

(define (serialize-object/binary o)
  "Return a compact binary serialization of O as a bytevector."
  (%serialize-object o %binary))

(define (%deserialize-object c bv m)
  "Return the instance of class C obtained by deserializing BV according to M."
  (let* ((s (libchop-method (unwrap-class c)
                            "class" "deserializer"
                            ('* size_t int '* '*)
                            (includes "#include <chop/objects.h>")))
         (p (gc-malloc-pointerless (class-instance-size c)))
         (r (make-size_t-pointer))
         (e (s (bytevector->pointer bv) (bytevector-length bv) m
               p r)))
    (values (wrap-object c p)
            (dereference-size_t r))))

(define (deserialize-object/ascii class str)
  "Deserialize STR and return a new instance of CLASS that corresponds."
  (%deserialize-object class (string->utf8 str) %ascii))

(define (deserialize-object/binary class bv)
  "Deserialize BV and return a new instance of CLASS that corresponds."
  (%deserialize-object class bv %binary))
