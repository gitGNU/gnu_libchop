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

(define-module (chop internal)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (define-wrapped-pointer-type
            c-offset-of
            c-size-of
            define-compile-time-value

            register-libchop-object!
            object?

            %libchop-libdir
            %libchop-cc
            %libchop-cppflags

            libchop
            libchop-function
            libchop-type-constructor
            chop-error-t))


;;;
;;; Generic FFI tools---should be in `(system foreign)'.
;;;

(define gc-malloc-pointerless
  (let ((alloc
         (pointer->procedure '* (dynamic-func "scm_gc_malloc_pointerless"
                                              (dynamic-link))
                             (list size_t '*)))
        (what (string->pointer "chop")))
    (lambda (size)
      (alloc size what))))

(define-syntax define-wrapped-pointer-type
  (lambda (stx)
    (syntax-case stx ()
      ((_ pred wrap unwrap print) ;; hygiene
       (with-syntax ((type-name (datum->syntax #'pred (gensym)))
                     (%wrap     (datum->syntax #'wrap (gensym))))
         #'(begin
             (define-record-type type-name
               (%wrap pointer)
               pred
               (pointer unwrap))
             (define wrap
               ;; Use a weak hash table to preserve pointer identity, i.e.,
               ;; PTR1 == PTR2 <-> (eq? (wrap PTR1) (wrap PTR2)).
               (let ((ptr->obj (make-weak-value-hash-table)))
                 (lambda (ptr)
                   (or (hash-ref ptr->obj ptr)
                       (let ((o (%wrap ptr)))
                         (hash-set! ptr->obj ptr o)
                         o)))))
             (set-record-type-printer! type-name print))))
      ((_ type-name print) ;; lazyness
       (let* ((type-name*  (syntax->datum #'type-name))
              (pred-name   (datum->syntax #'type-name
                                          (symbol-append type-name* '?)))
              (wrap-name   (datum->syntax #'type-name
                                          (symbol-append 'wrap- type-name*)))
              (%wrap-name  (datum->syntax #'type-name
                                          (symbol-append '%wrap- type-name*)))
              (unwrap-name (datum->syntax #'type-name
                                          (symbol-append 'unwrap-
                                                         type-name*))))
         (with-syntax ((pred   pred-name)
                       (wrap   wrap-name)
                       (%wrap  %wrap-name)
                       (unwrap unwrap-name))
           #'(define-wrapped-pointer-type pred wrap unwrap print)))))))

(eval-when (eval load compile)

  (define* (evaluate-c-integer-expression expr
                                          includes libs
                                          #:optional
                                          (cc "gcc")
                                          (cppflags '()) (cflags '())
                                          (ldflags '()))
    "Return the value of EXPR, a C expression that evaluates to an 8-bit
integer."
    (let ((file (string-append (tmpnam) ".c"))
          (exe  (tmpnam)))
      (dynamic-wind
        (lambda ()
          (let ((out (open-output-file file)))
            (format out "~a~%int main () { return (~a); }"
                    includes expr)
            (close-port out)))
        (lambda ()
          (let ((s (apply system* cc file "-o" exe
                          (append cppflags cflags ldflags libs))))
            (if (= 0 (status:exit-val s))
                (let ((s (system* exe)))
                  (status:exit-val s))
                (throw 'compilation-error 'evaluate-c-integer-expression
                       s expr cc))))
        (lambda ()
          (false-if-exception (delete-file file))
          (false-if-exception (delete-file exe))))))

  (define (c-size-of type includes libs . rest)
    "Return the size of TYPE, a C type, in bytes."
    (apply evaluate-c-integer-expression
           (format #f "sizeof (~a)" type)
           includes libs rest))

  (define (c-offset-of field type includes libs . rest)
    "Return the offset of FIELD in TYPE, a C struct, in bytes."
    (apply evaluate-c-integer-expression
           (pk 'expt (format #f "(char *)(&((~a *) 0)->~a) - (char *) 0"
                             type field))
           includes libs rest)))

(define-syntax define-compile-time-value
  ;; Define the given symbol by evaluating its body at compile-time.
  (syntax-rules ()
    ((_ name exp)
     (define-syntax name
       (lambda (s)
         (let ((val exp))
           (syntax-case s ()
             (_ (datum->syntax s val)))))))))


;;;
;;; Compilation environment.
;;;

;; FIXME: Substitute correct values for these.

(eval-when (eval load compile)
  (define %libchop-libdir
    (or (getenv "libchop_libdir") "../../src"))
  (define %libchop-cc
    "gcc")
  (define %libchop-cppflags
    `("-I" ,(or (getenv "libchop_includedir") "../../include"))))


;;;
;;; Libchop-specific helpers.
;;;

(define libchop
  (dynamic-link (string-append %libchop-libdir "/libchop")))

(define chop-error-t int)

(define (raise-chop-error e)
  (throw 'chop-error e))

(define-compile-time-value %offset-of-instance_size
  (c-offset-of "instance_size" "chop_class_t"
               "#include <chop/objects.h>"
               `("-L" ,%libchop-libdir "-lchop")
               %libchop-cc
               %libchop-cppflags))

(define (class-instance-size name)
  "Return the class of instances of NAME."
  (let* ((klass (dynamic-pointer (string-append "chop_" name "_class")
                                 libchop))
         (bytes (pointer->bytevector klass (sizeof size_t)
                                     %offset-of-instance_size)))
    (bytevector-uint-ref bytes 0 (native-endianness) (sizeof size_t))))

(define-syntax libchop-function
  (lambda (s)
    (syntax-case s ()
      ((_ ret c-name (args ...))
       (string? (syntax->datum #'c-name))
       (with-syntax ((fqcn (string-append "chop_"
                                          (syntax->datum #'c-name))))
         #'(pointer->procedure ret (dynamic-func fqcn libchop)
                               (list args ...))))
      ((_ c-name (args ...))
       (with-syntax (((params ...) (generate-temporaries #'(args ...))))
         #'(let ((f (libchop-function chop-error-t c-name (args ...))))
             (lambda (params ...)
               (let ((err (f params ...)))
                 (or (= 0 err)
                     (raise-chop-error err))))))))))

(define %libchop-objects
  (make-weak-key-hash-table 100))

(define (register-libchop-object! obj)
  "Register OBJ as a genuine libchop object.  Return OBJ."
  (hashq-set! %libchop-objects obj #t)
  obj)

(define (object? obj)
  "Return #t if OBJ is a genuine libchop object."
  (hashq-ref %libchop-objects obj))

(define-syntax libchop-type-constructor
  (lambda (s)
    (syntax-case s ()
      ((_ name (args ...) class-name wrap)
       (string? (syntax->datum #'class-name))
       (with-syntax (((params ...) (generate-temporaries #'(args ...))))
         #'(let ((make (libchop-function name (args ... '*))))
             (lambda (params ...)
               (let ((ptr (gc-malloc-pointerless
                           (class-instance-size class-name))))
                 (make params ... ptr)
                 (register-libchop-object! (wrap ptr))))))))))

(define init (libchop-function "init" ()))

(init)
