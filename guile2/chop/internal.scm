;;; Copyright (C) 2010, 2011, 2012, 2013  Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (chop config)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:re-export (%libchop-libdir)
  #:export (c-offset-of
            c-size-of
            c-integer-value
            compile-time-value
            define-compile-time-value
            pointer+
            mode_t
            make-pointer-pointer
            make-size_t-pointer
            dereference-size_t
            make-int-pointer
            dereference-int
            pointer-list->foreign-array

            define-libchop-type
            wrap-object
            unwrap-object
            register-libchop-object!
            object?

            class?
            wrap-class
            unwrap-class
            lookup-class

            %libchop-cc
            %libchop-cppflags
            %libchop-libs

            libchop
            libchop-function
            libchop-method
            libchop-slot-ref
            libchop-type-constructor
            chop-error-t
            define-error-code
            raise-chop-error
            error/not-impl
            gc-malloc
            gc-malloc-pointerless

            class-instance-size
            make-empty-buffer
            buffer->bytevector
            register-weak-reference

            proxy/leave-as-is
            proxy/eventually-close))


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

(define gc-malloc
  (let ((alloc
         (pointer->procedure '* (dynamic-func "scm_gc_malloc" (dynamic-link))
                             (list size_t '*)))
        (what (string->pointer "chop")))
    (lambda (size)
      (alloc size what))))

(define gc-realloc
  ;; Don't use `scm_gc_realloc' because it wants the previous size, and
  ;; because `GC_realloc' below really knows that size.
  (pointer->procedure '* (dynamic-func "GC_realloc" (dynamic-link))
                      (list '* size_t)))

(define-syntax compile-time-value
  (syntax-rules ()
    "Evaluate the given expression at compile time.  The expression must
evaluate to a simple datum."
    ((_ exp)
     (let-syntax ((v (lambda (s)
                       (let ((val exp))
                         (syntax-case s ()
                           (_ (datum->syntax s val)))))))
       v))))

(define-syntax define-compile-time-value
  ;; Define the given symbol by evaluating its body at compile-time.
  (syntax-rules ()
    ((_ name exp)
     (define-syntax name
       (lambda (s)
         (let ((val exp))
           (syntax-case s ()
             (_ (datum->syntax s val)))))))))

(eval-when (eval load compile)

  (define %libtool (getenv "LIBTOOL"))

  (define* (evaluate-c-integer-expression expr
                                          includes libs
                                          #:optional
                                          (cc "cc")
                                          (cppflags '()) (cflags '())
                                          (ldflags '()))
    "Return the value of EXPR, a C expression that evaluates to an 8-bit
integer."
    (let ((file (string-append (tmpnam) ".c"))
          (exe  (tmpnam)))
      (dynamic-wind
        (lambda ()
          (let ((out (open-output-file file)))
            (format out
                    (compile-time-value
                     (string-append "#include <stdio.h>~%"
                                    "#include <stdlib.h>~%~a~%"
                                    "int main () { printf (\"%i\\n\", (~a));~%"
                                    "return EXIT_SUCCESS; }"))
                    includes expr)
            (close-port out)))
        (lambda ()
          (let* ((cc  (cond ((string? %libtool)
                             `(,@(string-tokenize %libtool)
                               "--mode=link"
                               ,@(if (string? cc)
                                     (string-tokenize cc)
                                     (list cc))))
                            ((string? cc)
                             (string-tokenize cc))
                            (else cc)))
                 (cmd `(,@cc ,file "-o" ,exe
                             ,@cppflags ,@cflags ,@ldflags ,@libs))
                 (s   (apply system* cmd)))
            (if (= 0 (status:exit-val s))
                (let* ((in  (open-input-pipe
                             (if %libtool
                                 (string-append %libtool " --mode=execute \""
                                                exe "\"")
                                 (format #f
                                         "LD_LIBRARY_PATH=\"~a:$LD_LIBRARY_PATH\" \"~a\""
                                         (dirname libchop.so)
                                         exe))))
                       (val (read in))
                       (ret (close-pipe in)))
                  (if (and (= 0 (status:exit-val ret))
                           (number? val))
                      (pk 'c-expression expr val)
                      (throw 'runtime-error 'evaluate-c-integer-expression
                             ret expr exe)))
                (throw 'compilation-error 'evaluate-c-integer-expression
                       s expr cmd))))
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
           (format #f "(char *)(&((~a *) 0)->~a) - (char *) 0"
                   type field)
           includes libs rest)))


;;;
;;; Compilation environment.
;;;

;; FIXME: Substitute correct values for these.

(eval-when (eval load compile)
  (define libchop.so
    (find file-exists?
          (map (cut string-append %libchop-libdir "/" <>)
               `(,@(if %libtool
                       '("libchop.la")
                       '())
                 "libchop.so" ".libs/libchop.so"))))
  (define %libchop-libs
    (list libchop.so
          "-Wl,-rpath" (dirname libchop.so)))
  (define %libchop-cc
    (or (getenv "CC") "cc"))
  (define %libchop-cppflags
    `("-I" ,%libchop-includedir)))

(define mode_t
  (case (compile-time-value (c-size-of "mode_t"
                                       "#include <sys/types.h>"
                                       %libchop-libs
                                       %libchop-cc
                                       %libchop-cppflags))
    ((1) uint8)
    ((2) uint16)
    ((4) uint32)
    ((8) uint64)
    (else (error "could not determine size of `mode_t'"))))

(define (make-pointer-pointer)
  "Return a pointer to a region of sizeof(void *) bytes."
  (gc-malloc-pointerless (compile-time-value (sizeof '*))))

(define-compile-time-value sizeof-size_t
  (sizeof size_t))

(define (make-size_t-pointer)
  "Return a pointer to a region of sizeof(size_t) bytes."
  (gc-malloc-pointerless sizeof-size_t))

(define (dereference-size_t p)
  "Return the size_t value pointed to by P."
  (bytevector-uint-ref (pointer->bytevector p sizeof-size_t)
                       0 (native-endianness)
                       sizeof-size_t))


(define-compile-time-value sizeof-int
  (sizeof int))

(define (make-int-pointer)
  "Return a pointer to a region of sizeof(int) bytes."
  (gc-malloc-pointerless sizeof-int))

(define (dereference-int p)
  "Return the int value pointed to by P."
  (bytevector-uint-ref (pointer->bytevector p sizeof-int)
                       0 (native-endianness)
                       sizeof-int))

(define (pointer-list->foreign-array lst)
  "Return a pointer to a foreign array containing the pointers listed in LST."
  (let* ((count (length lst))
         (array (make-bytevector (* count (sizeof '*)))))
    (fold (lambda (ptr index)
            (bytevector-uint-set! array index (pointer-address ptr)
                                  (native-endianness)
                                  (sizeof '*))
            (+ index (sizeof '*)))
          0
          lst)
    (bytevector->pointer array)))


;;;
;;; Libchop-specific helpers.
;;;

(define libchop
  (dynamic-link (string-append %libchop-libdir "/libchop")))

(define chop-error-t int)

(define (raise-chop-error e)
  (throw 'chop-error e))

(define-syntax c-integer-value
  (syntax-rules ()
    "Return the value of the C integer C-NAME, which could be an enum value,
a macro, etc.  The result is a compile-time constant."
    ((_ c-name includes)
     (compile-time-value
      (evaluate-c-integer-expression c-name includes
                                     %libchop-libs
                                     %libchop-cc
                                     %libchop-cppflags)))))

(define-syntax define-error-code
  (syntax-rules ()
    "Define variable NAME to match the value of C-NAME."
    ((_ name c-name)
     (define name
       (c-integer-value c-name "#include <chop/errors.h>")))))

(define-error-code error/not-impl "CHOP_ERR_NOT_IMPL")

(define-compile-time-value %offset-of-instance_size
  (c-offset-of "instance_size" "chop_class_t"
               "#include <chop/objects.h>"
               ;; XXX: Should use `libtool --mode=link' but that wouldn't
               ;; work once installed.
               %libchop-libs
               %libchop-cc
               %libchop-cppflags))

(define (class-pointer-instance-size ptr)
  "Return the size of instances of PTR, a pointer to a class."
  (let ((bytes (pointer->bytevector ptr (sizeof size_t)
                                    %offset-of-instance_size)))
    (bytevector-uint-ref bytes 0 (native-endianness) (sizeof size_t))))

(define (class-name-instance-size name)
  "Return the class of instances of NAME."
  (class-pointer-instance-size
   (dynamic-pointer (string-append "chop_" name "_class")
                    libchop)))

(define-syntax full-class-type-name
  (syntax-rules ()
    "Given an abbreviated class name, e.g., \"chopper\", return the C type
name, e.g., \"chop_chopper_t\"."
    ((_ name)
     (compile-time-value (string-append "chop_" name "_t")))))

(define-syntax class-includes
  (syntax-rules ()
    "Return the include directive for class NAME."
    ((_ name)
     (compile-time-value (format #f "#include <chop/~as.h>" name)))))

(define %libchop-types
  ;; Mapping of `class' objects to wrap/unwrap pair.
  (make-hash-table))

;; Trick to delay loading of `(chop objects)'.
(define-syntax class-parent
  (syntax-rules ()
    ((_ c)
     ((@ (chop objects) class-parent) c))))

(define (wrap-object class ptr)
  "Wrap PTR, which points to an arbitrary libchop object.  This is meant to
be used for object types of other modules, e.g., when (chop choppers) wants
to wrap a `stream' object."
  (let loop ((class class))
    (match (hashq-ref %libchop-types class)
      ((wrap . _)
       (register-libchop-object! (wrap ptr)))
      (else
       (loop (class-parent class))))))

(define (unwrap-object class obj)
  "Unwrap OBJ, which points to an arbitrary libchop object."
  (let loop ((class class))
    (match (hashq-ref %libchop-types class)
      ((_ . unwrap)
       (unwrap obj))
      (else
       (loop (class-parent class))))))

;; Defined later.
(define register-libchop-type! #f)
(define lookup-class identity)

(define (print-object obj port)
  "Print libchop object OBJ to PORT."
  (let* ((class      ((@ (chop objects) object-class) obj))
         (class-name ((@ (chop objects) class-name) class))
         (ptr        (unwrap-object class obj)))
    (format port "#<chop ~a ~x (~x)>"
            class-name (object-address obj)
            (pointer-address ptr))))

(define-syntax define-libchop-type
  (lambda (s)
    (syntax-case s ()
      ((_ name c-name pred wrap unwrap)
       (string? (syntax->datum #'c-name))
       #'(begin
           (define-wrapped-pointer-type name
             pred wrap unwrap
             print-object)
           (register-libchop-type! (lookup-class c-name) wrap unwrap))))))

(define-syntax libchop-function
  (lambda (s)
    (syntax-case s ()
      ((_ ret c-name (args ...))
       (string? (syntax->datum #'c-name))
       (with-syntax ((fqfn (string-append "chop_"
                                          (syntax->datum #'c-name))))
         #'(pointer->procedure ret (dynamic-func fqfn libchop)
                               (list args ...))))
      ((_ c-name (args ...))
       (with-syntax (((params ...) (generate-temporaries #'(args ...))))
         #'(let ((f (libchop-function chop-error-t c-name (args ...))))
             (lambda (params ...)
               (let ((err (f params ...)))
                 (or (= 0 err)
                     (raise-chop-error err))))))))))

(define-syntax libchop-method
  (lambda (s)
    (syntax-case s (includes)
      ((_ ret object class method (args ...) (includes inc))
       (and (string? (syntax->datum #'class))
            (string? (syntax->datum #'method)))
       #'(let ((p (dereference-pointer
                   (make-pointer
                    (+ (compile-time-value
                        (c-offset-of method
                                     (full-class-type-name class)
                                     inc
                                     %libchop-libs
                                     %libchop-cc
                                     %libchop-cppflags))
                       (pointer-address object))))))
           (if (null-pointer? p)
               (raise-chop-error error/not-impl)
               (pointer->procedure ret p (list args ...)))))
      ((_ object class method (args ...) (includes inc))
       (with-syntax (((params ...) (generate-temporaries #'(args ...))))
         #'(let ((f (libchop-method chop-error-t object
                                    class method (args ...)
                                    (includes inc))))
             (lambda (params ...)
               (let ((err (f params ...)))
                 (or (= 0 err)
                     (raise-chop-error err)))))))
      ((_ ret object class method (args ...))
       #'(libchop-method ret object class method (args ...)
                         (includes (class-includes class))))
      ((_ object class method (args ...))
       #'(libchop-method object class method (args ...)
                         (includes (class-includes class)))))))

(define-syntax libchop-slot-ref
  (syntax-rules ()
    "Return FIELD of OBJECT-PTR, which has type CLASS or a sub-class thereof;
FIELD is assumed to have foreign type TYPE."
    ((_ class field type object-ptr)
     (libchop-slot-ref class field type object-ptr
                       (class-includes class)))
    ((_ class field type object-ptr includes)
     (let* ((ptr
             (pointer+ object-ptr
                       (compile-time-value
                        (c-offset-of field
                                     (full-class-type-name class)
                                     includes
                                     %libchop-libs
                                     %libchop-cc
                                     %libchop-cppflags)))))
       (car (parse-c-struct ptr (list type)))))))


(define %libchop-objects
  (make-weak-key-hash-table 100))

(define (register-libchop-object! obj)
  "Register OBJ as a genuine libchop object.  Return OBJ."
  (hashq-set! %libchop-objects obj #t)
  obj)

(define (object? obj)
  "Return #t if OBJ is a genuine libchop object."
  (hashq-ref %libchop-objects obj))

(define %destroy-object
  ;; The object finalizer.
  (dynamic-func "chop_object_destroy" libchop))

(define-syntax make-object
  (lambda (s)
    "Allocate and initialize a libchop object."
    (syntax-case s ()
      ((_ (args ...) class-name wrap init)
       (string? (syntax->datum #'class-name))
       (with-syntax (((params ...)
                      (generate-temporaries #'(args ...))))
         #'(lambda (params ...)
             (let ((ptr (gc-malloc (class-name-instance-size class-name))))
               (init params ... ptr)
               (set-pointer-finalizer! ptr %destroy-object)
               (register-libchop-object! (wrap ptr)))))))))

(define-syntax libchop-type-constructor
  (syntax-rules ()
    "Return a constructor for CLASS-NAME that initializes instances using the
C function NAME and wraps the resulting pointer with WRAP."
    ((_ ret name (args ...) class-name wrap)
     (make-object (args ...) class-name wrap
                  (libchop-function ret name (args ... '*))))
    ((_ name (args ...) class-name wrap)
     (make-object (args ...) class-name wrap
                  (libchop-function name (args ... '*))))))


;;;
;;; Initialization.
;;;

;; Provide our own allocation routines so that the GC knows how much heap is
;; really being used.  This is especially important since object like zip
;; filters are associated with a fair amount of heap.  The test for this is:
;;
;;   (let loop ((i 300000)) (make-zlib-zip-filter) (loop (1- i)))

(define init
  (libchop-function "init_with_allocator" ('* '* '*)))

(define (%malloc size class)
  (gc-malloc size))

(define (%realloc ptr size class)
  (let ((size (if (= 0 size) 1 size)))
    (gc-realloc ptr size)))

(define (%free ptr class)
  ;; Let the GC do its job.
  #t)

;; Keep the C closures in global variables to prevent them from being freed.
(define %malloc-c-func
  (procedure->pointer '* %malloc (list size_t '*)))
(define %realloc-c-func
  (procedure->pointer '* %realloc (list '* size_t '*)))
(define %free-c-func
  (procedure->pointer void %free '(* *)))

(init %malloc-c-func %realloc-c-func %free-c-func)


;; Bootstrap the object system.

(define %root-class #f)

(set! register-libchop-type!
      (lambda (class wrap unwrap)
        ;; Dummy bootstrap definition.
        (set! %root-class
              (list wrap unwrap))))

;; Boot!
(define-libchop-type class "class"
  class?
  wrap-class unwrap-class)

(set! register-libchop-type!
      (lambda (class wrap unwrap)
       "Register the type CLASS"
       (hashq-set! %libchop-types class (cons wrap unwrap))))

;; Add `lookup-class', which can now be used by `define-libchop-type'.
(set! lookup-class
  (let ((f (libchop-function '* "class_lookup" ('*))))
    (lambda (name)
      "Return the class called NAME or #f if no such class exists."
      (let ((ptr (f (string->pointer name))))
        (if (null-pointer? ptr)
            #f
            (register-libchop-object! (wrap-class ptr)))))))

;; Register type `class'.
(apply register-libchop-type! (lookup-class "class") %root-class)

(define (class-instance-size class)
  "Return the size in bytes of instances of CLASS."
  (class-pointer-instance-size (unwrap-class class)))


;;;
;;; The `chop_buffer_t' buffers.
;;;

(define register-weak-reference
  (let ((refs (make-weak-key-hash-table)))
    (lambda (source target)
      (hashq-set! refs source target))))

(define (pointer+ p x)
  (make-pointer (+ (pointer-address p) x)))

(define-compile-time-value %size-of-chop_buffer_t
  (c-size-of "chop_buffer_t"
             "#include <chop/buffers.h>"
             %libchop-libs
             %libchop-cc
             %libchop-cppflags))

(define make-empty-buffer
  (let ((init      (libchop-function "buffer_init" ('* size_t)))
        (destroy   (dynamic-func "chop_buffer_return" libchop)))
    (lambda* (#:optional (size 0))
      "Return a pointer to a new `chop_buffer_t' object."
      (let ((buf (gc-malloc %size-of-chop_buffer_t)))
        (init buf size)
        (set-pointer-finalizer! buf destroy)
        buf))))

(define buffer->bytevector
  (let ((buf-offset   (compile-time-value
                       (c-offset-of "buffer" "chop_buffer_t"
                                    "#include <chop/buffers.h>"
                                    %libchop-libs
                                    %libchop-cc
                                    %libchop-cppflags)))
        (size-offset (compile-time-value
                      (c-offset-of "size" "chop_buffer_t"
                                   "#include <chop/buffers.h>"
                                   %libchop-libs
                                   %libchop-cc
                                   %libchop-cppflags))))
    (lambda (buf)
      "Return a bytevector with the contents of BUF."
      (let* ((size  (bytevector-uint-ref (pointer->bytevector
                                          buf %size-of-chop_buffer_t)
                                         size-offset (native-endianness)
                                         (sizeof size_t)))
             (bv    (pointer->bytevector (dereference-pointer
                                          (pointer+ buf buf-offset))
                                         size)))
        ;; Note: we re-use `buf->buffer' as is instead of reallocating a new
        ;; one.
        (register-weak-reference bv buf)
        bv))))


;;;
;;; Proxies.
;;;

(define proxy/leave-as-is
  (c-integer-value "CHOP_PROXY_LEAVE_AS_IS" "#include <chop/chop.h>"))
(define proxy/eventually-close
  (c-integer-value "CHOP_PROXY_EVENTUALLY_CLOSE" "#include <chop/chop.h>"))
