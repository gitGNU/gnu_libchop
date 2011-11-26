;;; Copyright (C) 2011  Ludovic Courtès <ludo@gnu.org>
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

(define-module (chop choppers)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop internal)
  #:use-module (chop objects)
  #:export (chopper?
            whole-stream-chopper-open
            fixed-size-chopper-open
            anchor-based-chopper-open
            chopper-generic-open
            chopper-read-block
            chopper-stream
            chopper-typical-block-size))

(define-libchop-type chopper "chopper"
  chopper?
  wrap-chopper unwrap-chopper)

(define unwrap-stream
  (let ((stream (lookup-class "stream")))
    (lambda (s)
      (unwrap-object stream s))))


;;;
;;; Constructors.
;;;

(define whole-stream-chopper-open
  (let ((f (libchop-type-constructor "whole_stream_chopper_open"
                                     ('*)
                                     "whole_stream_chopper" wrap-chopper)))
    (lambda (stream)
      "Return a new chopper that returns all of STREAM as a single block."
      (f (unwrap-stream stream)))))

(define fixed-size-chopper-open
  (let ((f (libchop-type-constructor "fixed_size_chopper_init"
                                     ('* size_t int)
                                     "fixed_size_chopper" wrap-chopper)))
    (lambda* (stream block-size #:optional (pad? #f))
      "Return a new chopper that uses STREAM as its source and returns blocks
of BLOCK-SIZE bytes.  If PAD? is true then blocks are padded to be exactly
BLOCK-SIZE byte long."
      (f (unwrap-stream stream)
         block-size
         (if pad? 1 0)))))

(define anchor-based-chopper-open
  (let ((f (libchop-type-constructor "anchor_based_chopper_init"
                                     ('* size_t unsigned-long)
                                     "anchor_based_chopper" wrap-chopper)))
    (lambda* (stream window-size #:optional (fingerprint-mask 8191))
      "Return a new chopper that uses STREAM as its source and produces
variable-width blocks depending on the input data.  See the manual for
details."
      (f (unwrap-stream stream) window-size fingerprint-mask))))

(define* (chopper-generic-open class stream #:optional (block-size 8192))
  "Return a chopper of type CLASS draining input from STREAM and return
blocks of BLOCK-SIZE bytes on average."
  (and (object-is-a? class (lookup-class "chopper_class"))
       (let* ((c (gc-malloc (class-instance-size class)))
              (p (libchop-slot-ref "chopper_class" "generic_open" '*
                                   (unwrap-class class)
                                   "#include <chop/choppers.h>"))
              (f (pointer->procedure chop-error-t p `(* ,size_t *)))
              (e (f (unwrap-stream stream) block-size c)))
         (if (= e 0)
             (register-libchop-object! (wrap-chopper c))
             (raise-chop-error e)))))


;;;
;;; Methods.
;;;

(define (chopper-read-block c)
  "Return a bytevector with the block read from chopper C."
  (let ((m     (libchop-method (unwrap-chopper c)
                               "chopper" "read_block"
                               ('* '* '*)))
        (buf   (make-empty-buffer))
        (size* (make-size_t-pointer)))
    (m (unwrap-chopper c) buf size*)
    ;; XXX: We assume SIZE* is consistent with BUF.
    (buffer->bytevector buf)))

(define chopper-stream
  (let ((stream (lookup-class "stream")))
    (lambda (c)
      "Return the input stream C is associated with."
      (wrap-object stream
                   (libchop-slot-ref "chopper" "stream" '*
                                     (unwrap-chopper c))))))

(define (chopper-typical-block-size c)
  "Return the \"typical\" size of blocks produced by chopper C."
  (libchop-slot-ref "chopper" "typical_block_size" size_t
                    (unwrap-chopper c)))
