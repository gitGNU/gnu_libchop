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

(define-module (chop streams)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs io ports) #:select (make-custom-binary-input-port))
  #:use-module (chop internal)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:export (stream?
            file-stream-open
            mem-stream-open
            filtered-stream-open

            stream-read!
            stream-close

            port->stream
            stream->port

            error/unknown-stream
            error/stream-end))

(define-libchop-type stream "stream"
  stream?
  wrap-stream unwrap-stream)


;;;
;;; Constructors.
;;;

(define file-stream-open
  (let ((f (libchop-type-constructor "file_stream_open" ('*)
                                     "file_stream" wrap-stream)))
    (lambda (path)
      "Return a new input stream to the contents of the file at PATH."
      (f (string->pointer path)))))

(define mem-stream-open
  (let ((f (libchop-type-constructor void "mem_stream_open" ('* size_t '*)
                                     "mem_stream" wrap-stream)))
    (lambda (bv)
      "Return a new input stream whose contents are taken from bytevector BV."
      (f (bytevector->pointer bv)
         (bytevector-length bv)
         %null-pointer))))

(define filtered-stream-open
  (let ((f (libchop-type-constructor "filtered_stream_open"
                                     ('* int '* int)
                                     "filtered_stream" wrap-stream)))
    (lambda* (backend filter #:optional (close-backend? #f))
      "Return a new input stream whose input is drained from stream BACKEND
and filtered through FILTER.  If CLOSE-BACKEND? is true, then BACKEND will be
closed when the returned stream is closed."
      (let ((s (f (unwrap-stream backend)
                  (if close-backend?
                      proxy/eventually-close
                      proxy/leave-as-is)
                  (unwrap-object (lookup-class "filter") filter)
                  0)))
        (register-weak-reference s filter)
        (register-weak-reference s backend)
        s))))


;;;
;;; Methods.
;;;

(define (stream-read! s bv)
  "Read from stream S into bytevector BV.  Return the number of bytes read.
If the end-of-stream was reached, raise a `chop-error' with value
ERROR/STREAM-END."
  (let ((m   (libchop-method (unwrap-stream s)
                             "stream" "read"
                             ('* '* size_t '*)))
        (out (make-bytevector (sizeof size_t))))
    (m (unwrap-stream s) (bytevector->pointer bv)
       (bytevector-length bv)
       (bytevector->pointer out))
    (bytevector-uint-ref out 0 (native-endianness) (sizeof size_t))))

(define (stream-close s)
  "Close stream S."
  (let ((m (libchop-method void (unwrap-stream s)
                           "stream" "close"
                           ('*))))
    (m (unwrap-stream s))))

(define (stream->port s)
  "Return an input port wrapped around stream S."
  (define (read! bv start count)
    (if (= 0 start)
        (or (false-if-exception (stream-read! s bv)) 0)
        (let* ((count (- (bytevector-length bv) start))
               (bv*   (make-bytevector count))
               (read  (or (false-if-exception (stream-read! s bv*)) 0)))
          (bytevector-copy! bv* 0 bv start read)
          read)))

  (make-custom-binary-input-port (format #f "stream ~x"
                                         (pointer-address (unwrap-stream s)))
                                 read! #f #f
                                 (cut stream-close s)))

(define-error-code error/unknown-stream "CHOP_ERR_UNKNOWN_STREAM")
(define-error-code error/stream-end "CHOP_STREAM_END")
