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

(define-module (chop streams)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop internal)
  #:use-module (ice-9 format)
  #:export (file-stream-open
            port->stream
            stream->port))

(eval-when (eval load compile)
  (define (print-stream obj port)
    (format port "#<chop-stream ~x (~x)>"
            (object-address obj)
            (pointer-address (unwrap-stream obj)))))

(define-wrapped-pointer-type stream print-stream)

(define file-stream-open
  (let ((f (libchop-type-constructor "file_stream_open" ('*)
                                     "file_stream" wrap-stream)))
    (lambda (path)
      (f (string->pointer path)))))

;(file-stream-open "/dev/null")
