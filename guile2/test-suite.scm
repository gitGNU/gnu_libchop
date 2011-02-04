;;; libchop -- a utility library for distributed storage and data backup
;;; Copyright (C) 2008, 2010, 2011  Ludovic Courtès <ludo@gnu.org>
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

(define-module (chop test-suite)
  #:use-module (chop core)
  #:use-module (chop objects)
  #:use-module (chop streams)
  #:use-module (chop choppers)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "objects")

(test-equal "hex strings"
  '(#vu8(1 2 3 4 5 6 7 8 9 10) . 20)

  (call-with-values
      (lambda ()
        (hex-string->bytevector
         (bytevector->hex-string #vu8(1 2 3 4 5 6 7 8 9 10))))
    cons))

(test-equal "base32 strings"
  '(#vu8(1 2 3 4 5 6 7 8 9 10) . 16)

  (call-with-values
      (lambda ()
        (base32-string->bytevector
         (bytevector->base32-string #vu8(1 2 3 4 5 6 7 8 9 10))))
    cons))

(test-assert "lookup-class & co."
  (and (every (lambda (name)
                (equal? (class-name (lookup-class name))
                        name))
              '("fixed_size_chopper" "file_stream"
                "hash_block_indexer" "chk_block_indexer"))
       (object-is-a? (lookup-class "fixed_size_chopper")
                     (lookup-class "chopper_class"))))

(test-end)


;;;
;;; Streams.
;;;

(test-begin "streams")

(define (make-random-bytevector n)
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256))
            (loop (1+ i)))
          bv))))

(test-assert "/dev/null"
  (stream? (file-stream-open "/dev/null")))

(test-assert "error/stream-end"
  (catch 'chop-error
    (lambda ()
      (let ((bv (make-bytevector 12)))
        (stream-read! (file-stream-open "/dev/null") bv)
        #f))
    (lambda (key err . args)
      (= err error/stream-end))))

(test-assert "mem-stream"
  (let* ((in  (uint-list->bytevector (iota 123) (native-endianness) 4))
         (out (make-bytevector (bytevector-length in)))
         (s   (mem-stream-open in)))
    (let loop ((total 0))
      (define bv (make-bytevector 7))
      (let ((read (false-if-exception (stream-read! s bv))))
        (if (not read)
            (begin
              (stream-close s)
              (and (equal? out in)
                   (= total (bytevector-length in))))
            (begin
              (bytevector-copy! bv 0 out total read)
              (loop (+ total read))))))))

(test-assert "stream->port"
  (let* ((input  (make-random-bytevector 7777))
         (port   (stream->port (mem-stream-open input)))
         (output (make-bytevector (bytevector-length input))))
    (let loop ((total 0))
      (if (>= total (bytevector-length input))
          (and (= total (bytevector-length input))
               (bytevector=? input output))
          (let ((read (get-bytevector-n! port output
                                         total (1+ (random 10)))))
            (and (or (eof-object? read) (> read 0))
                 (loop (+ read total))))))))

(test-end)


;;;
;;; Choppers.
;;;

(test-begin "choppers")

(test-assert "chopper-generic-open"
  (let* ((s (mem-stream-open #vu8(1 2 3)))
         (c (chopper-generic-open (lookup-class "fixed_size_chopper") s)))
    (chopper? c)))

(test-assert "chopper-read-block"
  (let* ((input   (make-random-bytevector 7777))
         (stream  (mem-stream-open input))
         (chopper (anchor-based-chopper-open stream 99))
         (output  (make-bytevector (bytevector-length input))))
    (let loop ((total 0))
      (catch 'chop-error
        (lambda ()
          (let ((block (chopper-read-block chopper)))
            (bytevector-copy! block 0 output total
                              (bytevector-length block))
            (loop (+ (bytevector-length block) total))))
        (lambda (key err . args)
          (and (= err error/stream-end)
               (= total (bytevector-length input))
               (bytevector=? input output)))))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
