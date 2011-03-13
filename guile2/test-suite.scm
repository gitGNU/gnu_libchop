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
  #:use-module (chop stores)
  #:use-module (chop hash)
  #:use-module (chop cipher)
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
         (c (chopper-generic-open (lookup-class "fixed_size_chopper") s 777)))
    (and (chopper? c)
         (eq? (chopper-stream c) s)
         (= (chopper-typical-block-size c) 777))))

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


;;;
;;; Stores.
;;;

(test-begin "stores")

(test-assert "dummy"
  (let ((s (dummy-block-store-open "foo")))
    (store-write-block s #vu8(1 2 3) #vu8(7 7))
    #t))

(test-assert "gdbm"
  (let ((file (tmpnam)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (let ((s (file-based-block-store-open (lookup-class "gdbm_block_store")
                                              file
                                              (logior O_RDWR O_CREAT)
                                              #o644))
              (k #vu8(1 2 3 4 5))
              (v (u8-list->bytevector (iota 256))))
          (store-write-block s k v)
          (let ((r (bytevector=? v (store-read-block s k))))
            (store-close s)
            r)))
      (lambda ()
        (delete-file file)))))

(test-end)


;;;
;;; Hash.
;;;

(test-begin "hash")

(test-assert "disjoint type"
  (and (hash-method? hash-method/sha1)
       (string=? "MD5" (hash-method-name hash-method/md5))))

(test-assert "hash-size"
  (and (= (hash-size hash-method/sha1)   20)
       (= (hash-size hash-method/rmd160) 20)
       (= (hash-size hash-method/sha256) 32)))

(test-assert "lookup-hash-method"
  (every (lambda (h)
           (eq? h (lookup-hash-method (hash-method-name h))))
         (list hash-method/tiger hash-method/md5
               hash-method/haval hash-method/sha256)))

(test-assert "bytevector-hash"
  (every (lambda (bv h e)
           (bytevector=? e (bytevector-hash h bv)))
         (list #vu8() (string->utf8 "hello") (string->utf8 "world"))
         (list hash-method/sha1 hash-method/sha1 hash-method/md5)
         (map hex-string->bytevector
              '("da39a3ee5e6b4b0d3255bfef95601890afd80709"
                "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
                "7d793037a0760186574b0282f2f435e7"))))

(test-end)


;;;
;;; Ciphers.
;;;

(test-begin "cipher")

(test-assert "disjoint algorithm type"
  (and (cipher-algorithm? cipher-algorithm/blowfish)
       (string=? "AES" (cipher-algorithm-name cipher-algorithm/aes))))

(test-assert "cipher-algorithm-key-size"
  (and (= (cipher-algorithm-key-size cipher-algorithm/blowfish) 16)
       (= (cipher-algorithm-key-size cipher-algorithm/aes) 16)
       (= (cipher-algorithm-key-size cipher-algorithm/aes256) 32)))

(test-assert "cipher-algorithm-block-size"
  (= (cipher-algorithm-block-size cipher-algorithm/aes256) 16))

(test-assert "lookup-cipher-algorithm"
  (every (lambda (c)
           (eq? c (lookup-cipher-algorithm (cipher-algorithm-name c))))
         (list cipher-algorithm/blowfish
               cipher-algorithm/safer-sk128
               cipher-algorithm/des-sk
               cipher-algorithm/aes)))

(test-assert "disjoint mode type"
  (and (cipher-mode? cipher-mode/ecb)
       (string=? "CBC" (cipher-mode-name cipher-mode/cbc))))

(test-assert "lookup-cipher-mode"
  (every (lambda (m)
           (eq? m (lookup-cipher-mode (cipher-mode-name m))))
         (list cipher-mode/ecb
               cipher-mode/cfb
               cipher-mode/cbc)))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
