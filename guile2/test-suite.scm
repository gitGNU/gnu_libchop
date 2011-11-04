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
  #:use-module (chop block-indexers)
  #:use-module (chop indexers)
  #:use-module (chop filters)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))

(define (with-temporary-file proc)
  (let ((file (tmpnam)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc file))
      (lambda ()
        (delete-file file)))))


;;;
;;; Objects.
;;;

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
      (let ((read (false-if-end-of-stream (stream-read! s bv))))
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

(test-assert "stream->port & get-bytevector-all"
  (let* ((input  (make-random-bytevector 7777))
         (port   (stream->port (mem-stream-open input))))
    (bytevector=? input (get-bytevector-all port))))

(test-end)


;;;
;;; Choppers.
;;;

(test-begin "choppers")

(test-assert "chopper-generic-open"
  (let* ((s (mem-stream-open #vu8(1 2 3)))
         (c (chopper-generic-open (lookup-class "fixed_size_chopper") s 777)))
    (and (chopper? c)
         (eq? (object-class c) (lookup-class "fixed_size_chopper"))
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
  (with-temporary-file
   (lambda (file)
     (let ((s (file-based-block-store-open (lookup-class "gdbm_block_store")
                                           file
                                           (logior O_RDWR O_CREAT)
                                           #o644))
           (k #vu8(1 2 3 4 5))
           (v (u8-list->bytevector (iota 256))))
       (store-write-block s k v)
       (let ((r (bytevector=? v (store-read-block s k))))
         (store-close s)
         r)))))

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
               cipher-mode/cbc
               cipher-mode/ctr)))

(test-assert "make-cipher"
  (let ((c (make-cipher cipher-algorithm/aes cipher-mode/cbc)))
    (and (cipher? c)
         (eq? cipher-algorithm/aes (cipher-algorithm c))
         (eq? cipher-mode/cbc (cipher-mode c)))))

(test-assert "set-cipher-key!"
  (let ((c (make-cipher cipher-algorithm/aes cipher-mode/cbc))
        (b (make-bytevector (cipher-algorithm-key-size cipher-algorithm/aes))))
    (set-cipher-key! c b)
    (cipher? c)))

(test-end)


;;;
;;; Block indexers.
;;;

(test-begin "block-indexers")

(test-assert "hash-block-indexer-open"
  (let ((bi (hash-block-indexer-open hash-method/sha256)))
    (and (block-indexer? bi)
         (eq? (object-class bi)
              (lookup-class "hash_block_indexer")))))

(test-assert "chk-block-indexer-open"
  (let* ((c  (make-cipher cipher-algorithm/aes256 cipher-mode/cbc))
         (bi (chk-block-indexer-open c
                                     hash-method/sha256
                                     hash-method/md5)))
    (and (block-indexer? bi)
         (eq? (object-class bi)
              (lookup-class "chk_block_indexer")))))

(test-assert "block-indexer-index-handle-class"
  (let ((bi (hash-block-indexer-open hash-method/sha256)))
    (eq? (block-indexer-index-handle-class bi)
         (lookup-class "hash_index_handle"))))

(test-assert "chk-block-indexer-open"
  (let* ((c  (make-cipher cipher-algorithm/aes256 cipher-mode/cbc))
         (bi (chk-block-indexer-open c
                                     hash-method/sha256
                                     hash-method/md5)))
    (eq? (block-indexer-fetcher-class bi)
         (lookup-class "chk_block_fetcher"))))

(test-assert "block-indexer-index"
  (let* ((s  (dummy-block-store-open "foo"))
         (bi (hash-block-indexer-open hash-method/md5))
         (bv (uint-list->bytevector (iota 555) (native-endianness) 4))
         (i  (block-indexer-index bi s bv)))
    (and (index-handle? i)
         (eq? (object-class i)
              (block-indexer-index-handle-class bi)))))

(test-assert "index-handle equality"
  (let* ((s  (dummy-block-store-open "foo"))
         (bi (hash-block-indexer-open hash-method/md5))
         (bv (uint-list->bytevector (iota 555) (native-endianness) 4))
         (i1 (block-indexer-index bi s bv))
         (i2 (block-indexer-index bi s bv)))
    (object=? i1 i2)))

(test-assert "block-indexer ASCII serialization"
  (string=? "SHA1"
            (serialize-object/ascii
             (hash-block-indexer-open hash-method/sha1))))

(test-assert "block-indexer ASCII deserialization"
  (let ((c (lookup-class "hash_block_indexer")))
    ;; XXX: There's no equality predicate for `hash_block_indexer' so that's
    ;; the best we can test.
    (object-is-a? (deserialize-object/ascii c "SHA1") c)))

(test-assert "index-handle serialization"
  (let* ((s  (dummy-block-store-open "foo"))
         (bi (hash-block-indexer-open hash-method/md5))
         (bv (uint-list->bytevector (iota 555) (native-endianness) 4))
         (i  (block-indexer-index bi s bv))
         (a  "7mgi4cnq7qcckmlxcyn47xv3ge======/8ac"))
    (and (string=? a (serialize-object/ascii i))
         (let ((b (serialize-object/binary i)))
           (and (bytevector? b)
                (= (bytevector-length b)
                   (+ 4 4 (hash-size hash-method/md5))))))))

(test-assert "index-handle deserialization"
  (let* ((s  (dummy-block-store-open "foo"))
         (c  (lookup-class "hash_index_handle"))
         (bi (hash-block-indexer-open hash-method/md5))
         (bv (uint-list->bytevector (iota 555) (native-endianness) 4))
         (i  (block-indexer-index bi s bv)))
    (and (object=? (deserialize-object/ascii c (serialize-object/ascii i))
                   i)
         (object=? (deserialize-object/binary c (serialize-object/binary i))
                   i))))

(test-assert "block-indexer-fetcher"
  (let* ((bi (hash-block-indexer-open hash-method/md5))
         (bf (block-indexer-fetcher bi)))
    (and (block-fetcher? bf)
         (eq? (object-class bf)
              (block-indexer-fetcher-class bi)))))

(test-assert "block-fetcher-fetch"
  (with-temporary-file
   (lambda (file)
     (let* ((s  (file-based-block-store-open (lookup-class "gdbm_block_store")
                                             file
                                             (logior O_RDWR O_CREAT)
                                             #o644))
            (bi (chk-block-indexer-open (make-cipher cipher-algorithm/aes256
                                                     cipher-mode/cbc)
                                        hash-method/sha256
                                        hash-method/md5))
            (bf (block-indexer-fetcher bi))
            (bv (u8-list->bytevector (iota 256)))
            (i  (block-indexer-index bi s bv)))
       (and (index-handle? i)
            (let ((r (bytevector=? bv (block-fetcher-fetch bf i s))))
              (store-close s)
              r))))))

(test-assert "block-fetcher-exists?"
  (with-temporary-file
   (lambda (file)
     (let* ((s  (file-based-block-store-open (lookup-class "gdbm_block_store")
                                             file
                                             (logior O_RDWR O_CREAT)
                                             #o644))
            (bi (hash-block-indexer-open hash-method/sha256))
            (bf (block-indexer-fetcher bi))
            (bv (u8-list->bytevector (iota 256)))
            (i  (block-indexer-index bi s bv)))
       (and (index-handle? i)
            (let ((r (block-fetcher-exists? bf i s)))
              (store-close s)
              r))))))

(test-assert "!block-fetcher-exists?"
  (with-temporary-file
   (lambda (file)
     (let* ((s  (file-based-block-store-open (lookup-class "gdbm_block_store")
                                             file
                                             (logior O_RDWR O_CREAT)
                                             #o644))
            (i  (deserialize-object/ascii (lookup-class "integer_index_handle")
                                          "00000007"))
            (bf (deserialize-object/ascii (lookup-class "integer_block_fetcher")
                                          "")))
       (and (index-handle? i)
            (block-fetcher? bf)
            (let ((r (block-fetcher-exists? bf i s)))
              (store-close s)
              (not r)))))))

(test-end)


;;;
;;; Indexers.
;;;

(test-begin "indexers")

(test-assert "tree-indexer-open"
  (let ((i (tree-indexer-open)))
    (and (indexer? i)
         (eq? (object-class i)
              (lookup-class "tree_indexer")))))

(test-assert "indexer-stream-class"
  (let ((c (indexer-stream-class (tree-indexer-open))))
    (and (class? c)
         (eq? c (lookup-class "tree_stream")))))

(test-assert "indexer-index-blocks"
  (let* ((i   (tree-indexer-open))
         (in  (mem-stream-open (u8-list->bytevector (iota 256))))
         (c   (chopper-generic-open (lookup-class "fixed_size_chopper")
                                    in 777))
         (bi  (hash-block-indexer-open hash-method/sha1))
         (s1  (dummy-block-store-open "data"))
         (s2  (dummy-block-store-open "meta-data")))
    (index-handle? (indexer-index-blocks i c bi s1 s2))))

(test-assert "indexer-fetch-stream"
  (let* ((i   (tree-indexer-open))
         (in  (mem-stream-open (u8-list->bytevector (iota 256))))
         (c   (chopper-generic-open (lookup-class "fixed_size_chopper")
                                    in 777))
         (bi  (hash-block-indexer-open hash-method/sha1))
         (bf  (block-indexer-fetcher bi))
         (s1  (dummy-block-store-open "data"))
         (s2  (dummy-block-store-open "meta-data"))
         (ih  (indexer-index-blocks i c bi s1 s2))
         (s   (indexer-fetch-stream i ih bf s1 s2)))
    (and (stream? s)
         (eq? (object-class s) (lookup-class "tree_stream")))))

(test-assert "indexer-{index-blocks,fetch-stream}"
  (with-temporary-file
   (lambda (file)
     (let* ((i   (tree-indexer-open))
            (bv  (make-random-bytevector 5555))
            (in  (mem-stream-open bv))
            (c   (chopper-generic-open (lookup-class "anchor_based_chopper")
                                       in 777))
            (bi  (chk-block-indexer-open (make-cipher cipher-algorithm/aes256
                                                      cipher-mode/cbc)
                                         hash-method/sha256
                                         hash-method/md5))
            (bf  (block-indexer-fetcher bi))
            (s   (file-based-block-store-open (lookup-class "gdbm_block_store")
                                              file
                                              (logior O_RDWR O_CREAT)
                                              #o644))
            (ih  (indexer-index-blocks i c bi s s))
            (ih2 (deserialize-object/ascii (object-class ih)
                                           (serialize-object/ascii ih)))
            (out (stream->port (indexer-fetch-stream i ih2 bf s s))))
       (let ((r (and (object=? ih ih2)
                     (bytevector=? bv (get-bytevector-all out)))))
         (store-close s)
         r)))))

(test-assert "indexer-{index-blocks,fetch-stream} with serialized index tuple"
  (with-temporary-file
   (lambda (file)
     (let* ((i     (tree-indexer-open))
            (bv    (make-random-bytevector 6666))
            (in    (mem-stream-open bv))
            (c     (chopper-generic-open (lookup-class "anchor_based_chopper")
                                         in 555))
            (bi    (chk-block-indexer-open (make-cipher cipher-algorithm/aes256
                                                        cipher-mode/cbc)
                                           hash-method/sha256
                                           hash-method/md5))
            (s     (file-based-block-store-open (lookup-class "gdbm_block_store")
                                                file
                                                (logior O_RDWR O_CREAT)
                                                #o644))
            (ih    (indexer-index-blocks i c bi s s))
            (tuple (serialize-index-tuple/ascii ih i bi)))
       (let*-values (((ih2 i2 bf read)
                      (deserialize-index-tuple/ascii tuple))
                     ((out)
                      (stream->port (indexer-fetch-stream i2 ih2 bf s s))))
         (let ((r (and (object=? ih ih2)
                       ;; XXX: `tree_indexer' lacks an `equal' predicate
                       (object-is-a? i2 (object-class i))

                       (block-fetcher? bf)
                       (= read (string-length tuple))
                       (bytevector=? bv (get-bytevector-all out)))))
           (store-close s)
           r))))))

(test-end)


;;;
;;; Filters.
;;;

(test-begin "filters")

(test-assert "make-zlib-zip-filter"
  (let ((f (make-zlib-zip-filter)))
    (and (filter? f)
         (eq? (object-class f)
              (lookup-class "zlib_zip_filter")))))

(test-assert "make-zip-filter"
  (let* ((c (lookup-class "zlib_zip_filter"))
         (f (make-zip-filter c)))
    (and (filter? f)
         (eq? (object-class f) c))))

(test-assert "make-unzip-filter"
  (let* ((c (lookup-class "zlib_unzip_filter"))
         (f (make-unzip-filter c)))
    (and (filter? f)
         (eq? (object-class f) c))))

(test-assert "stacked filtered streams"
  (let* ((bv  (make-random-bytevector 5555))
         (in  (mem-stream-open bv))
         (zin (filtered-stream-open in (make-zlib-zip-filter)))
         (out (filtered-stream-open zin (make-zlib-unzip-filter))))
    (and (stream? in) (stream? zin) (stream? out)
         (let ((read (get-bytevector-all (stream->port out))))
           (bytevector=? read bv)))))

(test-assert "filtered store"
  (with-temporary-file
   (lambda (file)
     (let* ((s  (file-based-block-store-open (lookup-class "gdbm_block_store")
                                             file
                                             (logior O_RDWR O_CREAT)
                                             #o644))
            (fs (filtered-block-store-open (make-zlib-zip-filter)
                                           (make-zlib-unzip-filter)
                                           s #t))
            (k  #vu8(1 2 3 4 5))
            (v  (u8-list->bytevector (iota 256))))
       (store-write-block fs k v)
       (let ((r (bytevector=? v (store-read-block fs k))))
         (store-close fs)
         r)))))

(test-assert "stream->port error reported"
  (let* ((s (mem-stream-open #vu8(1 2 3 4)))
         (f (filtered-stream-open s (make-unzip-filter
                                     (lookup-class "zlib_unzip_filter")))))
    (catch 'chop-error
      (lambda ()
        (pk 'no-error (read (stream->port f)))
        #f)
      (lambda (key err . args)
        (not (= err error/stream-end))))))

(test-end)


(gc) ;; stress the GC

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
