;;; This file is part of cl-zstd
;;; Copyright 2020-2022 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defpackage :zstd-tests
  (:use :cl :cl-octet-streams :fiveam :zstd))

(in-package :zstd-tests)


(defun data-file-path (filename)
  (let ((path (concatenate 'string "tests/" filename)))
    (asdf:system-relative-pathname "zstd-tests" path)))

(defun load-data-file (filename)
  (with-open-file (file (data-file-path filename)
                        :element-type '(unsigned-byte 8))
    (let* ((size (file-length file))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buffer file)
      buffer)))

(defun same-files-p (path-1 path-2)
  (with-open-file (file-1 path-1 :element-type '(unsigned-byte 8))
    (with-open-file (file-2 path-2 :element-type '(unsigned-byte 8))
      (let ((buffer-1 (make-array 16384 :element-type '(unsigned-byte 8)))
            (buffer-2 (make-array 16384 :element-type '(unsigned-byte 8))))
        (loop for read-1 = (read-sequence buffer-1 file-1)
              for read-2 = (read-sequence buffer-2 file-2)
              never (or (/= read-1 read-2)
                        (mismatch buffer-1 buffer-2 :end1 read-1 :end2 read-1))
              until (zerop read-1))))))


(def-suite zstd-unit-tests
  :description "Unit tests for Zstandard (de)compression.")

(in-suite zstd-unit-tests)


(test decompressing-stream
  (with-octet-input-stream (input #(40 181 47 253 36 5 41 0
                                    0 1 2 3 4 5 47 214
                                    192 132))
    (let ((stream (make-decompressing-stream input))
          (tmp (make-array 2 :element-type '(unsigned-byte 8))))
      (is (= 1 (read-byte stream)))
      (is (= 2 (read-byte stream)))
      (is (= 2 (read-sequence tmp stream)))
      (is (equalp #(3 4) tmp))
      (is (= 5 (read-byte stream)))
      (is (eql :eof (read-byte stream nil :eof)))
      (close stream))))

(test decompress-stream
  (is (equalp #()
              (with-octet-output-stream (output)
                (with-octet-input-stream (input #(40 181 47 253 36 0 1 0
                                                  0 153 233 216 81))
                  (decompress-stream input output)))))
  (is (equalp #(1 2 3 4 5)
              (with-octet-output-stream (output)
                (with-octet-input-stream (input #(40 181 47 253 36 5 41 0
                                                  0 1 2 3 4 5 47 214
                                                  192 132))
                  (decompress-stream input output)))))
  (let ((tmp (with-octet-output-stream (output)
               (with-octet-input-stream (input #(40 181 47 253 164 160 134 1
                                                 0 85 0 0 16 115 115 1
                                                 0 155 134 57 192 2 13 41
                                                 68 46))
                 (decompress-stream input output)))))
    (is (= 100000 (length tmp)))
    (is-true (every (lambda (x) (= x 115)) tmp))))

(test decompress-file
  (let ((decompressed (data-file-path "test1.txt"))
        (compressed (data-file-path "test1.txt.zst"))
        (tmp "/tmp/zstd-test1.txt"))
    (unwind-protect
         (progn
           (is-true (decompress-file compressed tmp))
           (is (same-files-p decompressed tmp)))
      (uiop:delete-file-if-exists tmp)))
  (let ((decompressed (data-file-path "test2.txt"))
        (compressed (data-file-path "test2.txt.zst"))
        (tmp "/tmp/zstd-test2.txt"))
    (unwind-protect
         (progn
           (is-true (decompress-file compressed tmp))
           (is (same-files-p decompressed tmp)))
      (uiop:delete-file-if-exists tmp))))

(test decompress-buffer
  (let ((decompressed (load-data-file "test1.txt"))
        (compressed (load-data-file "test1.txt.zst")))
    (is (equalp decompressed (decompress-buffer compressed))))
  (is (equalp #(1 2 3 4 5) (decompress-buffer #(40 181 47 253 36 5 41 0
                                                0 1 2 3 4 5 47 214
                                                192 132)))))

(test decompress-corrupt-archive
  (signals zstd-error (decompress-buffer #(1 1 1 1)))
  (signals zstd-error (decompress-buffer #(40 181 47 253)))
  (signals zstd-error (decompress-buffer #(40 181 47 253 36 5 41 0
                                           0 1 2 3))))

(test compressing-stream
  (let ((compressed (with-octet-output-stream (output)
                      (let ((stream (make-compressing-stream output :level 9)))
                        (write-byte 1 stream)
                        (write-byte 2 stream)
                        (write-sequence #(3 4 1 2 3 4 1 2 3 4) stream)
                        (write-byte 5 stream)
                        (close stream)))))
    (is (equalp #(1 2 3 4 1 2 3 4 1 2 3 4 5)
                (decompress-buffer compressed)))))

(test compress-stream
  (is (equalp #()
              (with-octet-output-stream (output)
                (with-octet-pipe (pipe)
                  (with-octet-input-stream (input #())
                    (compress-stream input pipe)
                    (decompress-stream pipe output))))))
  (is (equalp #(1 2 3 4 5)
              (with-octet-output-stream (output)
                (with-octet-pipe (pipe)
                  (with-octet-input-stream (input #(1 2 3 4 5))
                    (compress-stream input pipe)
                    (decompress-stream pipe output))))))
  (let* ((data (make-array 100000
                           :element-type '(unsigned-byte 8)
                           :initial-element 115))
         (tmp (with-octet-output-stream (output)
                (with-octet-pipe (pipe)
                  (with-octet-input-stream (input data)
                    (compress-stream input pipe)
                    (decompress-stream pipe output))))))
    (is (equalp data tmp))))

(test compress-file
  (let ((decompressed (data-file-path "test1.txt"))
        (tmp-1 "/tmp/zstd-test1.txt.lz")
        (tmp-2 "/tmp/zstd-test1.txt"))
    (unwind-protect
         (progn
           (is-true (compress-file decompressed tmp-1))
           (is-true (decompress-file tmp-1 tmp-2))
           (is (same-files-p decompressed tmp-2)))
      (uiop:delete-file-if-exists tmp-1)
      (uiop:delete-file-if-exists tmp-2)))
  (let ((decompressed (data-file-path "test2.txt"))
        (tmp-1 "/tmp/zstd-test2.txt.lz")
        (tmp-2 "/tmp/zstd-test2.txt"))
    (unwind-protect
         (progn
           (is-true (compress-file decompressed tmp-1))
           (is-true (decompress-file tmp-1 tmp-2))
           (is (same-files-p decompressed tmp-2)))
      (uiop:delete-file-if-exists tmp-1)
      (uiop:delete-file-if-exists tmp-2))))

(test compress-buffer
  (let* ((decompressed (load-data-file "test1.txt"))
         (tmp-1 (compress-buffer decompressed))
         (tmp-2 (decompress-buffer tmp-1)))
    (is (< (length tmp-1) (length decompressed)))
    (is-false (mismatch decompressed tmp-2)))
  (let* ((decompressed (make-array 123456
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 55))
         (tmp-1 (compress-buffer decompressed))
         (tmp-2 (decompress-buffer tmp-1)))
    (is (< (length tmp-1) (length decompressed)))
    (is-false (mismatch decompressed tmp-2))))

(test compress-bad-options
  (signals zstd-error (compress-buffer #(7 6 6 7 6 6) :level nil))
  (signals zstd-error (compress-buffer #(7 6 6 7 6 6) :level 200)))
