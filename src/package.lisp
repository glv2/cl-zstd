;;; This file is part of cl-zstd
;;; Copyright 2020-2022 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defpackage :zstd
  (:use :cl :trivial-gray-streams)
  (:export #:compress-stream
           #:compress-file
           #:compress-buffer
           #:decompress-stream
           #:decompress-file
           #:decompress-buffer
           #:make-compressing-stream
           #:make-decompressing-stream
           #:with-compressing-stream
           #:with-decompressing-stream
           #:zstd-error))
