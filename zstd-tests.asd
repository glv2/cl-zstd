;;; This file is part of cl-zstd
;;; Copyright 2020 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defsystem "zstd-tests"
  :name "zstd-tests"
  :description "Tests for zstd"
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("cl-octet-streams" "fiveam" "uiop" "zstd")
  :in-order-to ((test-op (load-op "zstd-tests")))
  :perform (test-op (op s)
             (let ((tests (uiop:find-symbol* 'zstd-unit-tests :zstd-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:module "tests"
                :serial t
                :components ((:static-file "test1.txt")
                             (:static-file "test1.txt.zst")
                             (:static-file "test2.txt")
                             (:static-file "test2.txt.zst")
                             (:file "tests")))))
