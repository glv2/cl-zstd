;;; This file is part of cl-zstd
;;; Copyright 2020 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defsystem "zstd"
  :name "zstd"
  :description "Zstandard (de)compression using bindings to libzstd"
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("cffi" "cl-octet-streams")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "libzstd")
                             (:file "zstd")))))
