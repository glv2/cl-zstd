;;; This file is part of cl-zstd
;;; Copyright 2020 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :zstd)


(cffi:define-foreign-library libzstd
  (:darwin "libzstd.dylib")
  (:unix (:or "libzstd.so"
              "libzstd.so.1"))
  (t (:default "libzstd")))

(cffi:use-foreign-library libzstd)


;;;
;;; Types
;;;

(deftype i32 () '(signed-byte 32))
(deftype u32 () '(unsigned-byte 32))
(deftype u64 () '(unsigned-byte 64))

(cffi:defcstruct zstd-in-buffer
  (src :pointer)
  (size :unsigned-long-long)
  (pos :unsigned-long-long))

(cffi:defcstruct zstd-out-buffer
  (dst :pointer)
  (size :unsigned-long-long)
  (pos :unsigned-long-long))

(cffi:defcenum zstd-c-parameter
  (:zstd-c-compression-level 100)
  (:zstd-c-checksum-flag 201)
  (:zstd-c-nb-workers 400))

(cffi:defcenum zstd-end-directive
  :zstd-e-continue
  :zstd-e-flush
  :zstd-e-end)


;;;
;;; Error functions
;;;

(declaim (ftype (function (u64) u32) zstd-is-error))
(cffi:defcfun ("ZSTD_isError" zstd-is-error) :unsigned-int
  "Tell if a function result is an error code."
  (code :unsigned-long-long))

(declaim (ftype (function (u64) t) zstd-get-error-name))
(cffi:defcfun ("ZSTD_getErrorName" zstd-get-error-name) :string
  "Return a readable string from an error code."
  (code :unsigned-long-long))


;;;
;;; Compression functions
;;;

(declaim (ftype (function () t) zstd-create-cctx))
(cffi:defcfun ("ZSTD_createCCtx" zstd-create-cctx) :pointer
  "Create a compression context.")

(declaim (ftype (function (t) u64) zstd-free-cctx))
(cffi:defcfun ("ZSTD_freeCCtx" zstd-free-cctx) :unsigned-long-long
  "Free a compression context."
  (cctx :pointer))

(declaim (ftype (function (t t i32) u64) zstd-cctx-set-parameter))
(cffi:defcfun ("ZSTD_CCtx_setParameter" zstd-cctx-set-parameter) :unsigned-long-long
  "Set one compression parameter."
  (cctx :pointer)
  (param zstd-c-parameter)
  (value :int))

(declaim (ftype (function () i32) zstd-min-c-level))
(cffi:defcfun ("ZSTD_minCLevel" zstd-min-c-level) :int
  "Minimum compression level available.")

(declaim (ftype (function () i32) zstd-max-c-level))
(cffi:defcfun ("ZSTD_maxCLevel" zstd-max-c-level) :int
  "Maximum compression level available.")

(declaim (ftype (function (t t t t) u64) zstd-compress-stream2))
(cffi:defcfun ("ZSTD_compressStream2" zstd-compress-stream2) :unsigned-long-long
  "Compress the data with additional control on end directive."
  (cctx :pointer)
  (output :pointer)
  (input :pointer)
  (endop zstd-end-directive))

(declaim (ftype (function () u64) zstd-c-stream-in-size))
(cffi:defcfun ("ZSTD_CStreamInSize" zstd-c-stream-in-size) :unsigned-long-long
  "Recommended size for compression input buffer.")

(declaim (ftype (function () u64) zstd-c-stream-out-size))
(cffi:defcfun ("ZSTD_CStreamOutSize" zstd-c-stream-out-size) :unsigned-long-long
  "Recommended size for compression output buffer.")


;;;
;;; Decompression functions
;;;

(declaim (ftype (function () t) zstd-create-dctx))
(cffi:defcfun ("ZSTD_createDCtx" zstd-create-dctx) :pointer
  "Create a decompression context.")

(declaim (ftype (function (t) u64) zstd-free-dctx))
(cffi:defcfun ("ZSTD_freeDCtx" zstd-free-dctx) :unsigned-long-long
  "Free a decompression context."
  (dctx :pointer))

(declaim (ftype (function (t t t) u64) zstd-decompress-stream))
(cffi:defcfun ("ZSTD_decompressStream" zstd-decompress-stream) :unsigned-long-long
  "Decompress the data."
  (zds :pointer)
  (output :pointer)
  (input :pointer))

(declaim (ftype (function () u64) zstd-d-stream-in-size))
(cffi:defcfun ("ZSTD_DStreamInSize" zstd-d-stream-in-size) :unsigned-long-long
  "Recommended size for decompression input buffer.")

(declaim (ftype (function () u64) zstd-d-stream-out-size))
(cffi:defcfun ("ZSTD_DStreamOutSize" zstd-d-stream-out-size) :unsigned-long-long
  "Recommended size for decompression output buffer.")
