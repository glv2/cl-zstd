;;; This file is part of cl-zstd
;;; Copyright 2020 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :zstd)


(cffi:define-foreign-library libzstd
  (:unix (:or "libzstd.so"
              "libzstd.so.1"))
  (t (:default "libzstd")))

(cffi:use-foreign-library libzstd)


;;;
;;; Types
;;;

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

(cffi:defcfun ("ZSTD_isError" zstd-is-error) :unsigned-int
  "Tell if a function result is an error code."
  (code :unsigned-long-long))

(cffi:defcfun ("ZSTD_getErrorName" zstd-get-error-name) :string
  "Return a readable string from an error code."
  (code :unsigned-long-long))


;;;
;;; Compression functions
;;;

(cffi:defcfun ("ZSTD_createCCtx" zstd-create-cctx) :pointer
  "Create a compression context.")

(cffi:defcfun ("ZSTD_freeCCtx" zstd-free-cctx) :unsigned-long-long
  "Free a compression context."
  (cctx :pointer))

(cffi:defcfun ("ZSTD_CCtx_setParameter" zstd-cctx-set-parameter) :unsigned-long-long
  "Set one compression parameter."
  (cctx :pointer)
  (param zstd-c-parameter)
  (value :int))

(cffi:defcfun ("ZSTD_minCLevel" zstd-min-c-level) :int
  "Minimum compression level available.")

(cffi:defcfun ("ZSTD_maxCLevel" zstd-max-c-level) :int
  "Maximum compression level available.")

(cffi:defcfun ("ZSTD_compressStream2" zstd-compress-stream2) :unsigned-long-long
  "Compress the data with additional control on end directive."
  (cctx :pointer)
  (output :pointer)
  (input :pointer)
  (endop zstd-end-directive))

(cffi:defcfun ("ZSTD_CStreamInSize" zstd-c-stream-in-size) :unsigned-long-long
  "Recommended size for compression input buffer.")

(cffi:defcfun ("ZSTD_CStreamOutSize" zstd-c-stream-out-size) :unsigned-long-long
  "Recommended size for compression output buffer.")


;;;
;;; Decompression functions
;;;

(cffi:defcfun ("ZSTD_createDCtx" zstd-create-dctx) :pointer
  "Create a decompression context.")

(cffi:defcfun ("ZSTD_freeDCtx" zstd-free-dctx) :unsigned-long-long
  "Free a decompression context."
  (dctx :pointer))

(cffi:defcfun ("ZSTD_decompressStream" zstd-decompree-stream) :unsigned-long-long
  "Decompress the data."
  (zds :pointer)
  (output :pointer)
  (input :pointer))

(cffi:defcfun ("ZSTD_DStreamInSize" zstd-d-stream-in-size) :unsigned-long-long
  "Recommended size for decompression input buffer.")

(cffi:defcfun ("ZSTD_DStreamOutSize" zstd-d-stream-out-size) :unsigned-long-long
  "Recommended size for decompression output buffer.")
