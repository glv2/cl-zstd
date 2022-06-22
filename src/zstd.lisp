;;; This file is part of cl-zstd
;;; Copyright 2020-2022 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(in-package :zstd)


(deftype u8 () '(unsigned-byte 8))
(defconstant +buffer-size+ 4096)


;;;
;;; Errors
;;;

(define-condition zstd-error (simple-error)
  ())

(defmacro zstd-error (message &rest args)
  `(error 'zstd-error
          :format-control ,message
          :format-arguments (list ,@args)))

(defmacro zstd-check (form)
  (let ((code (gensym)))
    `(let ((,code ,form))
       (declare (type u64 ,code))
       (if (= (zstd-is-error ,code) 1)
           (zstd-error (zstd-get-error-name ,code))
           ,code))))


;;;
;;; Compression Gray streams
;;;

(defclass compressing-stream (fundamental-binary-output-stream)
  ((output-stream :accessor output-stream)
   (zstd-context :accessor zstd-context)
   (input-buffer :accessor input-buffer)
   (zstd-in-buffer :accessor zstd-in-buffer)
   (output-buffer :accessor output-buffer)
   (zstd-out-buffer :accessor zstd-out-buffer)))

(defmethod stream-element-type ((stream compressing-stream))
  '(unsigned-byte 8))

(defun compress-and-write (stream)
  (with-slots (output-stream zstd-context
               input-buffer zstd-in-buffer
               output-buffer zstd-out-buffer)
      stream
    (zstd-check (zstd-compress-stream2 zstd-context
                                       zstd-out-buffer
                                       zstd-in-buffer
                                       :zstd-e-continue))
    (cffi:with-foreign-slots ((size pos) zstd-in-buffer
                              (:struct zstd-in-buffer))
      (when (plusp pos)
        (replace input-buffer input-buffer :start2 pos :end2 size)
        (decf size pos)
        (setf pos 0)))
    (cffi:with-foreign-slots ((pos) zstd-out-buffer
                              (:struct zstd-out-buffer))
      (when (plusp pos)
        (write-sequence output-buffer output-stream :end pos)
        (setf pos 0)))))

(defmethod stream-write-byte ((stream compressing-stream) byte)
  (with-slots (input-buffer zstd-in-buffer) stream
    (cffi:with-foreign-slots ((size pos) zstd-in-buffer
                              (:struct zstd-in-buffer))
      (setf (aref input-buffer size) byte)
      (incf size)))
  (compress-and-write stream)
  byte)

(defmethod stream-write-sequence ((stream compressing-stream) seq start end
                                  &key &allow-other-keys)
  (with-slots (input-buffer zstd-in-buffer) stream
    (loop :while (< start end) :do
      (cffi:with-foreign-slots ((size) zstd-in-buffer
                                (:struct zstd-in-buffer))
        (let* ((available-space (- (length input-buffer) size))
               (n (min (- end start) available-space)))
          (replace input-buffer seq
                   :start1 size :start2 start :end2 (+ start n))
          (incf size n)
          (incf start n)))
      (compress-and-write stream)))
  seq)

(defmethod stream-finish-output ((stream compressing-stream))
  (with-slots (output-stream zstd-context zstd-in-buffer
               output-buffer zstd-out-buffer)
      stream
    (do ((frame-complete-p nil))
        (frame-complete-p)
      (setf frame-complete-p
            (zerop (zstd-check (zstd-compress-stream2 zstd-context
                                                      zstd-out-buffer
                                                      zstd-in-buffer
                                                      :zstd-e-end))))
      (cffi:with-foreign-slots ((pos) zstd-out-buffer
                                (:struct zstd-out-buffer))
        (when (plusp pos)
          (write-sequence output-buffer output-stream :end pos)
          (setf pos 0))))
    (cffi:with-foreign-slots ((size pos) zstd-in-buffer
                              (:struct zstd-in-buffer))
      (setf pos 0)
      (setf size 0))
    (cffi:with-foreign-slots ((pos) zstd-out-buffer
                              (:struct zstd-out-buffer))
      (setf pos 0))
    (finish-output output-stream))
  nil)

(defmethod close ((stream compressing-stream) &key &allow-other-keys)
  (when (open-stream-p stream)
    (finish-output stream)
    (with-slots (zstd-context input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer)
        stream
      (zstd-check (zstd-free-cctx zstd-context))
      (setf zstd-context nil)
      (setf input-buffer nil)
      (cffi:foreign-free zstd-in-buffer)
      (setf zstd-in-buffer nil)
      (setf output-buffer nil)
      (cffi:foreign-free zstd-out-buffer)
      (setf zstd-out-buffer nil)))
  t)

(defun initialize-context (context level)
  "Initialize the CONTEXT for the given compression LEVEL."
  (zstd-check (zstd-cctx-set-parameter context :zstd-c-compression-level level))
  (zstd-check (zstd-cctx-set-parameter context :zstd-c-checksum-flag 1))
  context)

(defun make-compressing-stream (output-stream &key (level 3))
  "Return a stream that will compress the bytes written to it at the given
compression LEVEL and write them to the OUTPUT-STREAM."
  (let ((stream (make-instance 'compressing-stream))
        (input-buffer-size (zstd-c-stream-in-size))
        (output-buffer-size (zstd-c-stream-out-size))
        (min-level (zstd-min-c-level))
        (max-level (zstd-max-c-level)))
    (setf (output-stream stream) output-stream)
    (with-slots (zstd-context input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer)
        stream
      (if (and (integerp level) (<= min-level level max-level))
          (let ((context (zstd-create-cctx)))
            (if (cffi:null-pointer-p context)
                (zstd-error "Failed to create compression context.")
                (setf zstd-context (initialize-context context level))))
          (zstd-error "LEVEL must be between ~d and ~d." min-level max-level))

      (setf input-buffer (cffi:make-shareable-byte-vector input-buffer-size))
      (setf zstd-in-buffer (cffi:foreign-alloc '(:struct zstd-in-buffer)))
      (cffi:with-pointer-to-vector-data (ffi-input-buffer input-buffer)
        (cffi:with-foreign-slots ((src size pos) zstd-in-buffer
                                  (:struct zstd-in-buffer))
          (setf src ffi-input-buffer)
          (setf size 0)
          (setf pos 0)))

      (setf output-buffer (cffi:make-shareable-byte-vector output-buffer-size))
      (setf zstd-out-buffer (cffi:foreign-alloc '(:struct zstd-out-buffer)))
      (cffi:with-pointer-to-vector-data (ffi-output-buffer output-buffer)
        (cffi:with-foreign-slots ((dst size pos) (zstd-out-buffer stream)
                                  (:struct zstd-out-buffer))
          (setf dst ffi-output-buffer)
          (setf size output-buffer-size)
          (setf pos 0))))
    stream))

(defmacro with-compressing-stream ((stream output-stream &key (level 3))
                                   &body body)
  "Within BODY, STREAM is bound to a compressing stream for the given
compression LEVEL and OUTPUT-STREAM. The result of the last form of BODY is
returned."
  `(with-open-stream (,stream (make-compressing-stream ,output-stream
                                                       :level ,level))
     ,@body))


;;;
;;; Decompression Gray streams
;;;

(defclass decompressing-stream (fundamental-binary-input-stream)
  ((input-stream :accessor input-stream)
   (zstd-context :accessor zstd-context)
   (input-buffer :accessor input-buffer)
   (zstd-in-buffer :accessor zstd-in-buffer)
   (output-buffer :accessor output-buffer)
   (zstd-out-buffer :accessor zstd-out-buffer)
   (frame-complete-p :accessor frame-complete-p)))

(defmethod stream-element-type ((stream decompressing-stream))
  '(unsigned-byte 8))

(defun read-and-decompress (stream)
  (with-slots (input-stream zstd-context input-buffer zstd-in-buffer
               zstd-out-buffer frame-complete-p)
      stream
    (let ((end-of-input-p nil))
      (cffi:with-foreign-slots ((size) zstd-in-buffer
                                (:struct zstd-in-buffer))
        (setf size (read-sequence input-buffer input-stream :start size))
        (setf end-of-input-p (zerop size)))
      (unless end-of-input-p
        (setf frame-complete-p
              (zerop (zstd-check (zstd-decompress-stream zstd-context
                                                         zstd-out-buffer
                                                         zstd-in-buffer))))
        (cffi:with-foreign-slots ((size pos) zstd-in-buffer
                                  (:struct zstd-in-buffer))
          (when (plusp pos)
            (replace input-buffer input-buffer :start2 pos :end2 size)
            (decf size pos)
            (setf pos 0))))
      end-of-input-p)))

(defmethod stream-listen ((stream decompressing-stream))
  (with-slots (input-stream zstd-in-buffer zstd-out-buffer) stream
    (or (plusp (cffi:foreign-slot-value zstd-out-buffer
                                        '(:struct zstd-out-buffer)
                                        'pos))
        (plusp (cffi:foreign-slot-value zstd-in-buffer
                                        '(:struct zstd-in-buffer)
                                        'size))
        (listen input-stream))))

(defmethod stream-read-byte ((stream decompressing-stream))
  (with-slots (output-buffer zstd-out-buffer) stream
    (let ((end-of-input-p (read-and-decompress stream)))
      (cffi:with-foreign-slots ((pos) zstd-out-buffer
                                (:struct zstd-out-buffer))
        (cond
          ((plusp pos)
           (let ((byte (aref output-buffer 0)))
             (replace output-buffer output-buffer :start2 1 :end2 pos)
             (decf pos)
             byte))
          ((and end-of-input-p (not (frame-complete-p stream)))
           (zstd-error "Truncated stream."))
          (t
           :eof))))))

(defmethod stream-read-sequence ((stream decompressing-stream) seq start end
                                 &key &allow-other-keys)
  (with-slots (output-buffer zstd-out-buffer) stream
    (let ((end-of-input-p nil))
      (loop :until (or (= start end) end-of-input-p) :do
        (setf end-of-input-p (read-and-decompress stream))
        (cffi:with-foreign-slots ((pos) zstd-out-buffer
                                  (:struct zstd-out-buffer))
          (loop :while (and (< start end) (plusp pos)) :do
            (let ((n (min (- end start) pos)))
              (replace seq output-buffer :start1 start :end2 n)
              (replace output-buffer output-buffer :start2 n :end2 pos)
              (decf pos n)
              (incf start n)))))
      (when (and end-of-input-p (not (frame-complete-p stream)))
        (zstd-error "Truncated stream."))))
  start)

(defmethod close ((stream decompressing-stream) &key &allow-other-keys)
  (when (open-stream-p stream)
    (with-slots (zstd-context input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer)
        stream
      (zstd-check (zstd-free-dctx zstd-context))
      (setf zstd-context nil)
      (setf input-buffer nil)
      (cffi:foreign-free zstd-in-buffer)
      (setf zstd-in-buffer nil)
      (setf output-buffer nil)
      (cffi:foreign-free zstd-out-buffer)
      (setf zstd-out-buffer nil)))
  t)

(defun make-decompressing-stream (input-stream)
  "Return a stream that will supply the bytes resulting from the decompression
of the data read from the INPUT-STREAM."
  (let ((stream (make-instance 'decompressing-stream))
        (input-buffer-size (zstd-d-stream-in-size))
        (output-buffer-size (zstd-d-stream-out-size)))
    (setf (input-stream stream) input-stream)
    (with-slots (zstd-context input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer frame-complete-p)
        stream
      (let ((context (zstd-create-dctx)))
        (if (cffi:null-pointer-p context)
            (zstd-error "Failed to create decompression context.")
            (setf zstd-context context)))

      (setf input-buffer (cffi:make-shareable-byte-vector input-buffer-size))
      (setf zstd-in-buffer (cffi:foreign-alloc '(:struct zstd-in-buffer)))
      (cffi:with-pointer-to-vector-data (ffi-input-buffer input-buffer)
        (cffi:with-foreign-slots ((src size pos) zstd-in-buffer
                                  (:struct zstd-in-buffer))
          (setf src ffi-input-buffer)
          (setf size 0)
          (setf pos 0)))

      (setf output-buffer (cffi:make-shareable-byte-vector output-buffer-size))
      (setf zstd-out-buffer (cffi:foreign-alloc '(:struct zstd-out-buffer)))
      (cffi:with-pointer-to-vector-data (ffi-output-buffer output-buffer)
        (cffi:with-foreign-slots ((dst size pos) zstd-out-buffer
                                  (:struct zstd-out-buffer))
          (setf dst ffi-output-buffer)
          (setf size output-buffer-size)
          (setf pos 0)))

      (setf frame-complete-p t))
    stream))

(defmacro with-decompressing-stream ((stream input-stream) &body body)
  "Within BODY, STREAM is bound to a decompressing stream for the given
INPUT-STREAM. The result of the last form of BODY is returned."
  `(with-open-stream (,stream (make-decompressing-stream ,input-stream))
     ,@body))


;;;
;;; Compression functions
;;;

(defun compress-stream (input output &key (level 3))
  "Read the data from the INPUT octet stream, compress it, and write the result
to the OUTPUT octet stream."
  (with-compressing-stream (stream output :level level)
    (let ((buffer (make-array +buffer-size+ :element-type 'u8)))
      (do ((n (read-sequence buffer input) (read-sequence buffer input)))
          ((zerop n) t)
        (write-sequence buffer stream :end n)))))

(defun compress-file (input output &key (level 3))
  "Read the data from the INPUT file, compress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type 'u8)
    (with-open-file (output-stream output :direction :output :element-type 'u8)
      (compress-stream input-stream output-stream :level level))))

(defun compress-buffer (buffer &key (start 0) end (level 3))
  "Read the data between the START and END offsets in the BUFFER, compress it,
and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (octet-streams:with-octet-output-stream (output)
      (with-compressing-stream (stream output :level level)
        (write-sequence buffer stream :start start :end end)))))


;;;
;;; Decompression functions
;;;

(defun decompress-stream (input output)
  "Read the data from the INPUT octet stream, decompress it, and write the
result to the OUTPUT octet stream."
  (with-decompressing-stream (stream input)
    (let ((buffer (make-array +buffer-size+ :element-type 'u8)))
      (do ((n (read-sequence buffer stream) (read-sequence buffer stream)))
          ((zerop n) t)
        (write-sequence buffer output :end n)))))

(defun decompress-file (input output)
  "Read the data from the INPUT file, decompress it, and write the result to
the OUTPUT file."
  (with-open-file (input-stream input :element-type 'u8)
    (with-open-file (output-stream output :direction :output :element-type 'u8)
      (decompress-stream input-stream output-stream))))

(defun decompress-buffer (buffer &key (start 0) end)
  "Read the data between the START and END offsets in the BUFFER, decompress
it, and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (octet-streams:with-octet-output-stream (output)
      (octet-streams:with-octet-input-stream (input buffer start end)
        (decompress-stream input output)))))
