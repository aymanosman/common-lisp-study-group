(in-package :nibbles)

(defun sb24ref/le (vector index)
  (let ((u24 (ub24ref/le vector index)))
    (if (> u24 (- (expt 2 23) 1))
        (- u24 (expt 2 24))
        u24)))

(defun ub24ref/le (vector index)
  (let ((u24 0))
    (setf (ldb (byte 8 0) u24) (aref vector (+ index 0)))
    (setf (ldb (byte 8 8) u24) (aref vector (+ index 1)))
    (setf (ldb (byte 8 16) u24) (aref vector (+ index 2)))
    u24))

(export '(sb24ref/le ub24ref/le))

(in-package :cl-user)

(defpackage :wav
  (:use :common-lisp)
  (:import-from #:nibbles
                #:read-ub16/le
                #:read-ub32/le)
  (:export #:read-wav
           #:make-wav))

(in-package :wav)

;; WRITE WAV

(defun wav-write-vector (vector stream)
  (loop :for elem :across vector
        :do (vector-push-extend elem stream)))

(defun wav-write-string (string stream)
  (wav-write-vector (map 'vector #'char-code string) stream))

(defun write-u16 (n stream)
  (vector-push (ldb (byte 8 0) n) stream)
  (vector-push (ldb (byte 8 8) n) stream))

(defun write-u32 (n stream)
  (vector-push (ldb (byte 8 0) n) stream)
  (vector-push (ldb (byte 8 8) n) stream)
  (vector-push (ldb (byte 8 16) n) stream)
  (vector-push (ldb (byte 8 24) n) stream))

(defun make-wav (&key (compression-code 1)
                   (channels 1)
                   (sample-rate 44100)
                   (bits-per-sample 16)
                   (block-align (* (floor (/ bits-per-sample 8)) channels))
                   (average-bytes-per-second (* sample-rate block-align))
                   data)
  (let* ((file-size (+ 8 ;; RIFF + size
                       4 ;; WAVE
                       8 ;; 'fmt ' + size
                       16 ;; size of fmt
                       8 ;; 'data' + size
                       (length data)))
         (wav-file (make-array file-size
                               :adjustable nil ;; TODO this is not needed if file size is calculated correctly
                               :fill-pointer 0
                               :element-type '(unsigned-byte 8))))
    (labels ((write-wav-format (stream)
               (write-u16 compression-code stream) ;; PCM compression
               (write-u16 channels stream) ;; channels
               (write-u32 sample-rate stream) ;; sample rate
               (write-u32 average-bytes-per-second stream) ;; average bytes per second
               (write-u16 block-align stream) ;; block align
               (write-u16 bits-per-sample stream))) ;; bits per sample
      (wav-write-string "RIFF" wav-file)
      (write-u32 (- file-size 8) wav-file)
      (wav-write-string "WAVE" wav-file)
      (wav-write-string "fmt " wav-file)
      (write-u32 16 wav-file)
      (write-wav-format wav-file)
      (wav-write-string "data" wav-file)
      (write-u32 (length data) wav-file)
      (wav-write-vector data wav-file))

    wav-file))

;; READ WAV

(defvar *wav-read-buffers* nil)

(defun make-wav-read-buffer (size)
  (let ((buf (make-array size :element-type '(unsigned-byte 8))))
    (sb-ext:atomic-push buf *wav-read-buffers*)
    buf))

(defun read-bytes (n stream)
  (assert (<= n 128))
  (let ((buf (or (sb-ext:atomic-pop *wav-read-buffers*)
                 (make-wav-read-buffer 128))))
    (read-sequence buf stream :start 0 :end n)
    (prog1 (subseq buf 0 n)
      (sb-ext:atomic-push buf *wav-read-buffers*))))

(defun read-wav (stream)
  (labels ((coerce-to-string (vector)
             (coerce (map 'vector #'code-char vector) 'string))
           (read-string (expected-string)
             (let ((expected (map 'vector #'char-code expected-string))
                   (given (read-bytes 4 stream)))
               (unless (equalp expected given)
                 (error "expected '~a' got ~a" expected-string (coerce-to-string given)))))
           (read-header (expected-id &optional expected-size)
             "Returns chunk size"
             (let ((id (map 'vector #'char-code expected-id))
                   (chunk-id (read-bytes 4 stream)))
               (unless (equalp id chunk-id)
                 (error "expected '~a' got ~a" expected-id (coerce-to-string chunk-id)))
               (let ((size (read-ub32/le stream)))
                 (when expected-size
                   (unless (= size expected-size)
                     (error "expected chunk size of ~a got ~a" expected-size size)))
                 size)))
           (read-compression-code ()
             (let ((code (read-ub16/le stream)))
               (unless (= 1 code)
                 (error "only compression code 1 is supported, got ~a" code))
               code))
           (read-channels ()
             (let ((channels (read-ub16/le stream)))
               (unless (member channels '(1 2))
                 (error "only 1 or 2 channels allowed, got ~a" channels))
               channels))
           (read-format ()
             (let (compression-code channels sample-rate average-bytes-per-second block-align bits-per-sample)
               (read-header "fmt " 16)
               (setf compression-code (read-compression-code))
               (setf channels (read-channels))
               (setf sample-rate (read-ub32/le stream))
               (setf average-bytes-per-second (read-ub32/le stream))
               (setf block-align (read-ub16/le stream))
               (setf bits-per-sample (read-ub16/le stream))
               (list :compression-code compression-code
                     :channels channels
                     :sample-rate sample-rate
                     :average-bytes-per-second average-bytes-per-second
                     :block-align block-align
                     :bits-per-sample bits-per-sample)))
           (read-data ()
             (let (size buf)
               (setf size (read-header "data"))
               (setf buf (make-array size :element-type '(unsigned-byte 8)))
               (read-sequence buf stream :end (- size 1))
               buf)))
    (read-header "RIFF")
    (read-string "WAVE")
    (values (read-format)
            (read-data))))
