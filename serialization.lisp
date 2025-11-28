(uiop:define-package :slither/serialization
  (:use :cl
        :org.shirakumo.fraf.math.vectors
        :ieee-floats
        :slither/utils)
  (:export
   :encode-argument
   :decode-arguments
   :decode-argument))

(in-package :slither/serialization)

(declaim (ftype (function ((or integer single-float vec2 vec3 vec4)) (vector (unsigned-byte 8)))
                encode-argument))
(defun encode-argument (argument)
  (etypecase argument
    (integer (concatenate '(vector (unsigned-byte 8))
                          #(0)
                          (integer->byte-array argument)))
    (single-float (concatenate '(vector (unsigned-byte 8))
                               #(1)
                               (integer->byte-array (encode-float32 argument))))
    (vec2 (concatenate '(vector (unsigned-byte 8))
                       #(2)
                       (integer->byte-array (encode-float32 (vx argument)))
                       (integer->byte-array (encode-float32 (vy argument)))))
    (vec3 (concatenate '(vector (unsigned-byte 8))
                       #(3)
                       (integer->byte-array (encode-float32 (vx argument)))
                       (integer->byte-array (encode-float32 (vy argument)))
                       (integer->byte-array (encode-float32 (vz argument)))))
    (vec4 (concatenate '(vector (unsigned-byte 8))
                       #(4)
                       (integer->byte-array (encode-float32 (vx argument)))
                       (integer->byte-array (encode-float32 (vy argument)))
                       (integer->byte-array (encode-float32 (vz argument)))
                       (integer->byte-array (encode-float32 (vw argument)))))))

(defun decode-arguments (arguments)
  (let (parsed)
    (loop until (= 0 (length arguments))
          do (multiple-value-bind (parsed-argument bytes-read)
                 (decode-argument arguments)
               (setf arguments (subseq arguments bytes-read))
               (push parsed-argument parsed)))
    (nreverse parsed)))

(declaim (ftype (function ((vector (unsigned-byte 8))) (values t integer))
                decode-argument))
(defun decode-argument (argument-vector)
  (declare (type (vector (unsigned-byte 8)) argument-vector))
  (let ((bytes-read 0))
    (flet ((read-integer (&key (bytes 1))
             (prog1 (vector-read-integer argument-vector :bytes bytes)
               (setf argument-vector (subseq argument-vector bytes))
               (incf bytes-read bytes))))
      (let ((parsed-argument
              (case (read-integer)
                (0 (read-integer :bytes 4))
                (1 (decode-float32 (read-integer :bytes 4)))
                (2 (vec2 (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))))
                (3 (vec3 (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))))
                (4 (vec4 (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4)))))))
        (values parsed-argument
                bytes-read)))))
