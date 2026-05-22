(uiop:define-package :slither/serialization
  (:use :cl
        :org.shirakumo.fraf.math.vectors
        :ieee-floats
        :slither/utils)
  (:export
   :encode-argument
   :decode-arguments
   :decode-argument
   :encode-arguments
   :vector-read-integer
   :integer->byte-array
   :with-vector-reader
   :with-vector-writer))

(in-package :slither/serialization)

(eval-always
  (defmacro with-vector-reader (vector reader-form &body body)
    (let ((index-symbol (gensym "INDEX")))
      (destructuring-bind (&key read-integer read-sequence read-argument remaining-bytes bytes-read) reader-form
        `(let ((,index-symbol 0))
           (flet (,@(when read-integer
                      `((,read-integer (bytes)
                                       (prog1 (vector-read-integer (subseq ,vector ,index-symbol) :bytes bytes)
                                         (incf ,index-symbol bytes)))))
                  ,@(when read-sequence
                      `((,read-sequence (bytes)
                                        (prog1 (subseq ,vector ,index-symbol (+ ,index-symbol bytes))
                                          (incf ,index-symbol bytes)))))
                  ,@(when read-argument
                      `((,read-argument ()
                                        (multiple-value-bind (argument bytes-read)
                                            (decode-argument (subseq ,vector ,index-symbol))
                                          (incf ,index-symbol bytes-read)
                                          (values argument
                                                  bytes-read)))))
                  ,@(when remaining-bytes
                      `((,remaining-bytes ()
                                          (- (length ,vector) ,index-symbol))))
                  ,@(when bytes-read
                      `((,bytes-read ()
                                     ,index-symbol))))
             ,@body)))))

  (defmacro with-vector-writer (vector writer-form &body body)
    (let ((index-symbol (gensym "INDEX"))
          (vector-symbol (gensym "VECTOR")))
      (destructuring-bind (&key write-integer write-sequence) writer-form
        `(let ((,index-symbol 0)
               (,vector-symbol ,vector))
           (flet (,@(when write-integer
                      `((,write-integer (integer &key bytes)
                                        (loop for byte across (integer->byte-array integer :bytes bytes)
                                              do (setf (aref ,vector-symbol ,index-symbol) byte)
                                                 (incf ,index-symbol)))))
                  ,@(when write-sequence
                      `((,write-sequence (bytes)
                                         (replace ,vector-symbol bytes :start1 ,index-symbol)
                                         (incf ,index-symbol (length bytes))))))
             ,@body
             ,vector-symbol))))))

(defun encode-arguments (arguments)
  (apply #'concatenate '(vector (unsigned-byte 8))
         (mapcar #'encode-argument arguments)))

(-> encode-argument ((or integer single-float double-float
                         vec2 vec3 vec4 string))
    (vector (unsigned-byte 8)))
(defun encode-argument (argument)
  (etypecase argument
    (integer (concatenate '(vector (unsigned-byte 8))
                          #(0)
                          (integer->byte-array argument)))
    (single-float (concatenate '(vector (unsigned-byte 8))
                               #(1)
                               (integer->byte-array (encode-float32 argument))))
    (double-float (concatenate '(vector (unsigned-byte 8))
                               #(2)
                               (integer->byte-array (encode-float64 argument) :bytes 8)))
    (vec2 (concatenate '(vector (unsigned-byte 8))
                       #(3)
                       (integer->byte-array (encode-float32 (vx argument)))
                       (integer->byte-array (encode-float32 (vy argument)))))
    (vec3 (concatenate '(vector (unsigned-byte 8))
                       #(4)
                       (integer->byte-array (encode-float32 (vx argument)))
                       (integer->byte-array (encode-float32 (vy argument)))
                       (integer->byte-array (encode-float32 (vz argument)))))
    (vec4 (concatenate '(vector (unsigned-byte 8))
                       #(5)
                       (integer->byte-array (encode-float32 (vx argument)))
                       (integer->byte-array (encode-float32 (vy argument)))
                       (integer->byte-array (encode-float32 (vz argument)))
                       (integer->byte-array (encode-float32 (vw argument)))))
    (string (with-vector-writer (make-octet-vector (+ 3 (length argument))) (:write-integer argument-write-byte)
              (argument-write-byte 6 :bytes 1)
              (argument-write-byte (length argument) :bytes 2)
              (loop for char across argument
                    do (argument-write-byte (char-code char) :bytes 1))))))

(defun decode-arguments (arguments)
  (let (parsed)
    (loop until (= 0 (length arguments))
          do (multiple-value-bind (parsed-argument bytes-read)
                 (decode-argument arguments)
               (setf arguments (subseq arguments bytes-read))
               (push parsed-argument parsed)))
    (nreverse parsed)))

(-> decode-argument ((vector (unsigned-byte 8)))
    (values t integer))
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
                (2 (decode-float64 (read-integer :bytes 8)))
                (3 (vec2 (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))))
                (4 (vec3 (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))))
                (5 (vec4 (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))
                         (decode-float32 (read-integer :bytes 4))))
                (6 (let ((string-length (read-integer :bytes 2)))
                     (incf bytes-read string-length)
                     (map 'string #'code-char (subseq argument-vector 0 string-length)))))))
        (values parsed-argument
                bytes-read)))))

(-> vector-read-integer ((vector (unsigned-byte 8)) &key (:bytes integer))
    integer)
(defun vector-read-integer (vector &key (bytes 1))
  "Read the amount of bytes from vector as one integer"
  (apply #'logior (loop for byte from 0 below bytes
                        collect (ash (aref vector byte) (* byte 8)))))

(-> integer->byte-array (integer &key (:bytes integer))
    (vector (unsigned-byte 8)))
(defun integer->byte-array (integer &key (bytes 4))
  (make-array bytes
              :element-type '(unsigned-byte 8)
              :initial-contents (loop for byte from 0 below bytes
                                      collect (ldb (byte 8 (* byte 8)) integer))))
