(defpackage #:slither/render/uniform
  (:use :cl
        :org.shirakumo.fraf.math.vectors
        :org.shirakumo.fraf.math.matrices)
  (:export #:uniform-location
           #:uniform-symbol
           #:value
           #:uniform
           #:uniform-wrapper
           #:uniform-value))

(in-package #:slither/render/uniform)

(defclass uniform () 
  ((uniform-location
    :type integer
    :accessor uniform-location
    :initform 0
    :initarg :location)
   (uniform-symbol
    :type symbol
    :accessor uniform-symbol
    :initarg :symbol)))

(defmethod (setf uniform-value) (value (uniform uniform))
  (setf (uniform-value (uniform-location uniform)) value))

(defmethod (setf uniform-value) ((value float) (uniform-location integer))
  (%gl:uniform-1f uniform-location value))

(defmethod (setf uniform-value) ((vec vec2) (uniform-location integer))
  (with-vec (x y) vec
    (%gl:uniform-2f uniform-location x y)))

(defmethod (setf uniform-value) ((vec vec3) (uniform-location integer))
  (with-vec (x y z) vec
    (%gl:uniform-3f uniform-location x y z)))

(defmethod (setf uniform-value) ((vec vec4) (uniform-location integer))
  (with-vec (x y z a) vec
    (%gl:uniform-4f uniform-location x y z a)))

(defmethod (setf uniform-value) ((mat mat3) (uniform-location integer))
  (gl:uniform-matrix-3fv uniform-location (marr3 mat)))

(defmethod (setf uniform-value) ((value integer) (uniform-location integer))
  (%gl:uniform-1i uniform-location value))

(defclass uniform-wrapper (uniform)
  ((value
    :reader value
    :initarg :value)))

(defgeneric uniform-sync (uniform-wrapper))
(defmethod uniform-sync ((uniform-wrapper uniform-wrapper))
  (setf (uniform-value uniform-wrapper) (value uniform-wrapper)))

(defmethod initialize-instance :after ((uniform-wrapper uniform-wrapper) &key)
  (uniform-sync uniform-wrapper))

(defmethod (setf value) (value (uniform-wrapper uniform-wrapper))
  (setf (slot-value uniform-wrapper 'value) value)
  (call-next-method))
                      
(defmethod (setf value) :after (value (uniform-wrapper uniform-wrapper))
  (uniform-sync uniform-wrapper))
