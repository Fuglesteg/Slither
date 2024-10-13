(defpackage #:slither/render/uniform
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors))

(in-package #:slither/render/uniform)

(defclass uniform () 
  ((id
    :type integer
    :accessor id
    :initform 0
    :initarg :id)
   (name
    :type string
    :accessor name
    :initarg :name)))

(defmethod (setf uniform-value) ((value float) (uniform uniform))
  (%gl:uniform-1f (id uniform) value))

(defmethod (setf uniform-value) ((vec vec2) (uniform uniform))
  (with-vec (x y) vec
    (%gl:uniform-2f (id uniform) x y)))

(defmethod (setf uniform-value) ((vec vec3) (uniform uniform))
  (with-vec (x y z) vec
    (%gl:uniform-3f (id uniform) x y z)))

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
  (setf (slot-value uniform-wrapper 'value) value))

(defmethod (setf value) :after (value (uniform-wrapper uniform-wrapper))
  (uniform-sync uniform-wrapper))

