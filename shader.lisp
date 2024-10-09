(defpackage #:slither/shaders
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors))

(in-package #:slither/shaders)

(defclass shader ()
  ((path
    :initarg :path
    :accessor path
    :type pathname)
   (id
    :initarg :id
    :type integer
    :initform 0
    :accessor id)
   (shader-type
    :initform nil
    :initarg :shader-type
    :accessor shader-type)))

(defclass vertex-shader (shader) ()
  (:default-initargs :shader-type :vertex-shader))

(defclass fragment-shader (shader) ()
  (:default-initargs :shader-type :fragment-shader))

(defun shader-compiled-successfully-p (shader-id)
  (let ((status (cffi:foreign-alloc :int)))
    (%gl:get-shader-iv shader-id :compile-status status)
    (let ((successfully-compiled (= 1 (cffi:mem-ref status :int))))
      (cffi:foreign-free status)
      successfully-compiled)))

(defmethod initialize-instance :after ((shader shader) &key)
  (let ((shader-id (gl:create-shader (shader-type shader))))
    (gl:shader-source shader-id (uiop:read-file-string (path shader)))
    (gl:compile-shader shader-id)
    (let ((shader-successfully-compiled (shader-compiled-successfully-p shader-id)))
      (if (not shader-successfully-compiled)
          (format t "~a:~%~a" (path shader) (gl:get-shader-info-log shader-id))
          (setf (id shader) shader-id)))))

(defun wrap-in-uniform (value uniform-location &key (type 'uniform-wrapper))
  (make-instance type
                 :value value
                 :id uniform-location))

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

(defclass shader-program ()
  ((id
    :initarg :id
    :accessor id
    :type integer)
   (vertex-shader
    :initarg :vertex-shader
    :accessor vertex-shader
    :type shader)
   (fragment-shader
    :initarg :fragment-shader
    :accessor fragment-shader
    :type shader)
   (uniforms
    :initarg :uniforms
    :accessor uniforms
    :initform '()
    :type list)))

(defmethod initialize-instance :after ((program shader-program) &key)
  (let ((program-id (gl:create-program)))
    (with-slots (vertex-shader fragment-shader) program
      (gl:attach-shader program-id (id vertex-shader))
      (gl:attach-shader program-id (id fragment-shader))
      (gl:link-program program-id)
      ;; Init uniforms
      (when (uniforms program)
        (mapcar
         (lambda (uniform)
           (setf (id uniform) (gl:get-uniform-location program-id (name uniform))))
         (uniforms program)))
      (setf (id program) program-id))))

(defmethod get-uniform ((program shader-program) (name string))
  (find-if (lambda (uniform) (string= name (name uniform))) (uniforms program)))