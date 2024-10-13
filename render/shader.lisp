(defpackage #:slither/render/shader
  (:use #:cl)
  (:export
   :use-program))

(in-package #:slither/render/shader)

; TODO: Load source with a macro
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
    
    (if (shader-compiled-successfully-p shader-id)
          (setf (id shader) shader-id)
          (format t "~a:~%~a" (path shader) 
                  (gl:get-shader-info-log shader-id)))))
