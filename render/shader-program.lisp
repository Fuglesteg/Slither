(defpackage #:slither/render/shader-program
  (:use #:cl))

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

(defmethod use-program ((program shader-program))
  (gl:use-program (id program)))

(defmethod get-uniform ((program shader-program) (name string))
  (find name (uniforms program) :key #'name :test #'string=))