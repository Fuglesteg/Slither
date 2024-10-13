(defpackage #:slither/render/texture
  (:use #:cl))

(in-package #:slither/render/texture)

(defclass texture ()
  ((id
    :accessor id
    :initarg :id)
   (image-path
    :accessor image-path
    :initarg :image-path)))

(defmethod initialize-instance :after ((texture texture) &key)
  (let ((id (gl:gen-texture)))
    (with-png-in-static-vector (png (image-path texture))
      (gl:bind-texture :texture-2d id)
      (gl:tex-image-2d :texture-2d 0 :rgb 
                       (width png) 
                       (height png) 
                       0 :rgb :unsigned-byte (data png))
     (setf (id texture) id))))

(defmethod bind-texture ((texture texture))
  (gl:bind-texture :texture-2d (id texture)))