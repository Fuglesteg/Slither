(uiop:define-package #:slither/render/texture
  (:use :cl)
  (:import-from :slither/assets
                :asset-data)
  (:export #:texture
           #:texture-bind
           #:texture-unbind
           #:texture-asset
           #:with-bound-texture))

(in-package #:slither/render/texture)

(defclass texture ()
  ((id
    :accessor texture-id
    :initarg :id)
   (asset
    :reader texture-asset
    :initform nil
    :initarg :asset)))

(defun (setf texture-asset) (new-asset texture)
  (setf (slot-value texture 'asset) new-asset)
  (texture-load-asset texture))

(defmethod texture-load-asset ((texture texture))
  (let ((id (gl:gen-texture))
        (png (asset-data (texture-asset texture))))
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest-mipmap-nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (static-vectors:with-static-vector (image-data (length (pngload:data png))
                                                   :initial-contents (pngload:data png))
      (gl:tex-image-2d :texture-2d 0 :rgba
                       (pngload:width png) 
                       (pngload:height png) 
                       0 :rgba :unsigned-byte image-data))
    (gl:generate-mipmap :texture-2d)
    (gl:bind-texture :texture-2d 0)
    (setf (texture-id texture) id)))
  
(defmethod initialize-instance :after ((texture texture) &key)
  (when (texture-asset texture)
    (texture-load-asset texture)))

(defmethod texture-bind ((texture texture))
  (gl:bind-texture :texture-2d (texture-id texture)))

(defun texture-unbind ()
  (gl:bind-texture :texture-2d 0))

(defmacro with-bound-texture (texture &body body)
  `(progn
     (texture-bind ,texture)
     ,@body
     (texture-unbind)))