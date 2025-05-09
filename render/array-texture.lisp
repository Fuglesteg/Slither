(uiop:define-package #:slither/render/array-texture
  (:use :cl)
  (:import-from :slither/assets
                :asset-data)
  (:export #:array-texture
           #:array-texture-bind
           #:array-texture-unbind
           #:with-bound-array-texture))

(in-package :slither/render/array-texture)

(defclass array-texture ()
    ((id
      :accessor array-texture-id
      :initarg :id)
     (asset
      :accessor array-texture-asset
      :initarg :asset)))

(declaim (ftype (function (pngload:png fixnum fixnum) (vector (unsigned-byte 8))) ; TODO: Figure out how to return a simple-array
                png-generate-repacked-sprite-sheet-data))
(defun png-generate-repacked-sprite-sheet-data (png sprite-width sprite-height)
  (let* ((png-width (pngload:width png))
         (png-data (pngload:data png))
         (result (make-array (length png-data)
                             :element-type '(unsigned-byte 8)
                             :fill-pointer 0)))
    (loop for column from 0 below (* png-width 4) by (* sprite-width 4)
          do (loop for row from 0 below sprite-height
                   do (let ((sprite-location (+ column (* (* png-width 4) row))))
                        (loop for i from sprite-location below (+ sprite-location (* sprite-width 4))
                              do (vector-push (elt png-data i) result)))))
    result))

(defmethod initialize-instance :after ((array-texture array-texture)
                                       &key width height)
  (let* ((id (gl:gen-texture))
         (png (asset-data (array-texture-asset array-texture)))
         (amount (/ (pngload:width png) width))
         (repacked-data (png-generate-repacked-sprite-sheet-data png width height)))
    (gl:bind-texture :texture-2d-array id)
    (%gl:tex-storage-3d :texture-2d-array 1 :rgba8 width height amount)
    (gl:tex-sub-image-3d :texture-2d-array
                         0 0 0 0
                         width height amount
                         :rgba :unsigned-byte repacked-data)
    (gl:tex-parameter :texture-2d-array :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d-array :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d-array :texture-max-level 4)
    (gl:tex-parameter :texture-2d-array :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d-array :texture-wrap-t :clamp-to-edge)
    (gl:bind-texture :texture-2d-array 0)
    (setf (array-texture-id array-texture) id)))

(defmethod array-texture-bind ((array-texture array-texture))
  (gl:bind-texture :texture-2d-array (array-texture-id array-texture)))

(defun array-texture-unbind ()
  (gl:bind-texture :texture-2d-array 0))

(defmacro with-bound-array-texture (array-texture &body body)
  `(progn
     (array-texture-bind ,array-texture)
     ,@body
     (array-texture-unbind)))
  