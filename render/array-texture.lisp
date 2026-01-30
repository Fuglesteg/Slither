(uiop:define-package #:slither/render/array-texture
  (:use :cl
        :slither/utils)
  (:import-from :slither/assets
                :asset-data)
  (:export #:array-texture
           #:array-texture-asset
           #:array-texture-bind
           #:array-texture-unbind
           #:with-bound-array-texture))

(in-package :slither/render/array-texture)

(defclass array-texture ()
    ((id
      :accessor array-texture-id
      :initarg :id)
     (asset
      :reader array-texture-asset
      :initform nil
      :initarg :asset)
     (sprite-width
      :accessor array-texture-sprite-width
      :initarg :sprite-width)
     (sprite-height
      :accessor array-texture-sprite-height
      :initarg :sprite-height)))

(defun (setf array-texture-asset) (new-asset array-texture)
  (setf (slot-value array-texture 'asset) new-asset)
  (array-texture-load-asset array-texture))

(defun png-generate-repacked-sprite-sheet-data (png sprite-width sprite-height)
  (let* ((png-width (* (pngload:width png) 4))
         (png-height (pngload:height png))
         (sprite-width (* sprite-width 4))
         (sprite-height sprite-height)
         (png-data (pngload:data png))
         (result (static-vectors:make-static-vector (length png-data)
                                                    :initial-element 0
                                                    :element-type 'octet))
         (output-pos 0))
    (loop for row from 0 below png-height by sprite-height
          do (loop for column from 0 below png-width by sprite-width
                   do (let ((sprite-location (+ (* row png-width) column)))
                        (loop for i from 0 below (* sprite-width sprite-height)
                              do (setf (elt result (+ i output-pos))
                                       (elt png-data
                                            (+ sprite-location
                                               (mod i sprite-width)
                                               (* png-width
                                                  (floor (/ i sprite-width)))))))
                        (incf output-pos (* sprite-width sprite-height)))))
    result))

(defmethod array-texture-load-asset ((array-texture array-texture))
  (let* ((id (gl:gen-texture))
         (png (asset-data (array-texture-asset array-texture)))
         (width (array-texture-sprite-width array-texture))
         (height (array-texture-sprite-height array-texture))
         (amount (* (/ (pngload:height png) height)
                    (/ (pngload:width png) width)))
         (repacked-data (png-generate-repacked-sprite-sheet-data png width height)))
    (gl:bind-texture :texture-2d-array id)
    (%gl:tex-storage-3d :texture-2d-array 1 :rgba8 width height amount)
    (gl:tex-sub-image-3d :texture-2d-array
                         0 0 0 0
                         width height amount
                         :rgba :unsigned-byte (static-vectors:static-vector-pointer repacked-data))
    (gl:tex-parameter :texture-2d-array :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d-array :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d-array :texture-max-level 0)
    (gl:tex-parameter :texture-2d-array :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d-array :texture-wrap-t :clamp-to-edge)
    (gl:bind-texture :texture-2d-array 0)
    (static-vectors:free-static-vector repacked-data)
    (setf (array-texture-id array-texture) id)))

(defmethod initialize-instance :after ((array-texture array-texture) &key &allow-other-keys)
  (when (array-texture-asset array-texture)
    (array-texture-load-asset array-texture)))

(defmethod array-texture-bind ((array-texture array-texture))
  (gl:bind-texture :texture-2d-array (array-texture-id array-texture)))

(defun array-texture-unbind ()
  (gl:bind-texture :texture-2d-array 0))

(defmacro with-bound-array-texture (array-texture &body body)
  `(progn
     (array-texture-bind ,array-texture)
     ,@body
     (array-texture-unbind)))
