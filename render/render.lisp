(uiop:define-package #:slither/render
  (:use :cl
        :org.shirakumo.fraf.math.matrices
        :org.shirakumo.fraf.math.vectors)
  (:import-from :slither/render/uniform
                #:uniform-value
                #:uniform-location
                #:uniform)
  (:import-from :slither/render/texture
                #:texture
                #:texture-id
                #:with-bound-texture)
  (:import-from :slither/render/array-texture
                #:array-texture
                #:with-bound-array-texture)
  (:import-from :slither/render/shader-program
                #:shader-program
                #:shader-program-id
                #:program-bind
                #:program-render
                #:get-uniform
                #:make-shader-program
                #:with-bound-shader-program)
  (:import-from :slither/render/shader
                #:define-vertex-shader
                #:define-fragment-shader
                #:vertex-shader
                #:fragment-shader
                #:shader)
  (:import-from :slither/render/vertex
                #:make-quad-vertex-array-object
                #:make-texture-vertex-array-object
                #:with-bound-vertex-array)
  (:import-from :slither/utils
                #:defmemo)
  (:import-from :slither/assets
                #:defasset)
  (:export #:set-camera-position
           #:screen-space-position
           #:screen-space-scale
           #:renderer-init
           #:renderer-flush
           #:draw-rectangle
           #:draw-static
           #:draw-texture
           #:draw-array-texture
           #:defshader
           #:define-vertex-shader
           #:define-fragment-shader
           #:define-shader-program
           #:define-texture
           #:define-array-texture))

(in-package #:slither/render)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *eval-on-init* nil)
  (defvar *initialized* nil)

  (defun eval-on-init ()
    (loop for function in *eval-on-init*
          do (funcall function))
    (setf *initialized* t))

  (defmacro delay-evaluation (&body body)
    (if *initialized*
        `(progn ,@body (values))
        `(setf *eval-on-init*
               (append
                *eval-on-init*
                (list
                 (lambda ()
                   ,@body))))))

  (defmacro defshader (name &key path type)
    `(progn
       (defasset ,name ,path)
       (defvar ,name nil)
       (delay-evaluation
         (setf ,name (make-instance ,type :name ',name)))))

  (defmacro define-vertex-shader (name &key path)
    `(defshader ,name :path ,path :type 'vertex-shader))

  (defmacro define-fragment-shader (name &key path)
    `(defshader ,name :path ,path :type 'fragment-shader))

  (defmacro define-shader-program (name &key vertex-shader
                                             fragment-shader
                                             uniforms
                                             on-bind
                                             on-render)
    `(progn
       (defvar ,name nil)
       (delay-evaluation
         (setf ,name (make-shader-program :vertex-shader ,vertex-shader
                                          :fragment-shader ,fragment-shader
                                          :uniform-symbols ,uniforms
                                          :on-bind ,on-bind
                                          :on-render ,on-render)))))
  
  (defmacro define-vertex-array-object (name &body body)
    `(progn
       (defvar ,name nil)
       (delay-evaluation
         (setf ,name (progn ,@body)))))
  
  (defmacro define-texture (name file)
    `(progn
       (defvar ,name nil)
       (defasset ,name ,file :png) 
       (delay-evaluation
         (setf ,name (make-instance 'texture :asset ',name)))))

  (defmacro define-array-texture (name file &key width height)
    `(progn
       (defvar ,name nil)
       (defasset ,name ,file :png) 
       (delay-evaluation
         (setf ,name (make-instance 'array-texture
                                    :asset ',name
                                    :width ,width
                                    :height ,height))))))

(define-vertex-shader static-vertex-shader :path (asdf:system-relative-pathname :slither "./render/shaders/static.vert"))
(define-fragment-shader color-fragment-shader :path (asdf:system-relative-pathname :slither "./render/shaders/color.frag"))

(define-shader-program static-shader-program
  :vertex-shader static-vertex-shader
  :fragment-shader color-fragment-shader
  :uniforms '(color))

(define-vertex-shader world-space-vertex-shader :path (asdf:system-relative-pathname :slither "./render/shaders/world-space.vert"))

(define-shader-program color-shader-program
  :vertex-shader world-space-vertex-shader
  :fragment-shader color-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              color)
  :on-bind (lambda (program)
             (setf (uniform-value (get-uniform program 'view-matrix)) *view-matrix*))
  :on-render (lambda (program drawcall-data)
               (setf (uniform-value (get-uniform program 'model-matrix)) (drawcall-data-model-matrix drawcall-data)
                     (uniform-value (get-uniform program 'color)) (drawcall-data-color drawcall-data))))

(define-vertex-shader texture-vertex-shader :path (asdf:system-relative-pathname :slither "./render/shaders/world-space-texture.vert"))
(define-fragment-shader texture-fragment-shader :path (asdf:system-relative-pathname :slither "./render/shaders/texture.frag"))

(define-shader-program texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader texture-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              texture-scale)
  :on-bind (lambda (program)
             (setf (uniform-value (get-uniform program 'view-matrix)) *view-matrix*))
  :on-render (lambda (program drawcall-data)
               (setf (uniform-value (get-uniform program 'model-matrix)) (drawcall-data-model-matrix drawcall-data)
                     (uniform-value (get-uniform program 'texture-scale)) (drawcall-data-texture-scale drawcall-data))))

(define-fragment-shader array-texture-fragment-shader :path (asdf:system-relative-pathname :slither "./render/shaders/array-texture.frag"))

(define-shader-program array-texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader array-texture-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              texture-index)
  :on-bind (lambda (program)
             (setf (uniform-value (get-uniform program 'view-matrix)) *view-matrix*))
  :on-render (lambda (program drawcall-data)
               (setf (uniform-value (get-uniform program 'model-matrix)) (drawcall-data-model-matrix drawcall-data)
                     (uniform-value (get-uniform program 'texture-index)) (drawcall-data-texture-index drawcall-data))))

(define-vertex-array-object quad-vertex-array (make-quad-vertex-array-object))
(define-vertex-array-object texture-vertex-array (make-texture-vertex-array-object))

(defun renderer-init ()
  (set-camera-position (vec2 0 0))
  (eval-on-init)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defvar *view-matrix* nil)
(defun set-camera-position (position &optional
                                     (zoom 1.0)
                                     (aspect (/ slither/window:*window-width* slither/window:*window-height*)))
  (let ((zoom (if (< zoom 0)
                  0
                  zoom)))
  (setf *view-matrix*
        (nm*
         (mscaling (vec2 (/ zoom aspect) zoom))
         (mtranslation
          (v* position -1))))))

(defun screen-space-position (vector)
  (m* (minv *view-matrix*) vector))

(defun screen-space-scale (vector)
  (v/ vector (vec2 (mcref *view-matrix* 0 0)
                   (mcref *view-matrix* 1 1))))

(defun draw-rectangle (position size color &key (shader-program color-shader-program)
                                                (vao quad-vertex-array)
                                                (layer 0)
                                                (depth 0))
  (add-drawcall :drawcall-key (make-drawcall-key :shader-program-id (shader-program-id shader-program)
                                                 :vao vao
                                                 :depth depth
                                                 :layer layer)
                :model-matrix (nm* (mtranslation position)
                                   (mscaling size))
                :color color))

(defun draw-static (&key (shader-program static-shader-program)
                         (vao quad-vertex-array)
                         (depth 0)
                         (layer 0))
  (add-drawcall :drawcall-key (make-drawcall-key :shader-program-id (shader-program-id shader-program)
                                                 :vao vao
                                                 :layer layer
                                                 :depth depth)))

(defun draw-texture (position size texture &key (shader-program texture-shader-program)
                                                (vao texture-vertex-array)
                                                (texture-scale (vec2 1.0 1.0))
                                                (layer 0)
                                                (depth 0))
  (add-drawcall :drawcall-key (make-drawcall-key :shader-program-id (shader-program-id shader-program)
                                                 :vao vao
                                                 :texture-id (texture-id texture)
                                                 :layer layer
                                                 :depth depth)
                :model-matrix (nm* (mtranslation position)
                                   (mscaling size))
                :texture-scale texture-scale))

(defun draw-array-texture (position size index array-texture &key (shader-program array-texture-shader-program)
                                                                  (vao texture-vertex-array)
                                                                  (layer 0)
                                                                  (depth 0))
  (add-drawcall :drawcall-key (make-drawcall-key :shader-program-id (shader-program-id shader-program)
                                                 :vao vao
                                                 :texture-id (texture-id array-texture)
                                                 :layer layer
                                                 :depth depth)
                :model-matrix (nm* (mtranslation position)
                                   (mscaling size))
                :texture-index index))

(defconstant +unset-uniform-id+ 1024)

(defstruct drawcall-data
  (color (vec4 0 0 0 0) :type vec4)
  (model-matrix (meye 3) :type mat3)
  (texture-scale (vec2 0 0) :type vec2)
  (texture-index 0 :type integer))

(deftype drawcall-key ()
  '(unsigned-byte 64))

(defstruct drawcall
  (key 0 :type drawcall-key)
  (data (make-drawcall-data) :type drawcall-data))

(declaim (type (vector drawcall) *drawcall-buffer*))
(defvar *drawcall-buffer*
  (make-array 32768
              :element-type 'drawcall
              :initial-element (make-drawcall)
              :fill-pointer 0
              :adjustable nil))

(declaim (ftype (function (&key (shader-program-id (unsigned-byte 8))
                                (vao (unsigned-byte 8))
                                (texture-id (unsigned-byte 8))
                                (layer (unsigned-byte 8))
                                (depth (unsigned-byte 8)))
                          drawcall-key)))
(defun make-drawcall-key (&key shader-program-id vao (texture-id 0) (layer 0) (depth 0))
  (let ((offset 0) (key 0))
    (declare (type drawcall-key key))
    (flet ((key-insert-field (value size)
             (setf key (dpb value (byte size offset) key))
             (incf offset size)))
      (key-insert-field shader-program-id 8)
      (key-insert-field vao 8)
      (key-insert-field texture-id 8)
      (key-insert-field depth 8)
      (key-insert-field layer 8)
      key)))

(declaim (ftype (function (drawcall-key) (values (unsigned-byte 8) 
                                                 (unsigned-byte 8)
                                                 (unsigned-byte 8)
                                                 (unsigned-byte 8)
                                                 (unsigned-byte 8)))
                drawcall-key-fields))
(defun drawcall-key-fields (key)
  (declare (type drawcall-key key))
  (let ((offset 0))
    (flet ((key-get-field (size)
             (prog1
                 (ldb (byte size offset) key)
               (incf offset size))))
      (values
       (key-get-field 8)
       (key-get-field 8)
       (key-get-field 8)
       (key-get-field 8)
       (key-get-field 8)))))

(defun add-drawcall (&key drawcall-key color model-matrix texture-scale texture-index)
  (let* ((drawcall (aref *drawcall-buffer* (fill-pointer *drawcall-buffer*)))
         (drawcall-data (drawcall-data drawcall)))
    (macrolet ((update-drawcall-field (field)
                 `(when ,field
                    (setf (,(intern (format nil "DRAWCALL-DATA-~a" (symbol-name field))) drawcall-data) ,field))))
      (setf (drawcall-key drawcall) drawcall-key)
      (update-drawcall-field color)
      (update-drawcall-field model-matrix)
      (update-drawcall-field texture-scale)
      (update-drawcall-field texture-index)))
  (incf (fill-pointer *drawcall-buffer*)))

(defun sort-drawcall-buffer ()
  (setf *drawcall-buffer* (sort *drawcall-buffer*
                                #'>
                                :key #'drawcall-key)))

(defun reset-drawcall-buffer ()
  (setf (fill-pointer *drawcall-buffer*) 0))

(defvar *current-shader-program* 100000)
(defvar *current-texture* 100000)
(defvar *current-vao* 100000)

(defun renderer-flush ()
  (gl:clear :color-buffer)
  (sort-drawcall-buffer)
  (let ((first-shader-program-id (drawcall-key-fields (drawcall-key (aref *drawcall-buffer* 0)))))
    (when (= *current-shader-program* first-shader-program-id)
      (program-bind first-shader-program-id)))
  (loop for drawcall across *drawcall-buffer*
        do (multiple-value-bind (shader-program-id vao-id texture-id)
               (drawcall-key-fields (drawcall-key drawcall))
             (unless (= shader-program-id *current-shader-program*)
               (program-bind shader-program-id)
               (setf *current-shader-program* shader-program-id))
             (unless (= *current-vao* vao-id)
               (gl:bind-vertex-array vao-id)
               (setf *current-vao* vao-id))
             (unless (= texture-id *current-texture*)
               (gl:bind-texture :texture-2d texture-id)
               (gl:active-texture :texture0)
               (setf *current-texture* texture-id))
             (program-render shader-program-id (drawcall-data drawcall))))
  (reset-drawcall-buffer)
  (gl:flush))