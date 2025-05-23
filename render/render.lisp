(uiop:define-package #:slither/render
  (:use :cl
        :org.shirakumo.fraf.math.matrices
        :org.shirakumo.fraf.math.vectors)
  (:import-from :slither/render/uniform
                #:uniform-value
                #:uniform)
  (:import-from :slither/render/texture
                #:texture
                #:with-bound-texture)
  (:import-from :slither/render/array-texture
                #:array-texture
                #:with-bound-array-texture)
  (:import-from :slither/render/shader-program
                #:shader-program
                #:shader-program-id
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
                                             uniforms)
    `(progn
       (defvar ,name nil)
       (delay-evaluation
         (setf ,name (make-shader-program :vertex-shader ,vertex-shader
                                          :fragment-shader ,fragment-shader
                                          :uniform-symbols ,uniforms)))))
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
              color))

(define-vertex-shader texture-vertex-shader :path (asdf:system-relative-pathname :slither "./render/shaders/world-space-texture.vert"))
(define-fragment-shader texture-fragment-shader :path (asdf:system-relative-pathname :slither "./render/shaders/texture.frag"))

(define-shader-program texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader texture-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              texture-scale))

(define-fragment-shader array-texture-fragment-shader :path (asdf:system-relative-pathname :slither "./render/shaders/array-texture.frag"))

(define-shader-program array-texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader array-texture-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              texture-index))

(define-vertex-array-object quad-vertex-array (make-quad-vertex-array-object))
(define-vertex-array-object texture-vertex-array (make-texture-vertex-array-object))

(defun renderer-init ()
  (set-camera-position (vec2 0 0))
  (eval-on-init)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defvar *view-matrix* nil)
(defun set-camera-position (position &optional (zoom 1.0) (aspect (/ slither/window:*window-width* slither/window:*window-height*)))
  (assert (> zoom 0))
  (setf *view-matrix*
        (nm*
         (mscaling (vec2 (/ zoom aspect) zoom))
         (mtranslation
          (v* position -1)))))

(defun screen-space-position (vector)
  (m* (minv *view-matrix*) vector))

(defun screen-space-scale (vector)
  (v/ vector (vec2 (mcref *view-matrix* 0 0)
                   (mcref *view-matrix* 1 1))))

(defun draw-rectangle (position size color &key (shader-program color-shader-program)
                                                (vao quad-vertex-array))
  (with-bound-shader-program shader-program
    (with-bound-vertex-array vao
      (setf (uniform-value (get-uniform shader-program 'model-matrix))
            (nmscale (nmtranslate (meye 3)
                                  position)
                     size))
      (setf (uniform-value (get-uniform shader-program 'view-matrix)) *view-matrix*)
      (setf (uniform-value (get-uniform shader-program 'color)) color)
      (%gl:draw-elements :triangles 6 :unsigned-int 0))))

(defun draw-static (&key (shader-program static-shader-program)
                         (vao quad-vertex-array))
  (with-bound-shader-program shader-program
    (with-bound-vertex-array vao
      (%gl:draw-elements :triangles 6 :unsigned-int 0))))

(defun draw-texture (position size texture &key (shader-program texture-shader-program)
                                                (vao texture-vertex-array)
                                                (texture-scale (vec2 1.0 1.0)))
  (with-bound-shader-program shader-program
    (with-bound-vertex-array vao
      (setf (uniform-value (get-uniform shader-program 'model-matrix))
            (nm* (mtranslation position)
                 (mscaling size))
            (uniform-value (get-uniform shader-program 'view-matrix)) *view-matrix*
            (uniform-value (get-uniform shader-program 'texture-scale)) texture-scale)
    (gl:active-texture :texture0)
      (with-bound-texture texture
        (%gl:draw-elements :triangles 6 :unsigned-int 0)))))

(defun draw-array-texture (position size index array-texture &key (shader-program array-texture-shader-program)
                                                                  (vao texture-vertex-array))
  (with-bound-shader-program shader-program
    (with-bound-vertex-array vao
      (setf (uniform-value (get-uniform shader-program 'model-matrix))
            (nm* (mtranslation position)
                 (mscaling size))
            (uniform-value (get-uniform shader-program 'view-matrix)) *view-matrix*
            (uniform-value (get-uniform shader-program 'texture-index)) index)
      (gl:active-texture :texture0)
      (with-bound-array-texture array-texture
        (%gl:draw-elements :triangles 6 :unsigned-int 0)))))