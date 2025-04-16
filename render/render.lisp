(uiop:define-package #:slither/render
  (:use :cl
        :org.shirakumo.fraf.math.matrices
        :org.shirakumo.fraf.math.vectors)
  (:import-from :slither/render/uniform
                #:value
                #:uniform)
  (:import-from :slither/render/texture
                #:bind-texture)
  (:import-from :slither/render/shader-program
                #:shader-program
                #:shader-program-id
                #:get-uniform
                #:make-shader-program)
  (:import-from :slither/render/shader
                #:define-vertex-shader
                #:define-fragment-shader
                #:vertex-shader
                #:fragment-shader
                #:shader)
  (:import-from :slither/render/vertex
                #:quad
                #:sprite)
  (:import-from :slither/utils
                #:defmemo)
  (:export #:set-model-matrix
           #:set-camera-position
           #:sprite
           #:render
           #:static
           #:quad
           #:renderer-init
           #:draw-rectangle))

(in-package #:slither/render)

(defvar *model-matrix* (meye 3))
(defun set-model-matrix (matrix)
  (setf *model-matrix* matrix))

(defvar *view-matrix* nil)
(defun set-camera-position (position)
  (setf *view-matrix* (nmtranslate (nmscale (meye 3) (vec2 0.09 0.16)) position))) ; TODO: aspect ratio

(defvar *color* nil)

(define-vertex-shader static :path "./render/shaders/static.vert")
(define-vertex-shader world-space :path "./render/shaders/world-space.vert")
(define-fragment-shader color :path "./render/shaders/color.frag")
(define-vertex-shader texture :path "./render/shaders/world-space.vert")
(define-fragment-shader sprite :path "./render/shaders/color.frag")

(defmemo static-shader-program
  (make-shader-program :vertex-shader (static)
                       :fragment-shader (color)
                       :uniform-symbols '(color)))

(defmemo color-shader-program
  (make-shader-program :vertex-shader (world-space)
                       :fragment-shader (color)
                       :uniform-symbols '(model-matrix
                                          view-matrix
                                          color)))

(defmemo sprite-shader-program
  (make-shader-program :vertex-shader (texture)
                       :fragment-shader (sprite)
                       :uniform-symbols '(model-matrix
                                          view-matrix)))

(defclass renderable ()
  ((vao
    :accessor vao
    :initarg :vao)
   (shader-program
    :accessor shader-program
    :initarg :shader-program)))

(defgeneric render (renderable))

(defmethod render :around ((renderable renderable))
  (with-slots (shader-program vao) renderable
    (gl:use-program (shader-program-id shader-program))
    (gl:bind-vertex-array vao)
    (call-next-method)
    (gl:bind-vertex-array 0)
    (gl:use-program 0)))

(defclass static (renderable) ()
  (:default-initargs
   :vao (quad)
   :shader-program (static-shader-program)))

(defmethod render ((static static))
  (%gl:draw-elements :triangles 6 :unsigned-int 0))

(defclass quad (renderable) ()
  (:default-initargs 
   :vao (quad)
   :shader-program (color-shader-program)))

(defmethod render :before ((quad quad))
  (setf (value (get-uniform (shader-program quad) 'model-matrix)) *model-matrix*
        (value (get-uniform (shader-program quad) 'view-matrix)) *view-matrix*
        (value (get-uniform (shader-program quad) 'color)) *color*))

(defmethod render ((quad quad))
  (%gl:draw-elements :triangles 6 :unsigned-int 0))

(defclass sprite (quad)
  ((texture
    :accessor sprite-texture
    :initarg :texture))
  (:default-initargs
   :vao (sprite)
   :shader-program (sprite-shader-program)))

(defmethod render :before ((sprite sprite))
  (call-next-method)
  (bind-texture (sprite-texture sprite)))

(defmethod render :after ((sprite sprite))
  (bind-texture 0)
  (call-next-method))

(defvar *quad* nil)
(defvar *sprite* nil)

(defun renderer-init ()
  (when (and (not *quad*) (not *sprite*))
    (setf *quad* (make-instance 'quad)
          *sprite* (make-instance 'sprite))))

(defun draw-rectangle (position size color)
  (let ((*model-matrix* (nmtranslate (nmscale (meye 3) size) position))
        (*color* color))
    (render *quad*)))
  
;(defun draw-texture (position size texture))