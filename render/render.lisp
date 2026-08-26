(uiop:define-package #:slither/render
  (:use :cl
        :slither/utils
        :org.shirakumo.fraf.math.matrices
        :org.shirakumo.fraf.math.vectors)
  (:use-reexport :slither/render/texture)
  (:import-from :slither/render/uniform
                #:uniform-value
                #:uniform-location
                #:uniform)
  (:import-from :slither/render/array-texture
                #:array-texture
                #:array-texture-asset
                #:with-bound-array-texture)
  (:import-from :slither/render/shader-program
                #:shader-program
                #:find-shader-program
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
           #:define-array-texture
           #:draw-circle
           #:screen-space-rotate
           #:static-vertex-shader
           #:color-fragment-shader
           #:static-shader-program
           #:world-space-vertex-shader
           #:color-shader-program
           #:texture-vertex-shader
           #:texture-fragment-shader
           #:texture-shader-program
           #:array-texture-fragment-shader
           #:array-texture-shader-program
           #:circle-fragment-shader
           #:circle-shader-program
           #:screen-space-vertex-shader
           #:ui-array-texture-shader-program
           #:shader-program
           #:ui-color-shader-program
           #:screen-space-rotation-direction
           #:screen-space-rotation
           #:screen-space-world-position
           #:ui-texture-shader-program
           #:define-drawcall
           #:draw))

(in-package #:slither/render)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *eval-on-init* nil)
  (defvar *initialized* nil)

  (defun eval-on-init ()
    (loop for function in *eval-on-init*
          do (restart-case (funcall function)
               (skip () :report "Skip current function")))
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
                                             on-render
                                             (extends nil))
    `(progn
       (defvar ,name nil)
       (delay-evaluation
         (setf ,name (make-shader-program :vertex-shader ,vertex-shader
                                          :fragment-shader ,fragment-shader
                                          :uniform-symbols ,uniforms
                                          :on-bind ,on-bind
                                          :on-render ,on-render
                                          :extends ,extends)))))

  (defmacro define-vertex-array-object (name &body body)
    `(progn
       (defvar ,name nil)
       (delay-evaluation
         (setf ,name (progn ,@body)))))

  (defmacro define-texture (name file)
    `(progn
       (defvar ,name (make-instance 'texture))
       (defasset ,name ,file :png)
       (delay-evaluation
         (setf (texture-asset ,name) ',name))))

  (defmacro define-array-texture (name file &key width height)
    `(progn
       (defvar ,name (make-instance 'array-texture
                                    :sprite-width ,width
                                    :sprite-height ,height))
       (defasset ,name ,file :png)
       (delay-evaluation
         (setf (array-texture-asset ,name) ',name)))))

(defun m3rotate (degrees)
  (let ((cosine (cos (degrees->radians degrees)))
        (sine (sin (degrees->radians degrees))))
    (mat3 cosine (- sine) 0
          sine cosine 0
          0 0 1)))

(defvar *view-matrix* (mat3))
(defvar *camera-zoom* 1.0)
(defun set-camera-position (position &key
                                     (zoom 1.0)
                                     (aspect-ratio (slither/window:aspect-ratio))
                                     (rotation 0))
  (let ((zoom (if (< zoom 0)
                  0
                  zoom)))
    (setf *camera-zoom* zoom)
    (setf *view-matrix*
          (handler-case
              (nm*
               (mscaling (vec2 (/ zoom aspect-ratio) zoom))
               (m3rotate (- 360 rotation))
               (mtranslation
                (v* position -1)))
            (arithmetic-error (c)
              (declare (ignore c))
              *view-matrix*)))))

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
             (setf (uniform-value (get-uniform program 'view-matrix)) *view-matrix*)))

(define-vertex-shader texture-vertex-shader :path (asdf:system-relative-pathname :slither "./render/shaders/world-space-texture.vert"))
(define-fragment-shader texture-fragment-shader :path (asdf:system-relative-pathname :slither "./render/shaders/texture.frag"))

(define-shader-program texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader texture-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              texture-scale
              color)
  :on-bind (lambda (program)
             (setf (uniform-value (get-uniform program 'view-matrix)) *view-matrix*)))

(define-fragment-shader array-texture-fragment-shader :path (asdf:system-relative-pathname :slither "./render/shaders/array-texture.frag"))

(define-shader-program array-texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader array-texture-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              texture-index
              color)
  :on-bind (lambda (program)
             (setf (uniform-value (get-uniform program 'view-matrix)) *view-matrix*)))

(define-fragment-shader circle-fragment-shader
  :path (asdf:system-relative-pathname :slither "./render/shaders/circle.frag"))

(define-shader-program circle-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader circle-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              color)
  :on-bind (lambda (program)
             (setf (uniform-value (get-uniform program 'view-matrix)) *view-matrix*)))

(define-vertex-shader screen-space-vertex-shader
  :path (asdf:system-relative-pathname :slither "./render/shaders/screen-space.vert"))

(define-vertex-shader screen-space-texture-vertex-shader
  :path (asdf:system-relative-pathname :slither "./render/shaders/screen-space-texture.vert"))

(defvar *ui-view-matrix* (mat3))

(defun ui-view-matrix-update ()
  (setf *ui-view-matrix*
        (nm*
         (mtranslation (vec2 -1.0 1.0))
         (mscaling (vec2 (/ 2 slither/window:*window-width*)
                         (/ 2 slither/window:*window-height*))))))

(define-shader-program ui-texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader texture-fragment-shader
  :uniforms '(model-matrix
              view-matrix
              texture-scale
              color)
  :on-bind (lambda (program)
             (setf (uniform-value (get-uniform program 'view-matrix)) *ui-view-matrix*)))

(define-shader-program ui-array-texture-shader-program
  :vertex-shader texture-vertex-shader
  :fragment-shader array-texture-fragment-shader
  :uniforms '(model-matrix
              texture-index
              color
              view-matrix)
  :on-bind (lambda (program)
             (ui-view-matrix-update)
             (setf (uniform-value (get-uniform program 'view-matrix)) *ui-view-matrix*)))

(define-shader-program ui-color-shader-program
  :vertex-shader world-space-vertex-shader
  :fragment-shader color-fragment-shader
  :uniforms '(model-matrix
              color
              view-matrix)
  :on-bind (lambda (program)
             (ui-view-matrix-update)
             (setf (uniform-value (get-uniform program 'view-matrix)) *ui-view-matrix*)))


(define-vertex-array-object quad-vertex-array (make-quad-vertex-array-object))
(define-vertex-array-object texture-vertex-array (make-texture-vertex-array-object))

(defun renderer-init ()
  (set-camera-position (vec2 0 0))
  (eval-on-init)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defun screen-space-world-position (vector)
  (if (= 0 (mdet *view-matrix*))
      vector
      (m* (minv *view-matrix*) vector)))

(defun screen-space-position (vector)
  (if (= 0 (mdet *view-matrix*))
      vector
      (m* *view-matrix* vector)))

(defun screen-space-scale (vector)
    (v/ vector
        (vec2 *camera-zoom*
              *camera-zoom*)))

(defun screen-space-rotate (rotation)
  (rotation-lerp rotation
                 (radians->degrees (atan (mcref *view-matrix* 0 1)
                                         (mcref *view-matrix* 0 0)))
                 1.0))

(defun screen-space-rotation-direction ()
  (safe-vscale (vec2 (mcref *view-matrix* 0 1)
                     (mcref *view-matrix* 0 0))
               1.0))

(defun screen-space-rotation ()
  (vec2->rotation (screen-space-rotation-direction)))

(defgeneric drawcall-data-bind-program (drawcall-data shader-program)
  (:method ((drawcall-data t) shader-program)))

(defmacro define-drawcall (name data &key shader-program (vao 0) (layer 0) (depth 0) texture array-texture draw)
  (let ((drawcall-data-symbol (intern (string-upcase (format nil "~a-drawcall-data" name))))
        (drawcall-data-make-symbol (intern (string-upcase (format nil "make-~a-drawcall-data" name))))
        (drawcall-function-symbol (intern (string-upcase (format nil "draw-~a" name))))
        (drawcall-data-symbol-bindings (mapcar #'car data))
        (drawcall-key-symbol-binding-default-pairs `((shader-program ,shader-program) (vao ,vao) (layer ,layer) (depth ,depth) (texture ,texture) (array-texture ,array-texture)))
        (drawcall-key-symbol-binding-keyword-pairs '(:shader-program-id (shader-program-id shader-program) :vao vao
                                                     :layer layer :depth depth :texture-id (or (and texture (texture-id texture)) 0)
                                                     :array-texture-id (or (and array-texture (slither/render/array-texture::array-texture-id array-texture)) 0))))
    (destructuring-bind (draw-lambda-list &body draw-body) (or draw `(() ()))
      `(progn
         ,@(when data
             `((defstruct (,drawcall-data-symbol (:constructor ,drawcall-data-make-symbol))
                 ,@data)))
         (defun ,drawcall-function-symbol (,@draw-lambda-list
                                           ,@(if (member '&key draw-lambda-list) nil (list '&key))
                                           ,@drawcall-key-symbol-binding-default-pairs)
           (when *initialized*
             (sb-vm:with-arena (*drawcall-arena*)
               ,(let* ((draw-macro-drawcall-key-form
                         `(make-drawcall-key ,@drawcall-key-symbol-binding-keyword-pairs))
                       (draw-macro
                         (if data
                             `(draw (&rest keyword-arguments)
                                    `(add-drawcall :drawcall-key ,',draw-macro-drawcall-key-form
                                                   :drawcall-data (,',drawcall-data-make-symbol ,@keyword-arguments)))
                             `(draw (&rest keyword-arguments)
                                    (declare (ignore keyword-arguments))
                                    `(add-drawcall :drawcall-key ,',draw-macro-drawcall-key-form)))))
                  `(macrolet (,draw-macro)
                     ,@(if draw
                           draw-body
                           '((draw))))))))
         ,@(when data
             `((defmethod drawcall-data-bind-program ((drawcall-data ,drawcall-data-symbol) (shader-program shader-program))
                 ,@(loop for drawcall-data-slot in drawcall-data-symbol-bindings
                         collect `(setf (uniform-value (get-uniform shader-program ',drawcall-data-slot)) (,(intern (string-upcase (format nil "~a-~a" drawcall-data-symbol drawcall-data-slot)))
                                                                                                           drawcall-data))))))))))

(define-drawcall rectangle
  ((model-matrix (meye 3) :type mat3)
   (color (vec4) :type vec4))
  :shader-program color-shader-program
  :vao quad-vertex-array
  :draw ((position size &key (color (vec4 1.0)) (anchor :center))
         (let* ((position (position-apply-anchor position size anchor))
                (model-matrix (nm* (mtranslation position)
                                   (mscaling size))))
           (draw :color color
                 :model-matrix model-matrix))))

(define-drawcall circle
  ((model-matrix (meye 3) :type mat3)
   (color (vec4) :type vec4))
  :shader-program circle-shader-program
  :vao texture-vertex-array
  :draw ((position size &key (color (vec4 1.0)))
         (let ((model-matrix (nm* (mtranslation position)
                                 (mscaling size))))
           (draw :model-matrix model-matrix
                 :color color))))

(define-drawcall static
  nil
  :shader-program static-shader-program
  :vao quad-vertex-array)

(define-drawcall texture
  ((model-matrix (meye 3) :type mat3)
   (color (vec4) :type vec4)
   (texture-scale (vec2 1.0) :type vec2))
  :shader-program texture-shader-program
  :vao texture-vertex-array
  :draw ((position size &key (color (vec4 1.0)) (rotation 0) (anchor :center) (texture-scale (vec2 1.0)))
         (let* ((position (position-apply-anchor position size anchor))
                (model-matrix (nm* (mtranslation position)
                                   (m3rotate rotation)
                                   (mscaling size))))
           (draw :model-matrix model-matrix
                 :color color
                 :texture-scale texture-scale))))

(define-drawcall array-texture
  ((model-matrix (meye 3) :type mat3)
   (color (vec4) :type vec4)
   (texture-index 0 :type fixnum))
  :shader-program array-texture-shader-program
  :vao texture-vertex-array
  :draw ((position size &key (color (vec4 1.0)) (rotation 0) (anchor :center) (texture-index 0))
         (let* ((position (position-apply-anchor position size anchor))
                (model-matrix (nm* (mtranslation position)
                                   (m3rotate rotation)
                                   (mscaling size))))
           (draw :model-matrix model-matrix
                 :color color
                 :texture-index texture-index))))

(deftype drawcall-key ()
  '(unsigned-byte 64))

(defstruct drawcall
  (key 0 :type drawcall-key)
  (data (make-drawcall-data) :type (or null structure-object)))

(declaim (type (vector (or null drawcall)) *drawcall-buffer*))
(defvar *drawcall-buffer*
  (make-array 32768
              :element-type '(or null drawcall)
              :fill-pointer nil
              :adjustable nil))

(declaim (ftype (function (&key (shader-program-id (unsigned-byte 8))
                                (vao (unsigned-byte 8))
                                (texture-id (unsigned-byte 8))
                                (array-texture-id (unsigned-byte 8))
                                (layer (unsigned-byte 8))
                                (depth (unsigned-byte 8)))
                          drawcall-key)))
(defun make-drawcall-key (&key shader-program-id vao (texture-id 0) (array-texture-id 0) (layer 0) (depth 0))
  (let ((offset 0) (key 0))
    (declare (type drawcall-key key))
    (flet ((key-insert-field (value size)
             (setf key (dpb value (byte size offset) key))
             (incf offset size)))
      (key-insert-field shader-program-id 8)
      (key-insert-field vao 8)
      (key-insert-field texture-id 8)
      (key-insert-field array-texture-id 8)
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
       (key-get-field 8)
       (key-get-field 8)))))

(defvar *drawcall-arena* (sb-vm:new-arena (* 16 1024 1024))) ; 16 MBs

(defun add-drawcall (&key drawcall-key drawcall-data)
  (vector-push (make-drawcall :key drawcall-key
                              :data drawcall-data)
               *drawcall-buffer*))

(defun sort-drawcall-buffer ()
  (setf *drawcall-buffer* (sort *drawcall-buffer*
                                #'<
                                :key #'drawcall-key)))

(defun reset-drawcall-buffer ()
  (setf (fill-pointer *drawcall-buffer*) 0)
  (sb-vm:rewind-arena *drawcall-arena*))

(defvar *current-shader-program* most-positive-fixnum)
(defvar *current-texture* most-positive-fixnum)
(defvar *current-vao* most-positive-fixnum)
(defvar *current-array-texture* most-positive-fixnum)

(defun renderer-flush ()
  (gl:clear :color-buffer)
  (sort-drawcall-buffer)
  (let ((first-shader-program-id (drawcall-key-fields (drawcall-key (aref *drawcall-buffer* 0)))))
    (when (= *current-shader-program* first-shader-program-id)
      (program-bind first-shader-program-id)))
  (loop for drawcall across *drawcall-buffer*
        do (multiple-value-bind (shader-program-id vao-id texture-id array-texture-id)
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
             (unless (= array-texture-id *current-array-texture*)
               (gl:bind-texture :texture-2d-array array-texture-id)
               (setf *current-array-texture* array-texture-id))
             (when-let ((shader-program (find-shader-program shader-program-id)))
               (drawcall-data-bind-program (drawcall-data drawcall) shader-program)
               (program-render shader-program))))
  (reset-drawcall-buffer)
  (gl:flush))
