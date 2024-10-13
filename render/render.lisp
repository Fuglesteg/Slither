(defpackage #:slither/render
  (:use #:cl)
  (:import-from :alexandria
                #:with-gensyms))

(in-package #:slither/render)

(defparameter *color-shader-program* nil)
(defun color-shader-program ()
  (when (null *color-shader-program*)
    (setf *color-shader-program* 
          (make-instance 'shader-program
                         :vertex-shader (make-instance 'vertex-shader
                                                       :path "./vertex.glsl")
                         :fragment-shader (make-instance 'fragment-shader
                                                         :path "./color.glsl")
                         :uniforms (list
                                    (make-instance 'uniform
                                                   :name "modelMatrix")
                                    (make-instance 'uniform
                                                   :name "viewMatrix")))))
  *color-shader-program*)

(defparameter *sprite-shader-program* nil)
(defun sprite-shader-program ()
  (when (null *sprite-shader-program*)
    (setf *sprite-shader-program*
          (make-instance 'shader-program
                         :vertex-shader (make-instance 'vertex-shader
                                                       :path "./vertex.glsl")
                         :fragment-shader (make-instance 'fragment-shader
                                                         :path "./sprite.glsl")
                         :uniforms (list
                                    (make-instance 'uniform
                                                   :name "modelMatrix")
                                    (make-instance 'uniform
                                                   :name "viewMatrix")))))
  *sprite-shader-program*)

(defclass renderable ()
  ((vao
    :accessor vao
    :initarg :vao)
   (shader-program
    :accessor shader-program
    :initarg :shader-program)))

(defgeneric render (renderable))

(defmethod render :before ((renderable renderable))
  (with-slots (shader-program vao) renderable
    (gl:use-program (id shader-program))
    (gl:bind-vertex-array vao)))

(defmethod render :after ((renderable renderable))
  (gl:bind-vertex-array 0)
  (gl:use-program 0))

(defclass quad (renderable) ()
  (:default-initargs 
   :vao (gen-quad)
   :shader-program (color-shader)))

(defmethod render ((quad quad))
  (%gl:draw-elements :triangles 6 :unsigned-int 0))

(defclass sprite (quad)
  ((texture
    :accessor texture
    :initarg :texture))
  (:default-initargs
   :shader-program (sprite-shader)))

(defmethod render :before ((sprite sprite))
  (call-next-method)
  (bind-texture (texture sprite)))

(defmethod render :after ((sprite sprite))
  (bind-texture 0)
  (call-next-method))

(defmacro with-vertex-array (name &body body)
  `(let ((,name (gl:gen-vertex-array)))
     (gl:bind-vertex-array ,name)
     ,@body
     (gl:bind-vertex-array 0)))

(defmacro with-buffer (name type &body body)
  `(let ((,name (gl:gen-buffer)))
     (gl:bind-buffer ,type ,name)
     ,@body
     (gl:bind-buffer ,type 0)))

(defun gen-quad-buffer (&key (usage-frequency :dynamic-draw))
  (with-buffer vbo :array-buffer
    (with-quad-vertices-array (vertice-data)
      (%gl:buffer-data :array-buffer
                       (* (length vertice-data) 
                          (cffi:foreign-type-size :float))
                       (static-vectors:static-vector-pointer vertice-data)
                       usage-frequency))))

(defun cffi-type-of-vector (vector)
  (lisp-type->cffi-type (elt vector 0)))

(defun lisp-type->cffi-type (lisp-type)
  (case lisp-type
    (single-float :float)
    (double-float :double)
    (string :string)
    (fixnum :int)
    (integer :long)
    (boolean :bool)
    (otherwise (error "No C conversion available for type: ~a" lisp-type))))

(defun vector->static-vector (vector)
  (static-vectors:make-static-vector (length vector)
                                     :element-type 
                                     (cffi-type-of-vector vector)
                                     :initial-contents vector))

(defun cffi-memory-size-of-vector (vector)
  (* (length vector) (cffi-type-of-vector vector)))

(defun send-buffer-data (vector &key (buffer-type :array-buffer)
                                     (usage-type :static-draw))
  (%gl:buffer-data buffer-type
                   (cffi-memory-size-of-vector vector)
                   (static-vectors:static-vector-pointer 
                    (vector->static-vector vector))
                   usage-type))

(defmacro define-vertex-array-object (name &key vertices 
                                                indices 
                                                (usage-type :static-draw))
  (with-gensyms (vao vbo ebo)
    `(with-vertex-array ,vao
       (with-buffer ,vbo :array-buffer
         (send-buffer-data ,vertices
                           :usage-type ,usage-type))
       (with-buffer ,ebo :element-array-buffer
         (send-buffer-data ,indices
                           :usage-type ,usage-type))
       (gl:vertex-attrib-pointer 0 2 :float nil (* 2 (cffi:foreign-type-size :float)) 0))))

(defun gen-quad ()
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (let ((sv (get-gl-quad 0.0 0.0 1.0 1.0)))
      (%gl:buffer-data :array-buffer 
                       (* 8 (cffi:foreign-type-size :float))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw)
      (static-vectors:free-static-vector sv))
    (gl:bind-buffer :element-array-buffer ebo)
    (let ((sv (get-quad-indices-array)))
      (%gl:buffer-data :element-array-buffer 
                       (* 6 (cffi:foreign-type-size :unsigned-int))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw)
      (static-vectors:free-static-vector sv))
    (gl:vertex-attrib-pointer 0 2 :float nil (* 2 (cffi:foreign-type-size :float)) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    vao))

(defmacro with-quad-vertices-array ((var &key (x 0)(y 0) 
                                              (width 1) 
                                              (height 1)) 
                                    &body body)
  `(static-vectors:with-static-vector (,var 8 
                                     :element-type 'single-float
                                     :initial-contents (get-quad-positions x y width height))
     ,@body))



(defun get-quad-vertices (x y width height)
  (let ((left (- x (/ width 2)))
        (right (+ x (/ width 2)))
        (down (+ y (/ height 2)))
        (up (- y (/ height 2))))
    (list 
     right up
     right down
     left down
     left up)))

(defun get-quad-indices-array ()
  (static-vectors:make-static-vector 6 
                                     :element-type '(unsigned-byte 32)
                                     :initial-contents '(0 1 3 1 2 3)))
