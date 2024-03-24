(defpackage #:slither
  (:use #:cl))

(in-package #:slither)
(require :cl-opengl)
(require :cl-glut)
(require :3d-math)
(require :cffi)
(require :static-vectors)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defclass shader ()
  ((path
    :initarg :path
    :accessor path
    :type pathname)
   (id
    :initarg :id
    :type integer
    :initform 0
    :accessor id)
   (shader-type
    :initform nil
    :initarg :shader-type
    :accessor shader-type)))

(defclass vertex-shader (shader) ()
  (:default-initargs :shader-type :vertex-shader))

(defclass fragment-shader (shader) ()
  (:default-initargs :shader-type :fragment-shader))

(defmethod initialize-instance :after ((shader shader) &key)
  (let ((shader-id (gl:create-shader (shader-type shader))))
    (gl:shader-source shader-id (uiop:read-file-string (path shader)))
    (gl:compile-shader shader-id)
    (setf (id shader) shader-id)))

(defvar *game-objects* '())

(defvar *shader-program* nil)

(defun init ()
  (setf *shader-program* (make-instance 'shader-program 
                 :vertex-shader (make-instance 'vertex-shader :path #P"./vertex.glsl")
                 :fragment-shader (make-instance 'fragment-shader :path #P"./circle.glsl")))
  (setf *bg* (gen-quad)))

(defvar *bg* nil)

(defun gen-quad ()
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (%gl:buffer-data :array-buffer 
                     (* 8 (cffi:foreign-type-size :float))
                     (static-vectors:static-vector-pointer (get-gl-quad 0.0 0.0 2.0 2.0))
                     :static-draw)
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

(defun get-gl-quad (x y width height)
  (static-vectors:make-static-vector 8 
                                     :element-type 'single-float
                                     :initial-contents (get-quad-positions x y width height)))

(defun get-quad-positions (x y width height)
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

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun read-repl ()
  (continuable
    (let ((connection (or micros/swank-api:*emacs-connection* micros::default-connection)))
      (when connection
        (micros::handle-requests connection t)))))

(defun update ()
  (update-dt)
  (read-repl))

(defun render ()
  (gl:clear :color-buffer)
  (gl:use-program (id *shader-program*))
  (gl:bind-vertex-array *bg*)
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  ;(gl:draw-arrays :triangles 6 0)
  (gl:bind-vertex-array 0)
  (gl:flush))

(defclass game-window (glut:window) ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 250 :height 250
   :mode '(:single :rgb :multisample) :title "Snake game"))

(defparameter *window* (make-instance 'game-window))

(defun start-render-thread ()
  (sb-thread:make-thread (lambda ()
                           (glut:display-window (make-instance 'game-window)))))

(defun start-game ()
  (glut:display-window (make-instance 'game-window)))

(defmethod glut:reshape ((w game-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (if (<= width height)
      (gl:ortho -5 5 (/ (* -5 height) width)
                (/ (* 5 height) width) -5 5)
      (gl:ortho (/ (* -5 width) height) (/ (* 5 width) height)
                -5 5 -5 5))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:display-window :before ((w game-window))
  (init)
  (gl:clear-color 0 0 0 0)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:idle ((window game-window))
  (glut:post-redisplay))

(defmethod glut:display ((window game-window))
  (update)
  (render))

(defclass game-object (vector2)
   ((color
    :initform (make-instance 'vector4)
    :initarg :color
    :type vector4
    :accessor color)))

(defclass circle (game-object)
  ((radius
    :initform 0.0
    :initarg :radius
    :type float
    :accessor radius)))

(defclass uniform () 
  ((id
    :type integer
    :accessor id
    :initform 0
    :initarg :id)
   (name
    :type string
    :accessor name
    :initarg :name)))

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
           (gl:get-uniform-location program-id (name uniform)))
         (uniforms program)))
      (setf (id program) program-id))))

(defmethod glut:keyboard ((w game-window) key x y)
  (declare (ignore x y))
  (case key
    (#\i (init))
    (#\Esc (glut:destroy-current-window))))

(defvar *last-time* 0)
(defvar *dt* 0)

(defun get-time ()
  (glut:get :elapsed-time))

(defun update-dt ()
  (let ((time (get-time)))
    (setf *dt* (- time *last-time*)
          *last-time* time)))