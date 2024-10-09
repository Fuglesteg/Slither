(defpackage #:slither
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/shaders
        #:slither/render
        #:sliter/window
        #:slither/input)
  (:export :start-game
           :define-game-object))

(in-package #:slither)

(defvar *game-objects* '())

(defvar *shader-program* nil)

(defun init ()
  (setf *shader-program* (make-instance 'shader-program 
                 :vertex-shader (make-instance 'vertex-shader :path #P"./vertex.glsl")
                 :fragment-shader (make-instance 'fragment-shader :path #P"./circle.glsl")
                 :uniforms (list
                            (make-instance 'uniform :name "position")
                            (make-instance 'uniform :name "screenSize")
                            (make-instance 'uniform :name "zoom"))))
  (setf *bg* (gen-quad))
  (gl:use-program (id *shader-program*))
  (%gl:uniform-2f (id (get-uniform *shader-program* "screenSize")) (screen-width) (screen-height))
  (set-circles))

(defun random-float (&optional (range 1) (precision 10))
  (float (* (/ (random precision) precision) range)))

(defun update ()
  (update-dt)
  (loop for circle in *circles*
        do (tick circle)))

(defun render ()
  (gl:clear :color-buffer)
  (gl:use-program (id *shader-program*))
  (gl:bind-vertex-array *bg*)
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  (gl:bind-vertex-array 0)
  (gl:flush))

(defun start-game ()
  (open-window))

(defclass transform ()
  ((location
    :initform (make-instance 'vec2)
    :accessor location
    :initarg :location)
   (rotation
    :initform 0
    :accessor rotation
    :initarg :rotation)))

(defclass game-object (transform)
   ((color
    :initform (make-instance 'vec4)
    :initarg :color
    :accessor color)))

(defun rotation->vec2 (rotation)
  (vscale (vec2 (cos rotation)
                (sin rotation))
          1))

(defun randomly-negate (number)
  (case (random 2)
    (0 (- number))
    (1 number)))

(defun radians->degrees (radians)
  (* radians (/ 180 pi)))

(defmethod angle-towards ((from vec2) (to vec2))
  (let ((direction (vscale (v- to from) 1)))
    (radians->degrees (atan (vx direction) (vy direction)))))

(defun random-point-outside-bounds (&optional (distance 2))
  (vec2 (randomly-negate (+ distance (random-float distance)))
        (randomly-negate (+ distance (random-float distance)))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun random-color ()
  (vec3 (random-float) (random-float) (random-float)))

(defvar *model-matrix* (meye 3))
(defvar *camera-position* (vec2 1.0))
(defvar *view-matrix* (nmtranslate (nmscale (meye 3) (vec2 0.5 0.5)) *camera-position*))
(defvar *zoom* nil)

(defvar *last-time* 0)
(defvar *dt* 0)

(defun get-time ()
  (glut:get :elapsed-time))

(defun update-dt ()
  (let ((time (get-time)))
    (setf *dt* (- time *last-time*)
          *last-time* time)))

(defun get-fps ()
  (float (/ 1000 *dt*)))
