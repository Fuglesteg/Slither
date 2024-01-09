(defpackage #:snake-game
  (:use #:cl))

(in-package #:snake-game)
(ql:quickload :cl-opengl)
(ql:quickload :cl-glut)

(defun clamp (x min max)
  (cond
    ((> x max) max)
    ((< x min) min)
    (t x)))

(defclass game-window (glut:window)
  ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 250 :height 250
                     :mode '(:single :rgb :multisample) :title "Snake game"))

(defmethod glut:display-window :before ((w game-window))
  (gl:clear-color 0 0 0 0)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:display ((w game-window))
  (step-game)
  (gl:clear :color-buffer)
  (render *player*)
  (render-bezier)
  (gl:flush))

(defmethod glut:idle ((window game-window))
  (glut:post-redisplay))

(defclass player ()
  ((x
     :initform 0
     :initarg x
     :accessor x
     :type number)
   (y
     :initform 0
     :initarg y
     :accessor y
     :type number)
   (dx
     :initform 0
     :initarg dx
     :accessor dx
     :type number)
   (dy
     :initform 0
     :initarg dy
     :accessor dy
     :type number)
   (width
     :initform 1
     :initarg width
     :accessor width
     :type number)
   (height
     :initform 1
     :initarg height
     :accessor height
     :type number)))

(defmethod render ((player player))
  (let* ((x (/ (x player) 100))
         (y (/ (y player) 100))
         (left (- x (width player)))
	 (top (- y (height player)))
	 (right (+ x (width player)))
	 (bottom (+ y (height player))))
	(gl:color 1 1 1)
	(gl:with-primitive :polygon
	  (gl:vertex bottom left 0)
	  (gl:vertex top left 0)
	  (gl:vertex top right 0)
	  (gl:vertex bottom right 0))))

(defclass vec2 ()
  ((x 
    :initform 0
    :accessor x
    :type single
    :initarg :x)
   (y 
    :initform 0
    :accessor y
    :type single
    :initarg :y)))

(defun render-bezier ()
  (let* ((bezier-points (generate-bezier-points
                        (make-instance 'vec2 :x 0.1 :y 0.1)
                        (make-instance 'vec2 :x 0.8 :y 0.8)
                        (make-instance 'vec2 :x 1.8 :y 0)))
        (length (* (length bezier-points) (* 2 (cffi:foreign-type-size :float)))))
    (gl:with-gl-array (bezier-line :float :count length)
      (gl:bind-buffer :array-buffer (slot-value bezier-line 'pointer))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil 0 0)
      (gl:disable-vertex-attrib-array 0)
      (gl:draw-arrays :line-strip 0 length)
      (gl:bind-buffer :array-buffer 0))))
  

(defun generate-bezier-points (p1 p2 p3 &key (quality 100))
  (loop for i from 0 to quality
        for j = (- 1 (/ i quality))
        collect (compute-bezier-point p1 p2 p3 j)))

(defun compute-bezier-point (p1 p2 p3 i)
  (make-instance 'vec2
                 :x (compute-bezier-value (x p1) (x p2) (x p3) i)
                 :y (compute-bezier-value (y p1) (y p2) (y p3) i)))

(defun compute-bezier-value (p1 p2 p3 i)
  (+ (* (* 2 (- 1 i)) (- p2 p1)) (* (* 2 i) (- p3 p2))))

(defmethod move ((p player) x y)
  (setf (x p) (clamp (+ x (x p)) -250 250))
  (setf (y p) (clamp (+ y (y p)) -250 250)))

(defmethod add-speed ((p player) dx dy)
  (incf (dx p) dx)
  (incf (dy p) dy))

(defun step-game ()
  (decf (dx *player*) 0.001)
  (move *player* (dx *player*) (dy *player*)))

(defvar *player* (make-instance 'player))

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

(defconstant %game-window% (make-instance 'game-window))

(defmethod glut:keyboard ((w game-window) key x y)
  (declare (ignore x y))
  (case key
    (#\w (add-speed *player* 0.1 0))
    (#\a (add-speed *player* 0 -0.1))
    (#\s (add-speed *player* -0.1 0))
    (#\d (add-speed *player* 0 0.1))
    (#\Esc (glut:destroy-current-window))))

(defun start-game ()
  (sb-thread:make-thread (lambda () (glut:display-window %game-window%))))
