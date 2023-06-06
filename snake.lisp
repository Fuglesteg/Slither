(defpackage #:snake-game
  (:use #:cl))

(in-package #:snake-game)
(require :cl-opengl)
(require :cl-glut)

(defclass game-window (glut:window)
  ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 250 :height 250
                     :mode '(:single :rgb :multisample) :title "Snake game"))

(defmethod glut:display-window :before ((w game-window))
  ;; Select clearing color.
  (gl:clear-color 0 0 0 0)
  ;; Initialize viewing values. (gl:matrix-mode :projection) (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:display ((w game-window))
  (gl:clear :color-buffer)
  ;; Draw white polygon (rectangle) with corners at
  ;; (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0).
  ;; Start processing buffered OpenGL routines.
  (render *player*)
  (gl:flush))

;(defmethod glut:idle ((window game-window))
;  (glut:post-redisplay))


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
  (let* ((left (- (x player) (width player)))
	 (top (- (y player) (height player)))
	 (right (+ (x player) (width player)))
	 (bottom (+ (y player) (height player))))
	(gl:color 1 1 1)
	(gl:with-primitive :polygon
	  (gl:vertex bottom left 0)
	  (gl:vertex top left 0)
	  (gl:vertex top right 0)
	  (gl:vertex bottom right 0))))

(defmethod move ((p player) x y)
  (incf (x p) x)
  (incf (y p) y))

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
    (#\w (move *player* 1 0))
    (#\a (move *player* 0 -1))
    (#\s (move *player* -1 0))
    (#\d (move *player* 0 1))
    (#\Esc (glut:destroy-current-window)))
  (glut:display %game-window%))

(sb-thread:make-thread (lambda () (glut:display-window %game-window%)))
; (glut:display-window %game-window%)

; (glut:display %game-window%)
