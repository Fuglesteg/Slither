(defpackage #:slither/utils
  (:use #:cl 
        #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :continuable))

(in-package #:slither/utils)

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun random-float (&optional (range 1) (precision 10))
  (float (* (/ (random precision) precision) range)))

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

(defun random-element (list)
  (nth (random (length list)) list))

(defun random-color ()
  (vec3 (random-float) (random-float) (random-float)))
