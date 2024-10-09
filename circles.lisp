(defpackage #:circles
  (:use :slither))

(in-package #:circles)

(defvar *bg* nil)

(defun make-random-circle (uniform-location)
  (make-instance 'circle 
                 :location (wrap-in-uniform
                            (vec2 (random-float 2)
                                  (random-float 2))
                            uniform-location)
                 :rotation (random 360)
                 :radius (wrap-in-uniform
                          (/ (- 1.2 (random-float)) 2)
                          (+ uniform-location 1)
                          :type 'radius)
                 :color (wrap-in-uniform
                         (random-color)
                         (+ uniform-location 2))))

(defun set-circles ()
  (let* ((circles-uniform-location 2)
         (circles (loop repeat 20
                        for i from circles-uniform-location by 3
                        collect (make-random-circle i))))
    (setf *circles* circles)))

(defclass circle (game-object)
  ((radius
    :initform 0.0
    :initarg :radius)))

(defmethod location ((circle circle))
  (value (slot-value circle 'location)))

(defmethod (setf location) ((location vec2) (circle circle))
  (setf (slot-value (slot-value circle 'location) 'value) location))

(defmethod (setf location) :after ((location vec2) (circle circle))
  (setf (uniform-value (slot-value circle 'location))
        (m* (nmtranslate
             (nmscale (meye 3) (vec2 (value *zoom*)))
             *camera-position*)
            *model-matrix*
            location)))

(defmethod radius ((circle circle))
  (value (slot-value circle 'radius)))

(defmethod (setf radius) ((radius float) (circle circle))
  (setf (slot-value (slot-value circle 'radius) 'value) radius))

(defmethod (setf radius) :after ((radius float) (circle circle))
  (setf (uniform-value (slot-value circle 'radius))
        (* radius (value *zoom*))))

(defmethod tick ((circle circle))
  (with-slots (rotation radius) circle
    (if (out-of-bounds-p circle :forgiveness (+ 2 (value radius)))
        (let ((point-outside (random-point-outside-bounds))
              (point-inside (random-point-in-bounds)))
          (setf (location circle) point-outside
                (rotation circle) (angle-towards point-outside point-inside)))
        (let ((direction (rotation->vec2 rotation)))
          (setf (location circle) (v+ (location circle) (v/ direction 100)))))))

(defun random-point-in-bounds ()
  (vec2 (random-float 2) (random-float 2)))

(defgeneric out-of-bounds-p (circle &key forgiveness))

(defmethod out-of-bounds-p ((circle circle) &key (forgiveness 0))
  (let ((location (location circle))
        (radius
          (radius circle)))
    (out-of-bounds-p location :forgiveness (+ forgiveness radius))))

(defmethod out-of-bounds-p ((point vec2) &key (forgiveness 0))
  (with-vec (x y) point
    (or (< (+ x forgiveness) 0)
        (> (- x forgiveness) 2)
        (< (+ y forgiveness) 0)
        (> (- y forgiveness) 2))))

(defmethod (setf uniform-value) :after ((value float) (uniform (eql *zoom*)))
  (dolist (circle *circles*)
    (uniform-sync (slot-value circle 'radius))))

(defclass radius (uniform-wrapper) ())

(defmethod uniform-sync ((radius radius))
  (setf (uniform-value radius) (* (value radius) (if *zoom* (value *zoom*) 0))))

