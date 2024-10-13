(defpackage #:slither/input
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors)
  (:export :key-held-p
           :key-pressed-p
           :mouse-position
           :key-pressed
           :key-released
           :set-mouse-position))

(in-package #:slither/input)

(defparameter *keys* '())

(defclass key-info ()
  ((pressed-frame
    :accessor pressed-frame
    :initarg :pressed-frame
    :type 'integer)))

(defun key-held-p (key)
  (not (null (assoc key *keys*))))

(defun key-pressed-p (key)
  (let ((key-info (cdr (assoc key *keys*))))
    (when key-info
      (= (pressed-frame key-info) (frame)))))

(defun key-pressed (key frame)
  (acons key (make-instance 'key-info :pressed-frame frame) *keys*))

(defun key-released (key)
  (remove key *keys* :key #'car))

(defparameter *mouse-position* (vec2 0 0))

(defun mouse-position ()
  *mouse-position*)

(defun set-mouse-position (x y)
  (vsetf *mouse-position* x y))
