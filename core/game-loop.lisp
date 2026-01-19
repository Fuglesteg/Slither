(in-package :slither/core)

(defvar *tick* 0)

(defun current-tick ()
  *tick*)

(defun (setf current-tick) (new-value)
  (setf *tick* new-value))
