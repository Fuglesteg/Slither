(in-package :slither/core)

(declaim (type integer *tick*))
(defvar *tick* 0)

(defun current-tick ()
  *tick*)

(defun (setf current-tick) (new-value)
  (setf *tick* new-value))

(declaim (type single-float *delta-time*))
(defvar *delta-time* 0.0)

(defun delta-time ()
  *delta-time*)

(defun dt ()
  (delta-time))

(defun (setf delta-time) (new-value)
    (setf *delta-time* new-value))

(declaim (type single-float *accumulative-delta-time*))
(defvar *accumulative-delta-time* 0.0)

(defun accumulative-delta-time ()
  *accumulative-delta-time*)

(defun (setf accumulative-delta-time) (new-value)
  (setf *accumulative-delta-time* new-value))

(defvar *tick-delta* (/ 1.0 60.0))

(defun tick-delta ()
  *tick-delta*)

(defun (setf tick-delta) (new-value)
  (setf *tick-delta* new-value))

(defvar *last-frame-time* 0)
(defvar *current-time* 0)

(defun calculate-delta-time (current-time)
  (setf *current-time* current-time)
  (setf (delta-time) (- *current-time* *last-frame-time*))
  (setf *last-frame-time* current-time))

(let ((accumulated-time 0)
      last-tick-time)
  (defun last-tick-time ()
    last-tick-time)
  (defun (setf last-tick-time) (new-value)
    (setf last-tick-time new-value))
  (defun ticks-behind ()
    (floor (/ (- (org.shirakumo.fraf.glfw:time) last-tick-time)
              (tick-delta))))
  (defun calculate-accumulated-ticks ()
    (floor (/ accumulated-time (tick-delta))))
  (defun accumulated-ticks (current-time)
    (when (null last-tick-time)
      (setf last-tick-time current-time))
    (incf accumulated-time (- current-time last-tick-time))
    (let ((accumulated-ticks (calculate-accumulated-ticks)))
      (decf accumulated-time (* (tick-delta) accumulated-ticks))
      (setf last-tick-time current-time)
      (min accumulated-ticks 10)))
  (defun interpolation-alpha ()
    (clamp (/ accumulated-time (tick-delta)) 0.0 1.0)))
