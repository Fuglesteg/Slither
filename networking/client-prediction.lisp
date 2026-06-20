(uiop:define-package :slither/networking/client-prediction
  (:use :cl
        :slither/core)
  (:export :server-tick
           :client-prediction-tick-rate-flush
           :register-round-trip-time
           :round-trip-time
           :reset-client-prediction))

(in-package :slither/networking/client-prediction)

(let ((server-tick 0))
  (defun server-tick ()
    server-tick)
  (defun (setf server-tick) (new-value)
    (setf server-tick new-value)))

(let ((smoothed-round-trip-time 0.0d0))
  (defun round-trip-time ()
    smoothed-round-trip-time)
  (defun register-round-trip-time (rtt)
    (setf smoothed-round-trip-time (+ (* smoothed-round-trip-time 0.9)
                                      (* rtt 0.1))))
  (defun reset-client-prediction ()
    (setf smoothed-round-trip-time 0.0d0))
  (defun client-prediction-tick-rate-flush ()
    (let* ((target-tick (+ (server-tick) (ceiling (/ smoothed-round-trip-time
                                                     (tick-delta)))))
           (tick-offset (- (current-tick) target-tick)))
      (cond
        ((> (abs tick-offset) 100) (setf (current-tick) target-tick))
        ((> tick-offset 1.5)
         (setf (tick-delta) (* slither/core::*base-tick-delta* 1.02)))
        ((< tick-offset -1.5)
         (setf (tick-delta) (* slither/core::*base-tick-delta* 0.98)))
        (t (setf (tick-delta) slither/core::*base-tick-delta*))))))
