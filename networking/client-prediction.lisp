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

(defconstant +min-lead-tick-amount+ 3
  "Minimum number of ticks the client runs ahead of the server")

(defconstant +lead-safety-tick-amount+ 1
  "Additional lead in ticks on top of the round trip time")

(defconstant +lead-ahead-tick-amount+ 3
  "Amount of ticks ahead before slowdown")

(defconstant +lead-max-tick-amount+ 8
  "Maximum overshoot before snap back to target tick")

(let ((smoothed-round-trip-time 0.0d0))
  (defun round-trip-time ()
    smoothed-round-trip-time)
  (defun register-round-trip-time (rtt)
    (setf smoothed-round-trip-time (+ (* smoothed-round-trip-time 0.9)
                                      (* rtt 0.1))))
  (defun reset-client-prediction ()
    (setf smoothed-round-trip-time 0.0d0))
  (defun client-prediction-tick-rate-flush ()
    (let* ((rtt-tick-amount (/ smoothed-round-trip-time
                               (tick-delta)))
           (target-tick (+ (server-tick)
                           (max rtt-tick-amount
                                +min-lead-tick-amount+)
                           +lead-safety-tick-amount+))
           (tick-offset (- (current-tick) target-tick)))
      (cond
        ((< tick-offset -2)
         (setf (current-tick) (ceiling target-tick))
         (setf (tick-delta) slither/core::*base-tick-delta*))
        ((< tick-offset 1)
         (setf (tick-delta) (* slither/core::*base-tick-delta* 0.98)))
        ((< tick-offset +lead-ahead-tick-amount+)
         (setf (tick-delta) slither/core::*base-tick-delta*))
        ((< tick-offset +lead-max-tick-amount+)
         (setf (tick-delta) (* slither/core::*base-tick-delta* 1.02)))
        (t
         (setf (current-tick) (ceiling target-tick))
         (setf (tick-delta) slither/core::*base-tick-delta*))))))
