(uiop:define-package :slither/networking/client-prediction
  (:use :cl
        :slither/core)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :server-tick
           :ticks-to-predict
           :client-prediction-tick-rate-flush
           :estimated-tick-offset
           :register-round-trip-time))

(in-package :slither/networking/client-prediction)

(defvar *round-trip-time*
  0.0d0
  "RTT measures how long a packet takes to reach the server and return to the client")

(let ((smoothed-server-tick 0.0d0))
  (defun (setf server-tick) (new-value)
    (incf smoothed-server-tick (* (- new-value smoothed-server-tick) 0.1)))
  (defun server-tick ()
    (round smoothed-server-tick)))

(defun estimated-tick-offset ()
  (1+ (ceiling (/ *round-trip-time* (tick-delta)))))

(defun tick-drift ()
  (- (current-tick) (server-tick) (ticks-to-predict)))

(let ((tick-offset nil)
      (smoothed-tick-drift 0.0d0)
      (ticks-at-current-offset 0))
  (defun register-round-trip-time (sent-at)
    (let ((raw-rtt (- (glfw:time) sent-at)))
      (incf *round-trip-time* (* (- raw-rtt *round-trip-time*) 0.1)))
    (unless tick-offset
      (setf tick-offset (1+ (ceiling (/ *round-trip-time* (tick-delta)))))))
  (defun ticks-to-predict ()
    (or tick-offset 0))
  (defun client-prediction-tick-rate-flush ()
    (incf smoothed-tick-drift (* (- (tick-drift) smoothed-tick-drift) 0.1))
    (cond
      ((> smoothed-tick-drift 0.5)
       (setf (tick-delta) (* slither/core::*base-tick-delta* 1.02)))
      ((< smoothed-tick-drift -0.5)
       (setf (tick-delta) (* slither/core::*base-tick-delta* 0.98)))
      (t
       (setf (tick-delta) slither/core::*base-tick-delta*)))
    (incf ticks-at-current-offset)
    (when (> ticks-at-current-offset 120)
      (cond
        ((> smoothed-tick-drift 1.5)
         (when tick-offset
           (incf tick-offset))
         (setf smoothed-tick-drift 0.0d0)
         (setf ticks-at-current-offset -240))
        ((< smoothed-tick-drift -1.5)
         (when tick-offset
           (decf tick-offset))
         (setf smoothed-tick-drift 0.0d0)
         (setf ticks-at-current-offset -240))))))
