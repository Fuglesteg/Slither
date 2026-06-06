(uiop:define-package :slither/networking/client-prediction
  (:use :cl
        :slither/core)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :server-tick
           :ticks-to-predict
           :client-prediction-tick-rate-flush
           :estimated-tick-offset
           :register-round-trip-time
           :reset-client-prediction))

(in-package :slither/networking/client-prediction)

(defvar *round-trip-time*
  0.0d0
  "RTT measures how long a packet takes to reach the server and return to the client")

;; ---- Server tick accessor ----
(let ((server-tick 0))
  (defun (setf server-tick) (new-value)
    (setf server-tick new-value))
  (defun server-tick ()
    server-tick))

(defun estimated-tick-offset ()
  "Conservative estimate of how many ticks the client should predict ahead
   based on half the current round-trip time."
  (max 1 (round (/ (/ *round-trip-time* 2.0d0) (tick-delta)))))

(defun tick-drift ()
  "Difference between the client's current tick and where we think
   the server's authoritative tick is, after subtracting our prediction offset."
  (- (current-tick) (server-tick) (ticks-to-predict)))

;; ---- Prediction state ----
(let ((tick-offset 0)                ; ← was NIL – now starts at 0
      (smoothed-tick-drift 0.0d0)
      (ticks-at-current-offset 0))

  (defun reset-client-prediction ()
    (setf tick-offset 0)             ; reset to a valid number
    (setf smoothed-tick-drift 0.0d0)
    (setf ticks-at-current-offset 0))

  (defun register-round-trip-time (sent-at)
    (let ((raw-rtt (- (glfw:time) sent-at)))
      (cond
        ((= *round-trip-time* 0.0d0)
         (setf *round-trip-time* raw-rtt)
         (setf tick-offset (estimated-tick-offset)))
        (t (incf *round-trip-time* (* (- raw-rtt *round-trip-time*) 0.1))))))

  (defun ticks-to-predict ()
    tick-offset)                     ; now always a number

  (defun client-prediction-tick-rate-flush ()
    ;; Smooth the instantaneous drift
    (incf smoothed-tick-drift (* (- (tick-drift) smoothed-tick-drift) 0.1))

    ;; Fine‑grained adjustment: tweak tick delta if drift is large
    (cond
      ((> smoothed-tick-drift 0.5)
       (setf (tick-delta) (* slither/core::*base-tick-delta* 1.02)))
      ((< smoothed-tick-drift -0.5)
       (setf (tick-delta) (* slither/core::*base-tick-delta* 0.98)))
      (t
       (setf (tick-delta) slither/core::*base-tick-delta*)))

    ;; Coarse adjustment: after 60 ticks at the current offset, if drift is still
    ;; significant, shift the whole prediction window by one tick.
    (incf ticks-at-current-offset)
    (when (> ticks-at-current-offset 60)
      ;; Decide whether to step the offset; then always reset the counter
      ;; so we wait another 60 ticks before the next possible adjustment.
      (cond
        ((> smoothed-tick-drift 0.3)
         (incf tick-offset)
         (setf smoothed-tick-drift 0.0d0))
        ((< smoothed-tick-drift -0.3)
         ;; Prevent offset from going negative, mimicking the guard that was
         ;; previously broken.
         (when (plusp tick-offset)
           (decf tick-offset))
         (setf smoothed-tick-drift 0.0d0)))
      (setf ticks-at-current-offset 0))))
