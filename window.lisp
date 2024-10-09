(defpackage #:slither/window
  (:use #:cl 
        #:slither/utils)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :with-game-loop))

(in-package #:slither/window)

(defclass game-window (glfw:window) ()
  (:default-initargs :refresh-rate 144
   :width 250 :height 250
   :title "Slither"))

(defparameter *window* nil)

(defvar *dt* 0)
(defvar *last-frame-time* 0)

(defun calculate-dt ()
  (let ((current-time (glfw:time)))
    (setf *dt* (- current-time *last-frame-time*)
          *last-frame-time* current-time)))

(defmacro with-game-loop (&body body)
  `(progn 
     (glfw:init)
     (setf *window* (make-instance 'game-window))
     (loop unless (glfw:should-close-p *window*)
              #+micros (read-repl)
              (calculate-dt)
              (glfw:poll-events)
              ,@body
              (glfw:swap-buffers))
     (glfw:destroy *window*)
     (glfw:shutdown)))

#+micros
(defun read-repl ()
  (continuable
   (let ((connection (or micros/swank-api:*emacs-connection* 
                         (micros::default-connection))))
     (when connection
       (micros::handle-requests connection t)))))
