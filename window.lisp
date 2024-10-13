(defpackage #:slither/window
  (:use #:cl 
        #:slither/utils)
  (:import-from #:slither/input
                :key-pressed
                :key-released
                :set-mouse-position)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :with-game-loop
           :frame))

(in-package #:slither/window)

(defclass game-window (glfw:window) ()
  (:default-initargs
   :width 250 :height 250
   :title "Slither"))

(defmethod mouse-moved ((window game-window) x y)
  (set-mouse-position x y))

(defmethod key-changed ((window game-window) key action)
  (case action
    (:press (key-pressed key (frame)))
    (:release (key-released key))))

(defvar *dt* 0)
(defvar *last-frame-time* 0)

(defun calculate-dt ()
  (let ((current-time (glfw:time)))
    (setf *dt* (- current-time *last-frame-time*)
          *last-frame-time* current-time)))

#+micros
(defun read-repl ()
  (continuable
    (let ((connection (or micros/swank-api:*emacs-connection* 
                          (micros::default-connection))))
      (when connection
        (micros::handle-requests connection t)))))

(defparameter *window* nil)
(defparameter *frame* 0)
(defun frame ()
  *frame*)

(defmacro with-game-loop (&body body)
  `(progn
     (glfw:init)
     (setf *window* (make-instance 'game-window))
     (loop unless (glfw:should-close-p *window*)
           do (progn
                (incf *frame*)
                #+micros (read-repl)
                (calculate-dt)
                (glfw:poll-events)
                ,@body
                (glfw:swap-buffers *window*)))
     (glfw:destroy *window*)
     (glfw:shutdown)))
