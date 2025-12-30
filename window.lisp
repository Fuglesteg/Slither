(defpackage #:slither/window
  (:use #:cl
        #:slither/utils)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export #:frame
           #:*dt*
           #:*window-width*
           #:*window-height*
           #:fps
           #:with-event-loop
           #:with-window))

(in-package #:slither/window)

(defclass game-window (glfw:window) ()
  (:default-initargs
   :width 250 :height 250
   :title "Slither"))

(defvar *window-height* nil)
(defvar *window-width* nil)

(defmethod glfw:window-resized ((window game-window) width height)
  (setf *window-width* width
        *window-height* height)
  (gl:viewport 0 0 width height))

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

(defvar *window* nil)

(defun fps ()
  (unless (= *dt* 0)
    (/ 1 *dt*)))

(defun open-window (&rest initargs)
  (glfw:init)
  (unless *window*
    (let ((window (apply #'make-instance 'game-window initargs)))
      (destructuring-bind (width height) (glfw:framebuffer-size window)
        (setf *window* window
              *window-width* width
              *window-height* height)))))

(defun close-window ()
  (glfw:destroy *window*)
  (setf *window* nil)
  (glfw:shutdown))

(defmacro with-window (initargs &body body)
  `(progn
     (apply #'open-window ,initargs)
     (unwind-protect
          (progn ,@body)
       (close-window))))

(defmacro with-event-loop (&body body)
  `(loop until (glfw:should-close-p *window*)
         do (progn
              #+micros (read-repl)
              (calculate-dt)
              (glfw:poll-events)
              (continuable
                ,@body)
              (glfw:swap-buffers *window*))))
