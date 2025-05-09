(defpackage #:slither/window
  (:use #:cl 
        #:slither/utils)
  (:import-from :slither/input
                #:key-pressed
                #:key-released
                #:set-mouse-position)
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

(defmethod glfw:mouse-moved ((window game-window) x y)
  (set-mouse-position x y))

(defmethod glfw:key-changed ((window game-window) key scan-code action modifiers)
  (case action
    (:press (key-pressed key (frame)))
    (:release (key-released key))))

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

(defparameter *window* nil)
(defparameter *frame* 0)
(defun frame ()
  *frame*)

(defun fps ()
  (unless (= *dt* 0)
    (/ 1 *dt*)))

(defun open-window ()
  (glfw:init)
  (unless *window*
    (setf *window* (make-instance 'game-window))))

(defun close-window ()
  (glfw:destroy *window*)
  (glfw:shutdown))

(defmacro with-window (&body body)
  `(progn
     (open-window)
     (unwind-protect
          (progn ,@body)
       (close-window))))

(defmacro with-event-loop (&body body)
  `(with-window
       (loop unless (glfw:should-close-p *window*)
             do (progn
                  (incf *frame*)
                  #+micros (let ((*features* `(,@*features* :in-game-loop)))
                               (read-repl))
                  (calculate-dt)
                  (glfw:poll-events)
                  (continuable
                    ,@body)
                  (glfw:swap-buffers *window*)))))
