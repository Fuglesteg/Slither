(defpackage #:slither/input
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:import-from #:slither/window
                #:game-window
                #:*window-width*
                #:*window-height*)
  (:export :key-held-p
           :mouse-position
           :normalized-mouse-position
           :key-pressed
           :key-released
           :set-mouse-position
           :normalized-screen-space-mouse-position))

(in-package #:slither/input)

(defparameter *keys* '())

(defun key-held-p (key)
  (not (null (find key *keys*))))

(defmethod glfw:key-changed ((window game-window) key scan-code action modifiers)
  (case action
    (:press (key-pressed key))
    (:release (key-released key))))

(defmethod glfw:mouse-button-changed ((window game-window) button action modifiers)
  (case action
    (:press (key-pressed button))
    (:release (key-released button))))

(defun key-pressed (key)
  (pushnew key *keys*))

(defun key-released (key)
  (setf *keys* (remove key *keys*)))

(defparameter *mouse-position* (vec2 0 0))

(defun set-mouse-position (x y)
  (vsetf *mouse-position* x y))

(defmethod glfw:mouse-moved ((window game-window) x y)
  (set-mouse-position x y))

(defun mouse-position ()
  *mouse-position*)

(defun normalized-mouse-position ()
  (v/ *mouse-position* (vec2 *window-width* *window-height*)))

(defun normalized-screen-space-mouse-position ()
  (v- (normalized-mouse-position) 0.5))