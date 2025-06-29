(uiop:define-package #:slither/input
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
           :key-press
           :key-release
           :set-mouse-position
           :normalized-screen-space-mouse-position
           :key-pressed-p
           :key-released-p))

(in-package #:slither/input)

(defparameter *keys* '())

(defun (setf key-state) (new-value key)
  (setf (cdr (assoc key *keys*)) new-value))

(defun key-state (key)
  (cdr (assoc key *keys*)))

(defun key-held-p (key)
  (let ((key-state (key-state key)))
    (or (eq key-state :held)
        (eq key-state :pressed))))

(defmethod glfw:key-changed ((window game-window) key scan-code action modifiers)
  (case action
    (:press (key-press key))
    (:release (key-release key))))

(defmethod glfw:mouse-button-changed ((window game-window) button action modifiers)
  (case action
    (:press (key-press button))
    (:release (key-release button))))

(defun key-press (key)
  (push (cons key :pressed) *keys*))

(defun key-pressed-p (key)
  (eq :pressed (key-state key)))

(defun key-release (key)
  (setf (key-state key) :released))

(defun key-released-p (key)
  (eq :released (key-state key)))

(defun input-poll ()
  (setf *keys*
        (mapcar (lambda (key-data)
                  (destructuring-bind (key . state) key-data
                    (if (eq state :pressed)
                        (cons key :held)
                        key-data)))
                (remove-if (lambda (key-data)
                             (destructuring-bind (key . state) key-data
                               (declare (ignore key))
                               (eq state :released)))
                           *keys*))))

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
  (v* (v- (normalized-mouse-position) 0.5) (vec2 1 -1) 2))
