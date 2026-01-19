(uiop:define-package #:slither/input
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:import-from #:slither/window
                #:game-window
                #:*window-width*
                #:*window-height*)
  (:export :input-poll
           :key-held-p
           :mouse-position
           :normalized-mouse-position
           :key-press
           :key-release
           :set-mouse-position
           :normalized-screen-space-mouse-position
           :key-pressed-p
           :key-released-p
           :set-input-table
           :encode-inputs
           :decode-inputs))

(in-package #:slither/input)

(defparameter *inputs* '())

(defun (setf key-state) (new-value key)
  (setf (cdr (assoc key *inputs*)) new-value))

(defun key-state (key)
  (cdr (assoc key *inputs*)))

(defun key-held-p (key)
  (let ((key-state (key-state key)))
    (or (eq key-state :held)
        (eq key-state :pressed))))

(defmethod glfw:key-changed ((window game-window) key scan-code action modifiers)
  (case action
    (:press (key-press key))
    (:release (key-release key))))

(defmethod glfw:mouse-button-changed ((window game-window) button action modifiers)
  (let ((button (case button
                  (:left :left-click)
                  (:right :right-click)
                  (:middle :middle-click))))
  (case action
    (:press (key-press button))
    (:release (key-release button)))))

(defun key-press (key)
  (push (cons key :pressed) *inputs*))

(defun key-pressed-p (key)
  (eq :pressed (key-state key)))

(defun key-release (key)
  (setf (key-state key) :released))

(defun key-released-p (key)
  (eq :released (key-state key)))

(defun input-poll ()
  (setf *inputs*
        (mapcar (lambda (key-data)
                  (destructuring-bind (key . state) key-data
                    (if (eq state :pressed)
                        (cons key :held)
                        key-data)))
                (remove-if (lambda (key-data)
                             (destructuring-bind (key . state) key-data
                               (declare (ignore key))
                               (eq state :released)))
                           *inputs*))))

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

(defvar *input-code-table* (make-hash-table :test 'eq))
(defvar *code-input-table* (make-hash-table :test 'eq))

(defun set-input-table (&rest inputs)
  (clrhash *input-code-table*)
  (clrhash *code-input-table*)
  (loop for input in inputs
        for i from 0
        do (setf (gethash input *input-code-table*) i)
           (setf (gethash i *code-input-table*) input)))

(set-input-table :w :a :s :d
                 :space :tab
                 :left :right)

(defun encode-inputs (inputs)
  (let ((encoded 0))
    (loop for (input . status) in inputs
          do (alexandria:when-let ((input-code (gethash input *input-code-table*)))
               (setf encoded
                     (dpb 1 (byte 1 input-code) encoded))))
    encoded))

(defun decode-inputs (inputs)
  (loop for i from 0 to 32
        when (= 1 (ldb (byte 1 i) inputs))
        collect (gethash i *code-input-table*)))
