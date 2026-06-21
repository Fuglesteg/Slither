(uiop:define-package #:slither/input
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:slither/utils
        #:slither/core)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:import-from #:slither/window
                #:game-window
                #:*window-width*
                #:*window-height*)
  (:export :inputs-update
           :inputs-tick
           :input-history-record
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
           :decode-inputs
           :mouse-scroll-x
           :mouse-scroll-y
           :definput
           :define-vector-input
           :input-released-p
           :input-held-p
           :input-pressed-p
           :held-p
           :released-p
           :pressed-p
           :vector-input-value
           :button-input-value
           :copy-inputs
           :vector-value))

(in-package #:slither/input)

(defvar *keys* (make-hash-table :test 'eq
                                :size 128))

(defun (setf key-state) (new-value key)
  (setf (gethash key *keys*) new-value))

(defun key-state (key)
  (gethash key *keys*))

(defun key-press (key)
  (setf (key-state key) :pressed))

(defun key-pressed-p (key)
  (eq :pressed (key-state key)))

(defun key-release (key)
  (setf (key-state key) :released))

(defun key-released-p (key)
  (eq :released (key-state key)))

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

(let ((scrollx 0.0))
  (defun (setf mouse-scroll-x) (new-value)
    (setf scrollx new-value))
  (defun mouse-scroll-x ()
    scrollx))

(let ((scrolly 0.0))
  (defun (setf mouse-scroll-y) (new-value)
    (setf scrolly new-value))
  (defun mouse-scroll-y ()
    scrolly))

(defmethod glfw:mouse-scrolled ((window game-window) xoffset yoffset)
  (setf (mouse-scroll-x) xoffset)
  (setf (mouse-scroll-y) yoffset))

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

(defclass button-input ()
  ((bindings
    :initarg :bindings
    :initform nil
    :accessor button-input-bindings)
   (value
    :initarg :value
    :initform nil
    :accessor button-input-value)))

(defclass vector-input ()
  ((polling-function
    :initarg :polling-function
    :accessor vector-input-polling-function)
   (up
    :initarg :up
    :accessor vector-input-up)
   (down
    :initarg :down
    :accessor vector-input-down)
   (left
    :initarg :left
    :accessor vector-input-left)
   (right
    :initarg :right
    :accessor vector-input-right)
   (value
    :initform (vec2)
    :initarg :value
    :type vec2
    :accessor vector-input-value)))

(defmacro vector-value (input)
  `(vector-input-value (input ',input)))

(defvar *input-code-table* (make-hash-table :test 'eq))
(defvar *code-input-table* (make-hash-table :test 'eq))

(defun register-input-button-code (symbol)
  (unless (gethash symbol *input-code-table*)
    (loop for i from 0 below 32
        until (not (gethash i *code-input-table*))
        finally (setf (gethash i *code-input-table*) symbol)
                (setf (gethash symbol *input-code-table*) i))))

(defun register-input-vector-code (symbol)
  (unless (gethash symbol *input-code-table*)
    (loop for i from 32 below 64
          until (not (gethash i *code-input-table*))
          finally (setf (gethash i *code-input-table*) symbol)
                  (setf (gethash symbol *input-code-table*) i))))


(defvar *inputs* (make-hash-table :test 'eq
                                  :size 32))

(defun input (input-symbol)
  (gethash input-symbol *inputs*))

(defun (setf input) (new-value input)
  (setf (gethash input *inputs*) new-value))

(defun register-button-input (name bindings)
  (register-input-button-code name)
  (setf (input name) (make-instance 'button-input :bindings bindings)))

(defmacro definput (name &key default-bindings)
  `(defparameter ,name (register-button-input ',name ',default-bindings)))

(defun input-pressed-p (input)
  (eq (button-input-value input)
      :pressed))

(defmacro pressed-p (input)
  `(input-pressed-p (input ',input)))

(defun input-held-p (input)
  (let ((input-state (button-input-value input)))
    (or (eq input-state :held)
        (eq input-state :pressed))))

(defmacro held-p (input)
  `(input-held-p (input ',input)))

(defun input-released-p (input)
  (eq (button-input-value input)
      :released))

(defmacro released-p (input)
  `(input-released-p (input ',input)))

(defun inputs-update ()
  "Read raw key states and update *INPUTS* for this tick. Does NOT transition press/release states."
  (do-hash-table (input-name input *inputs*)
    (declare (ignore input-name))
    (etypecase input
      (button-input
       (setf (button-input-value input)
             (let ((bindings (button-input-bindings input)))
               (case (length bindings)
                 (0 nil)
                 (1 (key-state (first bindings)))
                 (t (key-state (find-if (lambda (binding) (key-state binding))
                                        (button-input-bindings input))))))))
       (vector-input
        (let ((up (key-held-p (vector-input-up input)))
              (down (key-held-p (vector-input-down input)))
              (right (key-held-p (vector-input-right input)))
              (left (key-held-p (vector-input-left input))))
          (cond ((or up down left right)
                (let ((x 0) (y 0))
                  (when up (incf y))
                  (when down (decf y))
                  (when left (decf x))
                  (when right (incf x))
                  (vsetf (vector-input-value input) x y)))
               ((vector-input-polling-function input)
                (setf (vector-input-value input)
                      (funcall (vector-input-polling-function input))))
               (t (vsetf (vector-input-value input) 0.0 0.0))))))))

(defun inputs-tick ()
  "Advance key states states to transition pressed to held and released to nil.
Also resets scroll. Should be called after entity updates"
  (setf (mouse-scroll-x) 0.0)
  (setf (mouse-scroll-y) 0.0)
  (do-hash-table (key state *keys*)
    (case state
      (:released (setf (key-state key) nil))
      (:pressed  (setf (key-state key) :held)))))

(defun register-vector-input (name &key up down left right polling-function)
  (register-input-vector-code name)
  (setf (input name) (make-instance 'vector-input
                                    :up up
                                    :down down
                                    :left left
                                    :right right
                                    :polling-function polling-function)))

(defmacro define-vector-input (name &key up down left right poll)
  `(defparameter ,name (register-vector-input ',name
                                              :up ,up
                                              :down ,down
                                              :left ,left
                                              :right ,right
                                              :polling-function ,(if poll
                                                                     `(lambda () ,poll)
                                                                     nil))))

(defun encode-inputs (inputs)
  (let ((buttons 0)
        analogues)
    (do-hash-table (input-name input inputs)
      (etypecase input
        (button-input
         (when (input-held-p input)
           (alexandria:when-let ((input-code (gethash input-name *input-code-table*)))
             (setf buttons
                   (dpb 1 (byte 1 input-code) buttons)))))
        (vector-input (push (cons (gethash input-name *input-code-table*)
                                  (encode-argument (vector-input-value input)))
                            analogues))))
    (with-vector-writer (make-octet-vector (+ 4 ; Buttons size in bytes
                                              (loop for (analogue-input-code . analogue-data) in analogues
                                                    sum (+ 1 ; Input code
                                                           (length analogue-data)))))
        (:write-integer input-write-byte
         :write-sequence input-write-sequence)
      (input-write-byte buttons :bytes 4)
      (loop for (analogue-input-code . analogue-data) in analogues
            do (input-write-byte analogue-input-code :bytes 1)
               (input-write-sequence analogue-data)))))

(defun decode-inputs (inputs)
  (with-vector-reader inputs
      (:read-integer inputs-read-integer
       :read-argument inputs-read-argument
       :remaining-bytes inputs-remaining-bytes)
    (let ((buttons (let ((buttons-bitfield-integer (inputs-read-integer 4)))
                     (loop for i from 0 below 32
                           when (= 1 (ldb (byte 1 i) buttons-bitfield-integer))
                           collect (gethash i *code-input-table*))))
          (analogues (loop while (< 0 (inputs-remaining-bytes))
                           collect (cons (gethash (inputs-read-integer 1) *code-input-table*)
                                         (inputs-read-argument)))))
      (values buttons analogues))))

(defun apply-decoded-inputs (buttons analogues)
  (do-hash-table (input-name input *inputs*)
        do (etypecase input
             (button-input (if (find input-name buttons)
                             (unless (input-held-p input)
                               (setf (button-input-value input) :pressed))
                             (when (input-held-p input)
                               (setf (button-input-value input) :released))))
             (vector-input (when-let ((analogue (assoc-value analogues input-name)))
                             (setf (vector-input-value input) (vcopy analogue)))))))

(defun copy-inputs (inputs)
  (let ((copy (make-hash-table :test 'eq
                               :size 32)))
    (do-hash-table (input-name input inputs)
      (setf (gethash input-name copy)
            (etypecase input
              (button-input (make-instance 'button-input :value (button-input-value input)))
              (vector-input (make-instance 'vector-input :value (vcopy (vector-input-value input)))))))
    copy))

(defvar *input-history*
  (make-hash-table :test 'eq
                   :size 32))

(defconstant +input-history-size+ 30)

(defun input-history-record ()
  (do-hash-table (input-name input *inputs*)
    (let ((input-history-vector (gethash input-name *input-history*)))
      (unless input-history-vector
        (setf input-history-vector
              (make-array +input-history-size+))
        (setf (gethash input-name *input-history*) input-history-vector))
      (replace input-history-vector input-history-vector
               :start1 1)
      (setf (aref input-history-vector 0)
            (etypecase input
              (button-input (button-input-value input))
              (vector-input (vcopy (vector-input-value input))))))))

(defun input-history-shift (amount)
  (unless (= amount 0)
    (do-hash-table (input-name input-history-vector *input-history*)
      (declare (ignore input-name))
      (when input-history-vector
        (cond
          ((< +input-history-size+ (abs amount))
           (fill input-history-vector nil))
          ((< 0 amount)
            (replace input-history-vector input-history-vector
                     :start1 amount)
            (fill input-history-vector nil :end amount))
          (t
            (let ((shift-amount (- amount)))
              (replace input-history-vector input-history-vector
                       :start2 shift-amount)
              (fill input-history-vector nil
                    :start (- (length input-history-vector) shift-amount)))))))))

(defun input-history-reset-to-tick (tick)
  (input-history-shift (- tick (current-tick))))

(defun input-history-apply (inputs tick)
  (let ((tick-index (- (current-tick) tick)))
    (when (< 0 tick-index +input-history-size+)
      (do-hash-table (input-name input inputs)
        (let ((historic-value (aref (gethash input-name *input-history*)
                                    tick-index)))
          (etypecase input
            (button-input (setf (button-input-value input) historic-value))
            (vector-input (let ((old (vcopy (vector-input-value input))))
                            (when historic-value
                              (setf (vector-input-value input) (vcopy historic-value)))))))))))
