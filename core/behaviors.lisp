(in-package :slither/core)

(defvar *behavior* nil)

(defclass behavior ()
  ((entity
    :accessor behavior-entity
    :initarg :entity
    :initform (error "Entity is required")
    :type entity)))

(defmethod tick ((behavior behavior)))
(defmethod tick :around ((*behavior* behavior))
  (declare (special *behavior*))
  (call-next-method))

(defmethod start ((behavior behavior)))
(defmethod start :around ((*behavior* behavior))
  (declare (special *behavior*))
  (call-next-method))

(defmacro define-behavior-accessor (behavior slot-name &key reader writer networked)
  (declare (ignore networked))
  (let ((accessor-symbol (intern (format nil "~a-~a" (symbol-name behavior) (symbol-name slot-name))))
        (accessor-form `(slot-value
                         (etypecase behavior
                           (entity (entity-find-behavior behavior ',behavior))
                           (,behavior behavior)
                           (behavior (entity-find-behavior (behavior-entity behavior) ',behavior)))
                         ',slot-name)))
    `(progn
       (defun ,accessor-symbol (&optional (behavior (or *behavior* *entity*)))
            ,(if reader
                `(funcall reader ,accessor-form)
                accessor-form))
       (defun (setf ,accessor-symbol) (new-value &optional (behavior (or *behavior* *entity*)))
           (setf ,accessor-form ,(if writer
                                     `(funcall ,writer new-value)
                                     'new-value))))))

(defmacro define-behavior-method (behavior name method-arguments &body body)
  `(defun ,name ,method-arguments
     (let ((*behavior* (or *behavior*
                           (entity-find-behavior *entity* ',behavior))))
       ,@body)))

(defun behavior-invoke (behavior method &rest arguments)
  (let ((*behavior* behavior)
        (*entity* (behavior-entity behavior)))
    (apply method arguments)))

(defgeneric behavior-networked-p (behavior-symbol)
  (:method ((behavior-symbol t))
    nil))

(defgeneric behavior-networked-slots (behavior-symbol))

(defgeneric behavior-encode (behavior))
(defgeneric behavior-encode-full (behavior))
(defgeneric behavior-decode (behavior-symbol behavior-vector entity))
(defgeneric behavior-decode-full (behavior-symbol behavior-vector entity))

(defmacro defbehavior (name slots &body sections)
  (let (methods
        clos-slots
        slot-symbols
        networked-slots
        (slot-readers (make-hash-table))
        (slot-writers (make-hash-table)))
    (loop for slot in slots
          do (if (symbolp slot)
                 (progn (push slot slot-symbols)
                        (push (list slot
                                    :initarg (intern (symbol-name slot) :keyword))
                              clos-slots))
                 (destructuring-bind (symbol &key init writer reader networked) slot
                   (push symbol
                         slot-symbols)
                   (when networked
                     (push symbol networked-slots))
                   (let ((clos-slot (list symbol :initarg (intern (symbol-name symbol) :keyword))))
                     (when writer
                       (setf (gethash symbol slot-writers) writer))
                     (when reader
                       (setf (gethash symbol slot-readers) reader))
                     (when init
                       (nconc clos-slot
                              `(:initform ,init)))
                     (push clos-slot
                           clos-slots)))))
    (setf slot-symbols (nreverse slot-symbols))
    (setf clos-slots (nreverse clos-slots))
    (loop for (keyword-or-symbol . arguments) in sections
          collect
             (cond
               ((string= keyword-or-symbol :tick)
                (push
                 `(defmethod tick ((,(gensym) ,name))
                    ,@arguments)
                 methods))
               ((string= keyword-or-symbol :start)
                (push
                 `(defmethod start ((,(gensym) ,name))
                    ,@arguments)
                 methods))
               ((string= keyword-or-symbol :required-behaviors)
                (push
                 `(defmethod behavior-required-behaviors ((behavior (eql ',name)))
                    (quote ,arguments))
                 methods))
               ((string= keyword-or-symbol :networked)
                (when (first arguments)
                  (push
                   `(defmethod behavior-networked-p ((behavior (eql ',name)))
                      t)
                   methods)))
               (t (destructuring-bind (method-arguments . body) arguments
                    (push
                     `(define-behavior-method ,name ,(ensure-non-keyword-symbol keyword-or-symbol) ,method-arguments
                        ,@body)
                     methods)))))
    `(progn
       (defclass ,name (behavior) ,clos-slots)
       ,@(loop for slot-symbol in slot-symbols
               for slot-writer = (gethash slot-symbol slot-writers)
               for slot-reader = (gethash slot-symbol slot-readers)
               collect `(define-behavior-accessor ,name ,slot-symbol
                          :reader ,slot-reader
                          :writer ,slot-writer))
       ,@(flet ((behavior-encoder (slots)
                  `(let ((behavior-data (apply #'concatenate
                                               '(vector (unsigned-byte 8))
                                               (mapcar
                                                #'slither/serialization:encode-argument
                                                (list ,@(loop for slot in slots
                                                              collect `(slot-value behavior ',slot)))))))
                     (with-vector-writer (make-array (+ 2 (length behavior-data))
                                                     :element-type '(unsigned-byte 8))
                         (:write-integer behavior-write-integer
                          :write-sequence behavior-write-sequence)
                      (behavior-write-integer (length behavior-data) :bytes 2)
                      (behavior-write-sequence behavior-data))))
                (behavior-decoder (slots)
                  `(let ((arguments (slither/serialization:decode-arguments behavior-vector)))
                     (declare (ignorable arguments))
                     (make-instance ',name
                                    :entity entity
                                    ,@(loop for slot-symbol in slots
                                            for i from 0
                                            append (list (intern (symbol-name slot-symbol) :keyword)
                                                         `(elt arguments ,i)))))))
           `((defmethod behavior-encode-full ((behavior ,name))
               ,(behavior-encoder slot-symbols))
             (defmethod behavior-encode ((behavior ,name))
               ,(behavior-encoder networked-slots))
             (defmethod behavior-decode-full ((behavior-symbol (eql ',name)) behavior-vector entity)
               ,(behavior-decoder slot-symbols))
             (defmethod behavior-decode ((behavior-symbol (eql ',name)) behavior-vector entity)
               ,(behavior-decoder networked-slots))))
       (defmethod behavior-networked-slots ((behavior-symbol (eql ',name)))
         ',networked-slots)
       ,@methods)))
