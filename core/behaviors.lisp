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

(defmacro define-behavior-accessor (behavior slot-name &key reader writer)
  (let ((accessor-symbol (intern (format nil "~a-~a" (symbol-name behavior) (symbol-name slot-name)))))
    `(progn
       (defun ,accessor-symbol (&optional (behavior (or *behavior*
                                                     (entity-find-behavior *entity* ',behavior))))
         ,(if reader
             `(funcall reader (slot-value behavior ',slot-name))
             `(slot-value behavior ',slot-name)))
       (defun (setf ,accessor-symbol) (new-value &optional (behavior (or *behavior*
                                                                      (entity-find-behavior *entity* ',behavior))))
         (setf (slot-value behavior ',slot-name) ,(if writer
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

(defgeneric behavior-encode (behavior))
(defgeneric behavior-decode (behavior-symbol behavior-vector entity))

(defmacro defbehavior (name slots &body sections)
  (let (methods
        clos-slots
        slot-symbols
        (slot-readers (make-hash-table))
        (slot-writers (make-hash-table)))
    (loop for slot in slots
          do (if (symbolp slot)
                 (progn (push slot slot-symbols)
                        (push (list slot
                                    :initarg (intern (symbol-name slot) :keyword))
                              clos-slots))
                 (destructuring-bind (symbol &key init writer reader) slot
                   (push symbol
                         slot-symbols)
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
       (defmethod behavior-encode ((behavior ,name))
         (let ((behavior-data (apply #'concatenate
                                     '(vector (unsigned-byte 8))
                                     (mapcar
                                      #'slither/serialization:encode-argument
                                      (list ,@(loop for slot in slot-symbols
                                                    collect `(slot-value behavior ',slot)))))))
           (concatenate
            '(vector (unsigned-byte 8))
            (let ((data-length (length behavior-data)))
              (vector (ldb (byte 8 8) data-length)
                      (ldb (byte 8 0) data-length)))
            behavior-data)))
       (defmethod behavior-decode ((behavior-symbol (eql ',name)) behavior-vector entity)
         (let* ((behavior-size (vector-read-integer behavior-vector :bytes 2))
                (arguments (slither/serialization:decode-arguments (subseq behavior-vector 2 (+ 2 behavior-size)))))
           (declare (ignorable arguments))
           (values
            (make-instance ',name
                           :entity entity
                           ,@(loop for slot-symbol in slot-symbols
                                   for i from 0
                                   append (list (intern (symbol-name slot-symbol) :keyword)
                                                `(elt arguments ,i))))
            (+ 2 behavior-size))))
       ,@methods)))
