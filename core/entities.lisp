(in-package #:slither/core)

(defvar *entity* nil)

(defclass entity ()
  ((behaviors
    :accessor entity-behaviors
    :initarg :behaviors)))

(defgeneric entity-make-default-behaviors (entity))
(defgeneric entity-initialize-behaviors (entity))
(defmethod entity-initialize-behaviors ((entity entity))
  (setf (entity-behaviors entity) (entity-make-default-behaviors entity)))

(defmethod initialize-instance :after ((entity entity) &key)
  (entity-initialize-behaviors entity))

(defgeneric tick (entity))
(defmethod tick ((entity entity)))

(defmethod tick :around ((entity entity))
  (let ((*entity* entity))
    (call-next-method)))

(defmethod tick :before ((entity entity))
  (loop for behavior in (entity-behaviors entity)
        do (tick behavior)))

(defgeneric start (entity))

(defmethod start :around ((entity entity))
  (let ((*entity* entity))
    (call-next-method)))

(defmethod start :before ((entity entity))
  (loop for behavior in (entity-behaviors entity)
        do (start behavior)))

(defmethod start ((entity entity)))

(defmethod entity-find-behavior ((entity entity) (behavior symbol))
  (find behavior (entity-behaviors entity)
        :key #'type-of))

(defgeneric behavior-required-behaviors (behavior))
(defmethod behavior-required-behaviors ((behavior t)))

(defgeneric entity-encode (entity))
(defgeneric entity-decode (entity-type-id entity-vector))

(defvar *entity-type-id-counter* 1)
(defvar *entity-type-id-table* (make-hash-table :test 'eq))

(defun find-entity-type-by-id (id)
  (gethash id *entity-type-id-table*))

(defmacro define-entity-accessor (entity slot-name)
  (let ((accessor-symbol (intern (format nil "~a-~a" (symbol-name entity) (symbol-name slot-name)))))
  `(progn
     (defun ,accessor-symbol (&optional (entity *entity*))
       (slot-value entity ',slot-name))
     (defun (setf ,accessor-symbol) (new-value &optional (entity *entity*))
       (setf (slot-value entity ',slot-name) new-value)))))

(defmacro define-entity-method (entity name method-arguments &body body)
  (declare (ignore entity))
  `(defun ,(ensure-non-keyword-symbol name) ,method-arguments
     ,@body))

(defun entity-invoke (entity method &rest arguments)
  (let ((*entity* entity))
    (apply method arguments)))

(defmacro defentity (name slots &body sections)
  (let ((entity-type-id
          (or (loop for key being the hash-keys of *entity-type-id-table*
                using (hash-value value)
                when (eq value name)
                return key)
              (incf *entity-type-id-counter*))))
    (setf (gethash entity-type-id *entity-type-id-table*) name)
  (let (slot-symbols clos-slots)
    (loop for slot in slots
          do (if (symbolp slot)
                 (progn (push slot slot-symbols)
                        (list slot
                              :initarg (intern (symbol-name slot) :keyword)))
                 (destructuring-bind (symbol &optional default-value) slot
                   (push symbol
                         slot-symbols)
                   (push (list symbol
                               :initform default-value
                               :initarg (intern (symbol-name symbol) :keyword))
                         clos-slots))))
    (setf slot-symbols (nreverse slot-symbols))
    (setf clos-slots (nreverse clos-slots))
    (let (behavior-symbols methods)
      (loop for (keyword . arguments) in sections
            collect (cond
                      ((string= keyword :behaviors)
                       (setf behavior-symbols (mapcar (lambda (behavior)
                                                        (etypecase behavior
                                                          (symbol behavior)
                                                          (cons (car behavior))))
                                                      arguments))
                       (loop for behavior in behavior-symbols
                             do (loop for required-behavior in (behavior-required-behaviors behavior)
                                      do (unless (member required-behavior behavior-symbols)
                                           (error "Behavior ~a, required by ~a not found in behavior list"
                                                  required-behavior
                                                  behavior))))
                       (push
                        (let ((entity-symbol (gensym)))
                          `(defmethod entity-make-default-behaviors ((,entity-symbol ,name))
                             (list ,@(loop for behavior in arguments
                                           collect (etypecase behavior
                                                     (symbol `(make-instance ',behavior :entity ,entity-symbol))
                                                     (cons `(make-instance ,@(cons (list 'quote (car behavior)) (cdr behavior))
                                                                           :entity ,entity-symbol)))))))
                        methods))
                      ((string= keyword :tick)
                       (push
                        (let ((entity-symbol (gensym)))
                          `(defmethod tick ((,entity-symbol ,name))
                             ,@arguments))
                        methods))
                      ((string= keyword :start)
                       (push
                        (let ((entity-symbol (gensym)))
                          `(defmethod start ((,entity-symbol ,name))
                             ,@arguments))
                        methods))
                      (t
                       (push
                        (destructuring-bind (method-arguments . body) arguments
                          `(define-entity-method ,name ,(ensure-non-keyword-symbol keyword) ,method-arguments
                               ,@body))
                        methods))))
      `(progn
         (defclass ,name (entity)
           ,clos-slots)
         ,@(loop for slot-symbol in slot-symbols
                 collect `(define-entity-accessor ,name ,slot-symbol))
         (defmethod entity-encode ((entity ,name))
           (let ((entity-data (concatenate '(vector (unsigned-byte 8))
                                           ,@(loop for slot in slot-symbols
                                                         collect `(slither/serialization:encode-argument
                                                                   (slot-value entity ',slot))))))
           (concatenate
            '(vector (unsigned-byte 8))
            (vector ,entity-type-id)
            (let ((data-length (length entity-data)))
              (vector (ldb (byte 8 8) data-length)
                      (ldb (byte 8 0) data-length)))
            entity-data
            ,@(loop for behavior-symbol in behavior-symbols
                    collect `(behavior-encode (entity-find-behavior entity ',behavior-symbol))))))
         (defmethod entity-decode ((entity-type-id (eql ,entity-type-id)) entity-vector)
           (let* ((entity-size (vector-read-integer entity-vector :bytes 2))
                  (arguments (when (< 0 entity-size)
                               (slither/serialization:decode-arguments (subseq entity-vector 2 (+ 2 entity-size)))))
                  (index (+ 2 entity-size))
                  (entity (make-instance
                           ',(find-entity-type-by-id entity-type-id)
                           ,@(loop for slot-symbol in slot-symbols
                                   for i from 0
                                   append (list (intern (symbol-name slot-symbol) :keyword)
                                                `(elt arguments ,i))))))
             (declare (ignorable arguments))
             (setf (entity-behaviors entity)
                   (list ,@(loop for behavior-symbol in behavior-symbols
                                 collect `(multiple-value-bind (behavior behavior-size)
                                              (behavior-decode ',behavior-symbol (subseq entity-vector index) entity)
                                            (incf index behavior-size)
                                            behavior))))
             entity))
         ,@methods)))))

(defmacro with-behaviors (behaviors entity &body body)
  (let (behavior-binds slot-binds)
    (loop for behavior in behaviors
          do (etypecase behavior
               (symbol (push `(,behavior (entity-find-behavior ,entity ',behavior)) behavior-binds))
               (cons (destructuring-bind (slots behavior) behavior
                       (etypecase behavior
                         (symbol (push `(,behavior (entity-find-behavior ,entity ',behavior)) behavior-binds))
                         (cons (destructuring-bind (behavior-binding behavior-symbol) behavior
                                 (push `(,behavior-binding (entity-find-behavior ,entity ',behavior-symbol)) behavior-binds))))
                       (let ((behavior (etypecase behavior
                                         (symbol behavior)
                                         (cons (car behavior)))))
                         (loop for slot in slots
                               do (etypecase slot
                                    (symbol (push `(,slot (slot-value ,behavior ',slot)) slot-binds))
                                    (cons (push (destructuring-bind (slot-binding slot-symbol) slot
                                                  `(,slot-binding (slot-value ,behavior ',slot-symbol))) slot-binds)))))))))
    `(let* (,@behavior-binds
           ,@slot-binds)
       ,@body)))
