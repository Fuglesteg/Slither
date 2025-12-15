(in-package #:slither/core)

(defvar *entity* nil)

(defclass entity ()
  ((behaviors
    :accessor entity-behaviors
    :initarg :behaviors)))

(defgeneric entity-make-default-behaviors (entity))
(defgeneric entity-make-default-behavior (entity behavior-symbol))
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
(defgeneric entity-encode-full (entity))
(defgeneric entity-decode (entity-type-id entity-vector))
(defgeneric entity-decode-full (entity-type-id entity-vector))

(defvar *entity-type-id-counter* 1)
(defvar *entity-type-id-table* (make-hash-table :test 'eq))

(defun find-entity-type-by-id (id)
  (gethash id *entity-type-id-table*))

(defgeneric entity-type-id (entity))

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

(defgeneric entity-networked-slots (entity))
(defgeneric entity-networked-slots-with-behaviors (entity))

(defgeneric entity-find-networked-slot-id (entity slot-symbol &optional behavior))
(defgeneric entity-find-networked-slot-symbol (entity slot-id))

(defmacro defentity (name slots &body sections)
  (let ((entity-type-id
          (or (loop for key being the hash-keys of *entity-type-id-table*
                    using (hash-value value)
                    when (eq value name)
                    return key)
              (incf *entity-type-id-counter*))))
    (setf (gethash entity-type-id *entity-type-id-table*) name)
    (let (slot-symbols clos-slots networked-slots)
      (loop for slot in slots
            do (if (symbolp slot)
                   (progn (push slot slot-symbols)
                          (list slot
                                :initarg (intern (symbol-name slot) :keyword)))
                   (destructuring-bind (symbol &key init networked) slot
                     (when networked
                       (push symbol networked-slots))
                     (push symbol
                           slot-symbols)
                     (push (list symbol
                                 :initform init
                                 :initarg (intern (symbol-name symbol) :keyword))
                           clos-slots))))
      (setf slot-symbols (nreverse slot-symbols))
      (setf clos-slots (nreverse clos-slots))
      (let (behavior-symbols methods)
        (loop for (keyword . arguments) in sections
              collect (cond
                        ((string= keyword :behaviors)
                         (flet ((behavior-symbol (behavior)
                                  (etypecase behavior
                                    (symbol behavior)
                                    (cons (car behavior))))
                                (behavior-constructor (behavior entity)
                                  (etypecase behavior
                                    (symbol `(make-instance ',behavior :entity ,entity))
                                    (cons `(make-instance ,@(cons (list 'quote (car behavior)) (cdr behavior))
                                                          :entity ,entity)))))

                         (setf behavior-symbols (mapcar #'behavior-symbol
                                                        arguments))
                         (loop for behavior in behavior-symbols
                               do (loop for required-behavior in (behavior-required-behaviors behavior)
                                        do (unless (member required-behavior behavior-symbols)
                                             (error "Behavior ~a, required by ~a not found in behavior list"
                                                    required-behavior
                                                    behavior))))
                         (let ((entity-symbol (gensym)))
                           (loop for behavior in arguments
                               do (push `(defmethod entity-make-default-behavior ((,entity-symbol ,name)
                                                                                  (behavior-symbol (eql ',(behavior-symbol behavior))))
                                           ,(behavior-constructor behavior entity-symbol))
                                        methods))
                           (push `(defmethod entity-make-default-behaviors ((,entity-symbol ,name))
                                    (list ,@(loop for behavior in arguments
                                                  collect (behavior-constructor behavior entity-symbol))))
                                 methods))))
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
        (let ((networked-behavior-symbols (and behavior-symbols
                                               (remove-if-not #'behavior-networked-p behavior-symbols))))
        `(progn
           (defclass ,name (entity)
             ,clos-slots)
           ,@(loop for slot-symbol in slot-symbols
                   collect `(define-entity-accessor ,name ,slot-symbol))
           (defmethod entity-type-id ((entity ,name))
             ,entity-type-id)
           (defmethod entity-networked-slots ((entity-symbol (eql ',name)))
             ,networked-slots)
           ,@(let ((entity-and-behaviors-networked-slots
                     (let ((result nil))
                            (loop for networked-slot in networked-slots
                                  do (push (list networked-slot
                                                 nil)
                                           result))
                            (loop for behavior in networked-behavior-symbols
                                  append (loop for networked-slot in (behavior-networked-slots behavior)
                                               do (push (list networked-slot behavior)
                                                        result)))
                       result)))
               `((defmethod entity-find-networked-slot-symbol ((entity ,name) slot-id)
                   (ecase slot-id
                     ,@(loop for (networked-slot behavior) in entity-and-behaviors-networked-slots
                             for i from 0
                             collect `(,i (values ',networked-slot
                                                  ',behavior)))))
                 (defmethod entity-find-networked-slot-id ((entity ,name) slot-symbol &optional behavior)
                   (declare (ignorable behavior))
                   (cond
                     ,@(loop for (networked-slot slot-behavior) in entity-and-behaviors-networked-slots
                             for i from 0
                             collect `(,(if slot-behavior
                                            `(and (eq slot-symbol ',networked-slot)
                                                  (eq behavior ',slot-behavior))
                                            `(eq slot-symbol ',networked-slot))
                                       ,i))))
                 (defmethod entity-networked-slots-with-behaviors ((entity ,name))
                   ',entity-and-behaviors-networked-slots)))
           ,@(flet ((entity-encoder (slots behaviors)
                          `(let* ((entity-data (concatenate '(vector (unsigned-byte 8))
                                                           ,@(loop for slot in slots
                                                                   collect `(slither/serialization:encode-argument
                                                                             (slot-value entity ',slot)))))
                                 (behavior-data (concatenate '(vector (unsigned-byte 8))
                                                             ,@(loop for behavior in behaviors
                                                                   collect `(behavior-encode
                                                                            (entity-find-behavior entity ',behavior))))))
                             (with-vector-writer (make-array (+ 2
                                                                (length entity-data)
                                                                (length behavior-data))
                                                             :element-type '(unsigned-byte 8))
                                 (:write-integer write-integer
                                  :write-sequence entity-write-sequence)
                               (write-integer (length entity-data) :bytes 2)
                               (entity-write-sequence entity-data)
                               (entity-write-sequence behavior-data))))
                        (entity-decoder (slots behaviors)
                          `(with-vector-reader entity-vector (:read-integer read-integer
                                                              :read-sequence entity-vector-read-sequence)
                             (let* ((entity-size (read-integer 2))
                                    (arguments (when (< 0 entity-size)
                                                 (slither/serialization:decode-arguments
                                                  (entity-vector-read-sequence entity-size))))
                                    (entity (make-instance
                                             ',(find-entity-type-by-id entity-type-id)
                                             ,@(loop for slot-symbol in slots
                                                     for i from 0
                                                     append (list (intern (symbol-name slot-symbol) :keyword)
                                                                  `(elt arguments ,i))))))
                               (declare (ignorable arguments))
                               (setf (entity-behaviors entity)
                                     (list ,@(loop for behavior-symbol in behaviors
                                                   collect `(behavior-decode ',behavior-symbol
                                                                                 (entity-vector-read-sequence (read-integer 2))
                                                                                 entity))))
                               entity))))
               (let ((non-networked-behaviors (remove-if
                                               (lambda (behavior)
                                                 (member behavior networked-behavior-symbols))
                                               behavior-symbols)))
               `((defmethod entity-encode-full ((entity ,name))
                   ,(entity-encoder slot-symbols behavior-symbols))
                 (defmethod entity-encode ((entity ,name))
                   ,(entity-encoder networked-slots networked-behavior-symbols))
                 (defmethod entity-decode-full ((entity-type-id (eql ,entity-type-id)) entity-vector)
                   ,(entity-decoder slot-symbols behavior-symbols))
                 (defmethod entity-decode ((entity-type-id (eql ,entity-type-id)) entity-vector)
                   (let ((entity ,(entity-decoder networked-slots networked-behavior-symbols)))
                     (setf (entity-behaviors entity)
                           (append (entity-behaviors entity)
                                   (list ,@(loop for behavior in non-networked-behaviors
                                                 collect `(entity-make-default-behavior entity ',behavior)))))
                     entity)))))
           ,@methods))))))

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
