(uiop:define-package :slither/networking/actions
  (:use :cl
        :slither/utils
        :slither/networking/networked
        :slither/networking/protocol
        :slither/networking/connection)
  (:export :define-networked-action
           :call-networked-action))

(in-package :slither/networking/actions)

(defvar *networked-actions* (make-hash-table :test 'eq))

(defvar *networked-actions-id-counter* 0)

(defun hash-table-find-key (hash-table value &key (test 'eql))
  (do-hash-table (key hash-value hash-table)
    (when (funcall test value hash-value)
      (return-from hash-table-find-key
        (values key hash-value)))))

(defmacro define-networked-action (name lambda-list &body body)
  (alexandria:with-gensyms (networked-action-id)
    `(let ((,networked-action-id (or (hash-table-find-key *networked-actions* ',name)
                                     (incf *networked-actions-id-counter*))))
       (defun ,name ,lambda-list
         (when (eq (networking-environment)
                   :client)
           (connection-add-subpacket slither/networking/client::*server-connection*
                                     (make-subpacket :action
                                                     ,networked-action-id
                                                     ,@(lambda-list-bindings lambda-list))))
         ,@body)
       (setf (gethash ,networked-action-id *networked-actions*) ',name))))

(defun call-networked-action (action-id arguments)
  #+micros (micros:watch action-id)
  (apply (gethash action-id *networked-actions*) arguments))
