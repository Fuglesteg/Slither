(uiop:define-package :slither/networking/client
  (:use :cl
        :slither/utils
        :slither/core
        :slither/scenes
        :slither/networking/protocol
        :slither/networking/networked
        :slither/networking/socket
        :slither/networking/connection)
  (:import-from :serapeum
                :octet-vector)
  (:export
   :flush-server-connection
   :init-server-connection
   :send-inputs
   :unprocessed-inputs))

(in-package :slither/networking/client)

(defclass server-connection (connection) ())

(defvar *server-connection*)

(defun connect-to-server (address &optional (username "Solan Gundersen"))
  (socket-open)
  (setf *server-connection*
        (make-instance 'server-connection
                       :remote address))
  (connection-add-subpacket *server-connection* (make-subpacket :connect username)))

(defparameter *inbound-packet-buffer* (make-array 1000
                                                  :fill-pointer 0
                                                  :initial-element (make-octet-vector 0)
                                                  :element-type 'octet-vector))

(defun run-server-connection (address &optional username)
  (unwind-protect
       (loop
         (vector-push (socket-receive)
                      *inbound-packet-buffer*))
    (socket-close)
    (setf *server-connection* nil)))

(defun init-server-connection (address &optional username)
  (connect-to-server address username)
  (sb-thread:make-thread
   (lambda ()
     (run-server-connection address username))
   :name "server-connection"))

(defconstant +input-buffer-size+ 20)

(defvar *input-buffer*
  (make-array +input-buffer-size+
              :fill-pointer 0
              :initial-element nil))

(defun input-buffer-add (inputs)
  (input-buffer-shift)
  (setf (aref *input-buffer* 0) inputs))

(defun input-buffer-shift (&optional (amount 1))
  (unless (<= (1- (array-total-size *input-buffer*))
             (fill-pointer *input-buffer*))
    (incf (fill-pointer *input-buffer*)))
  (replace *input-buffer* *input-buffer*
           :start1 amount)
  *input-buffer*)

(defun send-inputs (inputs)
  (connection-add-subpacket *server-connection*
                            (make-subpacket :input
                                            inputs)))

(-> unprocessed-inputs () (vector t))
(defun unprocessed-inputs ()
  "Returns the inputs that the server has not yet processed.
Intended to be used for client-side prediction."
  *input-buffer*)

(-> flush-server-connection () ())
(defun flush-server-connection ()
  (send-inputs slither/input::*inputs*)
  (connection-flush *server-connection*)
  (let ((inbound-packet-buffer (subseq *inbound-packet-buffer* 0 (length *inbound-packet-buffer*))))
    (setf (fill-pointer *inbound-packet-buffer*) 0)
    (loop for packet across inbound-packet-buffer
          do (multiple-value-bind (protocol
                                   tick
                                   packet-id
                                   acknowledging-packet-id
                                   last-acknowledged-packets
                                   subpackets) (parse-packet packet)
               (declare (ignore protocol))
               (connection-acknowledge-received *server-connection*
                                                packet-id)
               (connection-acknowledge-sent *server-connection*
                                            acknowledging-packet-id
                                            last-acknowledged-packets)
               (when (> tick (+ 30 (current-tick)))
                 (setf (current-tick) tick)
                 #+nil(setf (last-tick-time) (org.shirakumo.fraf.glfw:time))
                 (slither/input::input-history-reset-to-tick tick))
               (let (failed-ownership-application)
                 (loop for (subpacket-type . subpacket) in subpackets
                       do (ecase subpacket-type
                            (:update
                             (destructuring-bind (networked-object-id place-id new-value) subpacket
                               (when-let ((networked-object (find-networked networked-object-id)))
                                 (when (<= (networked-last-tick-update networked-object) tick)
                                   (networked-apply-update networked-object
                                                           place-id
                                                           new-value)
                                   (setf (networked-last-tick-update networked-object) tick)))))
                            (:action
                             #+nil(destructuring-bind (networked-object-id action-id arguments) subpacket
                                    (apply #'networked-apply-action
                                           (find-networked networked-object-id)
                                           action-id
                                           arguments)))
                            (:entity
                             (destructuring-bind (entity-type-id entity) subpacket
                               (declare (ignore entity-type-id))
                               (add-entity entity)))
                            (:owner
                             (destructuring-bind (entity-ids) subpacket
                               (loop for entity-id in entity-ids
                                     do (alexandria:if-let ((networked (find-networked entity-id)))
                                          (setf (networked-mode networked)
                                                :client-predicted)
                                          (push entity-id failed-ownership-application)))))
                            (:destroy
                             (destructuring-bind (networked-object-id) subpacket
                               (when-let ((networked (find-networked networked-object-id)))
                                 (remove-entity (behavior-entity networked)))))))
                 (dolist (entity-id failed-ownership-application)
                   (alexandria:when-let ((networked (find-networked entity-id)))
                     (setf (networked-mode networked)
                           :client-predicted)))))))
  ;; Shift input buffer so that only the inputs from the last acknowledged packet to the current packet are available
  (input-buffer-add slither/input::*inputs*)
  (setf (fill-pointer *input-buffer*)
        (clamp (- (connection-outbound-packet-id *server-connection*)
                  (connection-last-acknowledged-sent-packet-id *server-connection*))
               0
               10)))
