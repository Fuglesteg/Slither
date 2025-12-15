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
   :init-server-connection))

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
                                                  :initial-element (make-array 0 :element-type '(unsigned-byte 8))
                                                  :element-type '(vector (unsigned-byte 8))))

(defun run-server-connection (address)
  (connect-to-server address)
  (unwind-protect
       (loop
         (vector-push (socket-receive)
                      *inbound-packet-buffer*))
    (socket-close)
    (setf *server-connection* nil)))

(defun init-server-connection (address)
  (sb-thread:make-thread
   (lambda ()
     (run-server-connection address))
   :name "server-connection"))

(defun flush-server-connection ()
  (connection-flush *server-connection*)
  (loop for packet across *inbound-packet-buffer*
        do (multiple-value-bind (protocol
                                 tick
                                 packet-id
                                 acknowledging-packet-id
                                 last-acknowledged-packets
                                 subpackets) (parse-packet packet)
             (declare (ignore tick protocol))
             (connection-acknowledge-received *server-connection*
                                              packet-id)
             (connection-acknowledge-sent *server-connection*
                                          acknowledging-packet-id
                                          last-acknowledged-packets)
             (loop for (subpacket-type . subpacket) in subpackets
                   do (ecase subpacket-type
                        (:update (destructuring-bind (networked-object-id place-id new-value) subpacket
                                   (alexandria:when-let ((networked-object (find-networked networked-object-id)))
                                   (networked-apply-update networked-object
                                                           place-id
                                                           new-value))))
                        (:action (destructuring-bind (networked-object-id action-id arguments) subpacket
                                   (apply #'networked-apply-action
                                          (find-networked networked-object-id)
                                          action-id
                                          arguments)))
                        (:spawn
                         (destructuring-bind (entity-type-id entity) subpacket
                           (declare (ignore entity-type-id))
                           (add-entity entity)))))))
  (setf (fill-pointer *inbound-packet-buffer*) 0))
