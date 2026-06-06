(uiop:define-package :slither/networking/client
  (:use :cl
        :slither/utils
        :slither/core
        :slither/scenes
        :slither/networking/protocol
        :slither/networking/client-prediction
        :slither/networking/networked
        :slither/networking/socket
        :slither/networking/connection)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:import-from :serapeum
                :octet-vector)
  (:export
   :flush-server-connection
   :init-server-connection
   :send-inputs
   :unprocessed-inputs
   :disconnect))

(in-package :slither/networking/client)

(defclass server-connection (connection) ())

(defvar *server-connection*)

(defconstant +default-port+ 7878)

(defun host-address (host)
  (let* ((port-separator-position (position #\: host))
         (domain (if port-separator-position
                     (subseq host 0 port-separator-position)
                     host))
         (port (if port-separator-position
                   (parse-integer (subseq host port-separator-position))
                   +default-port+)))
         (multiple-value-bind (ipv4-entry ipv6-entry)
             (sb-bsd-sockets:get-host-by-name domain)
           (list
            (or (sb-bsd-sockets:host-ent-address ipv4-entry)
                (sb-bsd-sockets:host-ent-address ipv6-entry))
            port))))

(defun connect-to-server (host &optional (username "Solan Gundersen"))
  (reset-client-prediction)
  (socket-open)
  (setf *server-connection*
        (make-instance 'server-connection
                       :remote (host-address host)))
  (connection-add-subpacket *server-connection* (make-subpacket :connect username)))

(defvar *inbound-packet-buffer-lock* (sb-thread:make-mutex :name "inbound-packet-buffer"))
(defvar *inbound-packet-buffer* (make-array 1000
                                            :fill-pointer 0
                                            :initial-element (make-octet-vector 0)
                                            :element-type 'octet-vector))

(defun disconnect ()
  (setf (connection-outbound-subpackets *server-connection*)
        (list (make-subpacket :disconnect)))
  (connection-flush *server-connection*)
  (socket-close)
  (setf (fill-pointer *inbound-packet-buffer*) 0)
  (setf *server-connection* nil))

(defun run-server-connection (address &optional username)
  (declare (ignore address username))
  (ignore-errors
    (unwind-protect
         (loop
           (let ((packet (socket-receive)))
             (sb-thread:with-mutex (*inbound-packet-buffer-lock*)
               (vector-push packet
                            *inbound-packet-buffer*))))
      (socket-close)
      (setf *server-connection* nil))))

(defvar *server-connection-thread* nil)

(defun init-server-connection (address &optional username)
  (connect-to-server address username)
  (setf *server-connection-thread*
        (sb-thread:make-thread
         (lambda ()
               (run-server-connection address username))
         :name "slither-server-connection")))

(defvar *input-packets* nil)

(defun send-inputs (inputs)
  (push (make-subpacket :input
                        (current-tick)
                        inputs)
        *input-packets*)
  (when (<= 5 (length *input-packets*))
    (setf *input-packets* (subseq *input-packets* 0 5)))
  (dolist (packet *input-packets*)
    (connection-add-subpacket *server-connection*
                              packet)))

(defun send-timestamp ()
  (connection-add-subpacket *server-connection*
                            (make-subpacket :echo
                                            (glfw:time))))

(defun process-inbound-packets ()
  (when *server-connection*
    (let ((inbound-packet-buffer
            (sb-thread:with-mutex (*inbound-packet-buffer-lock*)
              (prog1 (subseq *inbound-packet-buffer* 0 (length *inbound-packet-buffer*))
                (setf (fill-pointer *inbound-packet-buffer*) 0)))))
      (loop for packet across inbound-packet-buffer
            when packet
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
                 (when (< (server-tick) tick)
                   (setf (server-tick) tick)
                   (slither/input::input-history-reset-to-tick tick))
                 (let (failed-ownership-application)
                   (loop for (subpacket-type . subpacket) in subpackets
                         do (ecase subpacket-type
                              (:connect
                               (setf (current-tick) (+ tick (estimated-tick-offset))))
                              (:update
                               (destructuring-bind (networked-object-id place-id new-value) subpacket
                                 (when-let ((networked-object (find-networked networked-object-id)))
                                           (when (<= (networked-last-tick-update networked-object) tick)
                                             (networked-apply-update networked-object
                                                                     place-id
                                                                     new-value)
                                             (setf (networked-last-tick-update networked-object) tick)))))
                              (:action)
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
                                           (remove-entity (behavior-entity networked)))))
                              (:echo
                               (destructuring-bind (time) subpacket
                                 (register-round-trip-time time)))))
                   (dolist (entity-id failed-ownership-application)
                     (alexandria:when-let ((networked (find-networked entity-id)))
                                          (setf (networked-mode networked)
                                                :client-predicted)))))))))

(-> flush-server-connection () ())
(defun flush-server-connection ()
  (when *server-connection*
    (send-inputs slither/input::*inputs*)
    (send-timestamp)
    (connection-flush *server-connection*)
    ;; Control tick rate to nudge the predicted ticks to match the estimated offset
    (client-prediction-tick-rate-flush)))
