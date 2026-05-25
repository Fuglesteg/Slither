(uiop:define-package :slither/networking/server
  (:use :cl
        :slither/utils
        :slither/core
        :slither/scenes
        :slither/networking/protocol
        :slither/networking/networked
        :slither/networking/socket
        :slither/networking/connection)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export
   :user
   :user-name
   :client-connection
   :client-connection-user
   :client-connection-entities
   :send-update
   :init-server
   :flush-server
   :send-entity
   :on-new-connection
   :user
   :user-name
   :run-server
   :on-disconnect))

(in-package :slither/networking/server)

(defvar *client-connections*
  (make-hash-table :test 'equalp)) ; Must use equalp to compare the address vectors

(defun find-client-connection (remote)
  (gethash remote *client-connections*))

(defclass user ()
  ((name
    :initform (error "Name is required")
    :initarg :name
    :accessor user-name
    :type string)))

(defconstant +max-future-tick-amount+ 10)
(defconstant +max-late-tick-amount+ 5)

(defclass client-connection (connection)
  ((user
    :initform (error "User is required")
    :initarg :user
    :accessor client-connection-user
    :type user)
   (last-tick-received
    :initform (current-tick)
    :accessor client-connection-last-tick-received)
   (entities
    :initform nil
    :initarg :entities
    :reader client-connection-entities)
   (inputs
    :initform (slither/input:copy-inputs slither/input::*inputs*)
    :reader client-connection-inputs)
   (inputs-buffer
    :initform (make-array (+ +max-future-tick-amount+
                             +max-late-tick-amount+)
                          :initial-element nil)
    :accessor client-connection-inputs-buffer)))

(defun (setf client-connection-entities) (new-entities client-connection)
  (setf (slot-value client-connection 'entities)
        new-entities)
  (connection-add-subpacket
   client-connection
   (make-subpacket :owner
                   (mapcar
                    (lambda (entity)
                      (networked-id (entity-find-behavior entity 'networked)))
                    new-entities))))

(defun client-inputs-reset ()
  (do-hash-table (remote client *client-connections*)
    (declare (ignore remote))
    (let ((buffer (client-connection-inputs-buffer client)))
      (dotimes (i (length buffer))
        (when (aref buffer i)
          (setf (aref buffer i) :processed))))))

(defun client-inputs-add (client tick inputs)
  (let ((index (+ (- tick (current-tick)) +max-future-tick-amount+)))
    (if (< -1 index (+ +max-future-tick-amount+ +max-late-tick-amount+))
        (setf (aref (client-connection-inputs-buffer client) index) inputs)
        (format t "Missed input by ~a~%" index))))

(defun client-inputs-shift ()
  (do-hash-table (remote client *client-connections*)
    (declare (ignore remote))
    (with-accessors ((inputs client-connection-inputs-buffer)) client
      (replace inputs inputs :start1 1)
      (setf (aref inputs 0) nil))))

(defclass inbound-packet ()
  ((origin
    :initarg :origin
    :accessor inbound-packet-origin
    :type list)
   (data
    :initarg :data
    :accessor inbound-packet-data
    :type vector)))

(defparameter *inbound-packet-buffer* (make-array 1000
                                                  :fill-pointer 0
                                                  :initial-element (make-instance 'inbound-packet)
                                                  :element-type 'inbound-packet))

(defun run-listener ()
  (unless (and (boundp 'slither/networking/socket::*socket*)
               slither/networking/socket::*socket*)
    (socket-open)
    (socket-listen)
    (unwind-protect
         (loop (multiple-value-bind (data remote)
                   (socket-receive)
                 (when (and data
                            (> (length data) 0))
                   (vector-push (make-instance 'inbound-packet
                                               :origin remote
                                               :data data)
                                *inbound-packet-buffer*))))
      (socket-close)
      (setf *client-connections* nil))))

(defun init-listener ()
  (sb-thread:make-thread
   #'run-listener
   :name "packet-listener"))

(defvar *on-new-connection* nil)

(defun on-new-connection (connection)
  (funcall *on-new-connection* connection))

(defun (setf on-new-connection) (new-value)
  (setf *on-new-connection* new-value))

(defvar *on-disconnect* nil)

(defun on-disconnect (connection)
  (funcall *on-disconnect* connection))

(defun (setf on-disconnect) (new-value)
  (setf *on-disconnect* new-value))

(defun client-connections-add-subpacket (subpacket)
  (loop for key being the hash-keys of *client-connections*
        using (hash-value connection)
        do (connection-add-subpacket connection subpacket)))

(defun send-update (networked-object-id update-place-id new-value)
  (let ((subpacket (make-subpacket :update networked-object-id update-place-id new-value)))
    (client-connections-add-subpacket subpacket)))

(defun send-entity (entity)
  (let ((subpacket (make-subpacket :entity
                                   (networked-id (entity-find-behavior entity 'networked))
                                   entity)))
    (client-connections-add-subpacket subpacket)))

(defun send-destroy-entity (entity)
  (client-connections-add-subpacket
   (make-subpacket :destroy
                   (networked-id (entity-find-behavior entity 'networked)))))

(defun find-connection (origin)
  (gethash origin *client-connections*))

(defun entity-client-connection (entity)
  (loop for key being the hash-keys of *client-connections*
        using (hash-value client-connection)
        do (loop for client-entity in (client-connection-entities client-connection)
                 when (eq client-entity
                          entity)
                 do (return-from entity-client-connection client-connection))))

(defun run-server ()
  (glfw:init)
  (init-listener)
  (setf (on-add-networked)
        (lambda (networked)
          (send-entity (behavior-entity networked))))
  (setf (on-remove-networked)
        (lambda (networked)
          (send-destroy-entity (behavior-entity networked))))
  (let ((last-time (glfw:time))
        (tick 0))
    (loop
      (let ((wake-time (+ last-time (tick-delta))))
        (sleep (max 0 (- (- wake-time (glfw:time)) 0.002)))
        (loop until (>= (glfw:time) wake-time))
        (setf last-time wake-time)
        (incf tick)
        ;; Process all accumulated ticks
        (let ((slither/core::*tick* tick)
              (slither/core::*delta-time* (tick-delta)))
          #+micros (slither/window::read-repl)
          (flush-server)
          (loop for entity in (scene-entities (current-scene))
                do (let ((client-connection
                           (entity-client-connection entity)))
                     (cond
                       (client-connection
                         (loop for input across (client-connection-inputs-buffer client-connection)
                               do (unless (or (null input)
                                              (eq input :processed))
                                    (if (eq input :empty)
                                        (progn (tick entity)
                                               (fixed-tick entity))
                                        (destructuring-bind (buttons . analogues) input
                                          (let ((slither/input::*inputs* (client-connection-inputs client-connection)))
                                            (slither/input::apply-decoded-inputs buttons analogues)
                                            (tick entity)
                                            (fixed-tick entity)
                                            (do-hash-table (input-name input slither/input::*inputs*)
                                              (declare (ignore input-name))
                                              (typecase input
                                                (slither/input::button-input
                                                 (case (slither/input::button-input-value input)
                                                   (:released (setf (slither/input::button-input-value input) nil))
                                                   (:pressed (setf (slither/input::button-input-value input) :held)))))))))))
                         ;; Simulate if a null tick exited the window of accepted inputs
                         (unless (aref (client-connection-inputs-buffer client-connection)
                                       (1- (length (client-connection-inputs-buffer client-connection))))
                           (format t "Missed inputs for tick~a~%" (current-tick))
                           (finish-output)
                           (tick entity)
                           (fixed-tick entity)))
                       (t
                        (tick entity)
                        (fixed-tick entity)))))
          (client-inputs-reset)
          (client-inputs-shift))))))

(defun init-server ()
  (sb-thread:make-thread #'run-server
                         :name "slither-server"))

(defun flush-server ()
  ;; Get updates from networked objects
  (do-hash-table (key networked-object (networked-objects))
    (loop for (networked-slot . slot-behavior) in (networked-get-updated-places networked-object)
          do (send-update key
                          (entity-find-networked-slot-id (behavior-entity networked-object)
                                                         networked-slot
                                                         slot-behavior)
                          (if slot-behavior
                              (slot-value (entity-find-behavior (behavior-entity networked-object)
                                                                slot-behavior)
                                          networked-slot)
                              (slot-value (behavior-entity networked-object)
                                          networked-slot)))))
  ;; Send outbound packets to clients
  (do-hash-table (address client-connection *client-connections*)
    (declare (ignore address))
    (connection-flush client-connection))
  ;; Process inbound packets
  (let ((inbound-packet-buffer (subseq *inbound-packet-buffer* 0 (length *inbound-packet-buffer*))))
    (setf (fill-pointer *inbound-packet-buffer*) 0)
    (loop for inbound-packet across inbound-packet-buffer
        do (multiple-value-bind (protocol
                                 tick
                                 packet-id
                                 acknowledging-packet-id
                                 last-acknowledged-packets
                                 subpackets) (parse-packet (inbound-packet-data inbound-packet))
             (declare (ignore protocol)
                      (ignorable tick))
             (let ((connection (find-connection (inbound-packet-origin inbound-packet))))
               (loop for (subpacket-type . subpacket) in subpackets
                     do (restart-case
                          (ecase subpacket-type
                            (:connect (destructuring-bind (username) subpacket
                                        (when connection
                                          (error "Connection already exists"))
                                        (let ((user (make-instance 'user :name username)))
                                          (setf (gethash (inbound-packet-origin inbound-packet)
                                                         *client-connections*)
                                                (make-instance 'client-connection
                                                               :remote (inbound-packet-origin inbound-packet)
                                                               :user user))
                                          (setf connection (find-connection (inbound-packet-origin inbound-packet)))
                                          ;; Send scene to new connection
                                          (setf (connection-outbound-subpackets connection)
                                                (loop for key being the hash-keys of (networked-objects)
                                                      using (hash-value networked-object)
                                                      collect (make-subpacket :entity
                                                                              key
                                                                              (behavior-entity networked-object))))
                                          (connection-add-subpacket connection
                                                                    (make-subpacket :connect ""))
                                          (when *on-new-connection*
                                            (on-new-connection connection)))))
                            (:disconnect
                             (remhash (inbound-packet-origin inbound-packet) *client-connections*)
                             (on-disconnect connection))
                            (:update t #+nil(destructuring-bind (networked-object-id place-id new-value) subpacket
                                              (networked-apply-update (find-networked networked-object-id)
                                                                      place-id
                                                                      new-value)))
                            (:action #+nil(destructuring-bind (networked-object-id action-id arguments) subpacket
                                            (apply #'networked-apply-action
                                                   (find-networked networked-object-id)
                                                   action-id
                                                   arguments)))
                            (:entity nil)
                            (:input (when connection
                                      (destructuring-bind (buttons analogues) subpacket
                                        (if (and (null buttons)
                                                 (null analogues))
                                            (client-inputs-add connection
                                                               tick
                                                               :empty)
                                            (client-inputs-add connection
                                                               tick
                                                               (cons buttons analogues))))))
                            (:destroy (destructuring-bind (networked-object-id) subpacket
                                        (remove-entity
                                         (behavior-entity
                                          (find-networked networked-object-id)))))
                            (:echo (destructuring-bind (argument) subpacket
                                     (when connection
                                     (connection-add-subpacket connection (make-subpacket :echo argument))))))
                          (skip-packet ()
                            :report "Skip the current packet"
                            t)))
               (when connection
                 (setf (client-connection-last-tick-received connection) (current-tick))
                 (connection-acknowledge-received connection packet-id)
                 (connection-acknowledge-sent connection acknowledging-packet-id last-acknowledged-packets))))))
  ;; Remove timed out connections
  (do-hash-table (client-origin client-connection *client-connections*)
    (when (< (client-connection-last-tick-received client-connection)
             (- (current-tick) 100))
      (remhash client-origin *client-connections*)
      (on-disconnect client-connection))))
