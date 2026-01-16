(uiop:define-package #:slither/networking
  (:use :cl
        :slither/core
        :slither/utils
        :slither/serialization
        :slither/scenes)
  (:use-reexport :slither/networking/server
                 :slither/networking/client
                 :slither/networking/networked)
  (:export :start-server
           :serverp
           :start-network-client
           :clientp))

(in-package :slither/networking)

;;; Plan:
;;; - Behavior for networking
;;;   - Monitors changing values on entities/behaviors and generates packets for update requests
;;;   - Applies game-state updates from server
;;;   - Applies various techniques of client side prediction, set by user
;;; - Clients run limited simulation for client prediction
;;; - Clients apply game-state packets from server and applies them
;;; - Server runs full simulation, receives game-state update requests from clients, runs tick and sends game-state updates
;;; - Server checks client ownership of objects before applying requests (Authorization)

;;; TODO: Implement headless mode for server
;;; TODO: Implement hertz tickrate for server

(defun start-server ()
  (init-server)
  (setf (networking-environment) :server))

(defun serverp ()
  (eq (networking-environment)
      :server))

(defun start-network-client (address)
  (init-server-connection address)
  (setf (networking-environment) :client))

(defun clientp ()
  (eq (networking-environment)
      :client))
