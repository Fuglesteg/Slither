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

(defun start-server ()
  (init-server)
  (setf (networking-environment) :server))

(defun serverp ()
  (eq (networking-environment)
      :server))

(defun start-network-client (address &optional username)
  (init-server-connection address username)
  (setf (networking-environment) :client))

(defun clientp ()
  (eq (networking-environment)
      :client))
