(uiop:define-package :slither/examples/multiplayer
  (:use :cl
        :slither))

(in-package :slither/examples/multiplayer)

;; Inputs are sent to networked object id
;; Input behavior is used to apply inputs

(defentity player
  ((name :init "Reodor Felgen"))
  (:behaviors
   transform
   networked
   move
   rectangle))

(defscene multiplayer-server ()
  (:entities player)
  (:start
   (start-server)
   (setf (on-new-connection)
         (lambda (connection)
           (let ((player (spawn-entity 'player
                                       :name (user-name
                                              (slither/networking/server::client-connection-user connection)))))
             (setf (slither/networking/server::client-connection-entities connection)
                   (list player))
             (send-entity player)))))
  (:tick
   (flush-server)))

(defscene multiplayer-client ()
  (:start
   (start-network-client (list #(127 0 0 1) 7777)))
  (:tick
   (flush-server-connection)))

(defun start-game-server ()
  (setf (on-new-connection)
        (lambda (connection)
          (let ((player (spawn-entity 'player
                                      :name (user-name
                                             (slither/networking/server::client-connection-user connection)))))
            (setf (slither/networking/server::client-connection-entities connection)
                  (list player))
            (send-entity player))))
  (let ((scene (make-instance 'multiplayer-server)))
    (scene-make-networked scene)
    (setf (current-scene) scene))
  (org.shirakumo.fraf.glfw:init)
  (run-server))

(defun start-game-client ()
  (let ((scene (make-instance 'multiplayer-client)))
    (scene-make-networked scene)
    (setf (current-scene) scene))
  (start-game))
