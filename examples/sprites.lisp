(uiop:define-package #:slither/examples/sprites
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/sprites)

(define-texture player-icon #P"examples/player.png")

(defentity player ()
  (:behaviors
   (sprite :texture player-icon)
   move
   camera))

(defun start-example ()
  (add-entity (make-instance 'player))
  (start-game))