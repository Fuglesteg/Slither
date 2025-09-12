(uiop:define-package #:slither/examples/sprites
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/sprites)

(define-texture player-icon #P"examples/player.png")

(defentity player ()
  (:behaviors
   #+nil(sprite :texture player-icon)
   move
   rectangle
   camera))

(defun start-example ()
  (spawn-entity 'player)
  (start-game))