(uiop:define-package #:slither/examples/directional-audio
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/directional-audio)

(defentity player
    ()
  (:behaviors
   listener
   camera
   rectangle
   move))

(defsound woosh #P"woosh.mp3")

(defentity boombox
    ()
  (:behaviors
   (speaker :sound (slither/assets:asset-data 'woosh))
   rectangle)
  (:tick boombox
   (let ((speaker (entity-find-behavior boombox 'speaker)))
     (when speaker
       (cond ((key-pressed-p :p) (speaker-play speaker boombox)))))))

(defun start-example ()
  (spawn-entity 'player)
  (spawn-entity 'boombox)
  (start-game))