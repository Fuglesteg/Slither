(uiop:define-package #:slither/examples/directional-audio
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/directional-audio)

(defentity player
    ()
  (:behaviors
   (make-instance 'listener)
   (make-instance 'camera)
   (make-instance 'rectangle)
   (make-instance 'move)))

(defsound woosh #P"woosh.mp3")

(defentity boombox
    ()
  (:behaviors
   (make-instance 'speaker :sound (slither/assets:asset-data 'woosh))
   (make-instance 'rectangle))
  (:tick boombox
   (let ((speaker (entity-find-behavior boombox 'speaker)))
     (when speaker
       (cond ((key-pressed-p :p) (speaker-play speaker boombox)))))))

(defun start-example ()
  (add-entity (make-instance 'player)
              (make-instance 'boombox))
  (start-game))