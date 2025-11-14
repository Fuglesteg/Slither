(uiop:define-package #:slither
  (:use #:cl)
  (:use-reexport #:org.shirakumo.fraf.math.vectors
                 #:org.shirakumo.fraf.math.matrices
                 #:slither/utils
                 #:slither/render
                 #:slither/window
                 #:slither/audio
                 #:slither/input
                 #:slither/entities
                 #:slither/behaviors
                 #:slither/scenes
                 #:slither/physics)
  (:import-from #:slither/render/uniform
                #:uniform-value
                #:uniform-location)
  (:import-from #:slither/render/shader-program
                #:shader-program
                #:make-shader-program
                #:set-uniform-value
                #:get-uniform)
  (:export #:start-game))

(in-package #:slither)

(defun start-game (&key window)
  (with-window window
    (renderer-init)
    (audio-init)
      (with-event-loop
        (input-poll)
        (update-scene)
        (renderer-flush))))

#+nil(defun start-game (&key window)
  (with-window window
    (renderer-init)
    #+nil(audio-init)
    (let ((i 0))
      (sb-profile:reset)
      (with-event-loop
        (when (> i 1000)
          (sb-profile:report)
          (return))
        (input-poll)
        (update-entities)
        (renderer-flush)
        (incf i)))))
