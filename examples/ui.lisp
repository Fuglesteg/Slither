(uiop:define-package #:slither/examples/ui
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/ui)

(defscene ui-example ()
  (:tick
   (ui-layout
     (box (:layout-direction :top-to-bottom
           :background-color (vec4 1.0 0.5 0.3 1.0))
       (box (:height 10.0
             :width :grow
             :background-color (vec4 1.0 0.0 0.0 1.0)))
       (box (:padding-left 10.0 :padding-right 10.0
             :padding-top 10.0 :padding-bottom 10.0
             :child-gap 10.0)
         (loop repeat 10
               collect (box (:height 100.0 :width 100.0
                             :background-color (vec4 0.9 0.4 0.5 1.0)))))
       (box (:padding-left 10.0 :padding-right 10.0
             :padding-top 10.0 :padding-bottom 10.0
             :child-gap 10.0)
         (loop repeat 10
               collect (box (:height 100.0 :width 100.0
                             :background-color (vec4 0.1 0.4 0.7 1.0)))))
       (box (:height 10.0
             :width :grow
             :background-color (vec4 1.0 0.0 0.0 1.0)))))))

(defun start-example ()
  (setf (current-scene) (make-instance 'ui-example))
  (start-game))
