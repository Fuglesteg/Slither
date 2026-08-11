(uiop:define-package #:slither/examples/ui
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/ui)

(defscene ui-example-1 ()
  (:fixed-tick
   (ui-layout
     (box (:layout-direction :top-to-bottom
           :x 100.0
           :y 100.0
           :child-gap 20.0
           :background-color (vec4 0.5 0.5 0.9 1.0))
       (loop repeat 4
             collect (box (:child-gap 20.0)
                       (loop repeat 4
                             collect (box (:height 50.0
                                           :width 50.0
                                           :background-color (vec4 1.0))))))))))

(defvar *messagep* nil)

(defscene ui-example-2 ()
  (:fixed-tick
   (ui-layout
     (box (:x 30.0
           :y 30.0
           :layout-direction :top-to-bottom
           :child-gap 100.0)
       (text (:text-content "Planet Crash"
              :text-size 100.0))
       (box (:layout-direction :top-to-bottom)
         (text (:text-content "Username"
                :text-size 40.0))
         (text-input (:id "name")))
       (box (:layout-direction :top-to-bottom)
         (text (:text-content "Server"
                :text-size 40.0))
         (text-input (:id "ip-address")))
       (button (:button-text "Join server"
                :on-click (lambda (button)
                            (setf *messagep* (not *messagep*)))))
       (when *messagep*
         (text (:text-content "YO"
                :text-size 50.0)))))))

(defun start-example ()
  (setf (current-scene) (make-instance 'ui-example-2))
  (start-game))
