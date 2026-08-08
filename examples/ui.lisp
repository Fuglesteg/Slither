(uiop:define-package #:slither/examples/ui
  (:use :cl :slither)
  (:export :start-example))

(in-package #:slither/examples/ui)

(defscene ui-example-1 ()
  (:fixed-tick
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
             :width :grow
             :child-gap 10.0)
         (box (:width :grow
               :height :grow
               :background-color (vec4 1.0 0.0 0.0 1.0)))
         (loop repeat 8
               collect (box (:height 100.0 :width 100.0
                             :background-color (vec4 0.1 0.4 0.7 1.0)))))))))

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
       (button (:button-text "Join"
               :on-click (lambda (button)
                           (setf *messagep* (not *messagep*)))))
       (when *messagep*
         (text (:text-content "YO"
                :text-size 50.0)))))))

(defun start-example ()
  (setf (current-scene) (make-instance 'ui-example-2))
  (start-game))
