(uiop:define-package :slither/ui
  (:use :cl
        :slither/utils
        :slither/input
        :slither/render)
  (:export :draw-text
           :ui-draw-text
           :ui-draw-text-box))

(in-package :slither/ui)

(define-array-texture font (asdf:system-relative-pathname :slither "assets/font.png")
  :width 128
  :height 128)

(defun font-index->char (char)
  (case char
    (0 #\A) (1 #\B) (2 #\C)
    (3 #\D) (4 #\E) (5 #\F)
    (6 #\G) (7 #\H) (8 #\I)
    (9 #\J) (10 #\K) (11 #\L)
    (12 #\M) (13 #\N) (14 #\O)
    (15 #\P) (16 #\Q) (17 #\R)
    (18 #\S) (19 #\T) (20 #\U)
    (21 #\V) (22 #\W) (23 #\X)
    (24 #\Y) (25 #\Z)
    (26 #\a) (27 #\b) (28 #\c)
    (29 #\d) (30 #\e) (31 #\f)
    (32 #\g) (33 #\h) (34 #\i)
    (35 #\j) (36 #\k) (37 #\l)
    (38 #\m) (39 #\n) (40 #\o)
    (41 #\p) (42 #\q) (43 #\r)
    (44 #\s) (45 #\t) (46 #\u)
    (47 #\v) (48 #\w) (49 #\x)
    (50 #\y) (51 #\z)))

(defun char->font-index (char)
  (case char
    (#\A 0) (#\B 1) (#\C 2)
    (#\D 3) (#\E 4) (#\F 5)
    (#\G 6) (#\H 7) (#\I 8)
    (#\J 9) (#\K 10) (#\L 11)
    (#\M 12) (#\N 13) (#\O 14)
    (#\P 15) (#\Q 16) (#\R 17)
    (#\S 18) (#\T 19) (#\U 20)
    (#\V 21) (#\W 22) (#\X 23)
    (#\Y 24) (#\Z 25)
    (#\a 26) (#\b 27) (#\c 28)
    (#\d 29) (#\e 30) (#\f 31)
    (#\g 32) (#\h 33) (#\i 34)
    (#\j 35) (#\k 36) (#\l 37)
    (#\m 38) (#\n 39) (#\o 40)
    (#\p 41) (#\q 42) (#\r 43)
    (#\s 44) (#\t 45) (#\u 46)
    (#\v 47) (#\w 48) (#\x 49)
    (#\y 50) (#\z 51)))

(serapeum:-> draw-text (vec2
                        string
                        &key
                        (:rotation number)
                        (:size vec2)
                        (:shader-program shader-program)
                        (:layer integer)
                        (:depth integer)))
(defun draw-text (position text &key (rotation 0)
                                     (size (vec2 0.1))
                                     (shader-program array-texture-shader-program)
                                     (layer 0)
                                     (depth 0))
  (declare (type vec2 position)
           (type string text)
           (type number rotation)
           (type vec2 size))
    (loop for char across text
          for i from 0
          do (unless (char= char #\Space)
               (draw-array-texture (v+ position
                                       (let ((row-width 30)
                                             (whitespace (+ (* (vx size) 1.5)
                                                            (* (vx size) 0.1))))
                                         (vec2 (* whitespace (mod i row-width))
                                               (* (- whitespace) (floor (/ i row-width))))))
                                   size
                                   (char->font-index char)
                                   font
                                   :color (vec4 1.0 1.0 1.0 1.0)
                                   :rotation rotation
                                   :layer layer
                                   :depth depth
                                   :shader-program shader-program))))

(serapeum:-> ui-draw-text (vec2 string))
(defun ui-draw-text (text position)
  (declare (type vec2 position)
           (type string text))
  (draw-text position
             text
             :rotation 0
             :size (let ((aspect-ratio (/ slither/window:*window-width* slither/window:*window-height*)))
                     (vec2 (/ 0.1 aspect-ratio) 0.1))
             :depth 30
             :shader-program ui-array-texture-shader-program))

(defun calculate-text-offset (text-index &key (row-length 30) (character-width 0.1))
  (let ((whitespace (+ (* character-width 1.5)
                       (* character-width 0.1))))
    (vec2 (* whitespace (mod text-index row-length))
          (* (- whitespace) (floor (/ text-index row-length))))))

(defun ui-draw-text-box (text position &key (color (vec4 0.0 0.0 0.0 1.0))
                                            (row-length 30)
                                            (character-width 0.05))

  (draw-rectangle position
                  (v+ (vec2 0 0.1)
                      (calculate-text-offset (length text)
                                             :row-length row-length
                                             :character-width character-width))
                  color
                  :shader-program ui-color-shader-program
                  :anchor :right
                  :depth -1)
  (ui-draw-text position text))

(defun mouse-hover-p (position size &key (anchor :center))
  (declare (ignore anchor))
  (point

(defun ui-draw-button (text position &key (color (vec4 0.0 0.0 0.0 1.0))
                                          (select-color (vec4 0.1 0.1 0.1 1.0))
                                          (row-length 30)
                                          (character-width 0.05))
  (when (normalized-screen-space-mouse-position)
  (ui-draw-text-box text
                    position
                    :color color
                    :row-length row-length
                    :character-width character-width))


(defun ui-draw-tree (tree)
  (cond
    ((consp tree) (ui-draw-node (car tree))
                  (ui-draw-tree (cdr tree)))
    (t (draw-node tree))))

(defun ui-draw-node (node)
  (destructuring-bind (type arguments)
      (case type
        (:text
         (desctructuring-bind (text position) arguments
                              (ui-draw-text text position)))
        (:text-box (destructuring-bind (text position) arguments
                     (ui-draw-text-box text position))))))
