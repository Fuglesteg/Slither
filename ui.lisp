(uiop:define-package :slither/ui
  (:use :cl
        :slither/utils
        :slither/core
        :slither/input
        :slither/window
        :slither/render)
  (:export :draw-text
           :ui-draw-text
           :ui-draw-text-box
           :ui-draw-button
           :button
           :text-input))

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

(serapeum:-> draw-text (string
                        vec2
                        &key
                        (:rotation number)
                        (:size vec2)
                        (:shader-program shader-program)
                        (:layer integer)
                        (:depth integer)))
(defun draw-text (text position &key (rotation 0)
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
                                     (calculate-text-offset i :character-size size))
                                 size
                                 (char->font-index char)
                                 font
                                 :color (vec4 1.0 1.0 1.0 1.0)
                                 :rotation rotation
                                 :layer layer
                                 :depth depth
                                 :shader-program shader-program))))

(defun calculate-text-offset (text-index &key (row-length 30) (character-size (vec2 0.02)))
  (with-vec (character-width character-height) character-size
    (let ((whitespace character-width #+nil(+ (* character-width 1.5)
                                              (* character-width 0.1))))
      (vec2 (* whitespace (mod text-index row-length))
            (- character-height
               (* whitespace
                  (floor (/ text-index row-length))
                  2.5))))))

(-> ui-draw-text (string vec2 &key
                         (:size vec2)
                         (:layer integer)
                         (:depth integer)))
(defun ui-draw-text (text position &key (size (vec2 0.1))
                                        (layer 0)
                                        (depth 0))
  (declare (type vec2 position)
           (type string text))
  (draw-text text
             position
             :rotation 0
             :size (with-vec (width height) size
                     (vec2 (/ width (aspect-ratio))
                           height))
             :depth depth
             :layer layer
             :shader-program ui-array-texture-shader-program))

(defun ui-calculate-text-offset (text-index &key (row-length 30) (character-size (vec2 0.02)))
  (calculate-text-offset text-index
                         :row-length row-length
                         :character-size (vec2 (/ (vx character-size) (aspect-ratio))
                                               (vy character-size))))

(defun ui-draw-text-box (text position &key (color (vec4 0.0 0.0 0.0 1.0))
                                            size
                                            (row-length 30)
                                            (character-size (vec2 0.02)))
  (draw-rectangle (v+ position (vec2 0 (vx character-size)))
                  (if size
                      size
                      (v* (ui-calculate-text-offset (length text)
                                                    :row-length row-length
                                                    :character-size character-size)
                          (vec2 0.5 1)))
                  color
                  :shader-program ui-color-shader-program
                  :anchor :left
                  :layer 2
                  :depth 0)
  (ui-draw-text text
                position
                :size character-size
                :layer 2
                :depth 0))

(defun mouse-hover-p (position size &key (anchor :middle))
  (let ((position (position-apply-anchor position size anchor)))
    (v< (v- position size)
        (normalized-screen-space-mouse-position)
        (v+ position size))))

(defun ui-draw-button (text position &key (color (vec4 0.0 0.0 0.0 1.0))
                                          (select-color (vec4 0.1 0.1 0.1 1.0))
                                          (row-length 30)
                                          (character-size (vec2 0.02)))
  (let ((color (if (mouse-hover-p position
                                  (v* (ui-calculate-text-offset (length text)
                                                                :character-size character-size)
                                      (vec2 0.5 1))
                                  :anchor :bottom-left)
                   select-color
                   color)))
    (ui-draw-text-box text
                      position
                      :color color
                      :row-length row-length
                      :character-size character-size)))

(defbehavior text-box
    ((position :init (vec2))
     (size :init (vec2 0.01))
     (text :init "Button"))
  (:tick
   (ui-draw-text-box (button-text)
                     (button-position)
                     :character-size (button-size))))

; TODO: refactor to use text-box
(defbehavior button
    ((position :init (vec2))
     (size :init (vec2 0.01))
     (text :init "Button")
     (on-click :init (lambda (behavior))))
  (:tick
   (when (and (mouse-hover-p (button-position)
                             (v* (ui-calculate-text-offset (length (button-text))
                                                           :character-size (button-size))
                                 (vec2 0.5 1))
                             :anchor :bottom-left)
              (key-pressed-p :left-click))
     (funcall (button-on-click) *behavior*))
   (ui-draw-button (button-text)
                   (button-position)
                   :character-size (button-size))))

(defun ui-draw-text-input (text position &key size
                                              (color (vec4 0.0 0.0 0.0 1.0))
                                              focus)
  (ui-draw-text-box text
                    position
                    :character-size size
                    :color color)
  (when focus
    (draw-rectangle (v+ position
                        (ui-calculate-text-offset (length text)
                                                  :character-size size)
                        (vec2 0.002 0))
                    (vec2 0.001 (if size
                                    (vy size)
                                    0.02))
                    (vec4 1.0)
                    :shader-program ui-color-shader-program)))


(defbehavior text-input
    ((position :init (vec2))
     (size :init (vec2 0.20 0.02))
     (text :init "")
     (max-length :init 20)
     (focus :init nil))
  (:tick
   (when (key-pressed-p :left-click)
     (if (mouse-hover-p (text-input-position)
                        (text-input-size)
                        :anchor :bottom-left)
         (setf (text-input-focus) t)
         (setf (text-input-focus) nil)))
   (when (text-input-focus)
     (when (> (text-input-max-length)
              (length (text-input-text)))
       (flet ((register-input-char (input-keyword-symbol character)
                (when (key-pressed-p input-keyword-symbol)
                  (setf (text-input-text) (format nil "~a~a"
                                                  (text-input-text)
                                                  (if (key-held-p :left-shift)
                                                      (char-upcase character)
                                                      character))))))
         (register-input-char :a #\a) (register-input-char :b #\b) (register-input-char :c #\c)
         (register-input-char :d #\d) (register-input-char :e #\e) (register-input-char :f #\f)
         (register-input-char :g #\g) (register-input-char :h #\h) (register-input-char :i #\i)
         (register-input-char :j #\j) (register-input-char :k #\k) (register-input-char :l #\l)
         (register-input-char :m #\m) (register-input-char :n #\n) (register-input-char :o #\o)
         (register-input-char :p #\p) (register-input-char :q #\q) (register-input-char :r #\r)
         (register-input-char :s #\s) (register-input-char :t #\t) (register-input-char :u #\u)
         (register-input-char :v #\v) (register-input-char :w #\w) (register-input-char :x #\x)
         (register-input-char :y #\y) (register-input-char :z #\z) (register-input-char :space #\Space)))
     (when (key-pressed-p :backspace)
       (when (< 0 (length (text-input-text)))
         (setf (text-input-text)
               (subseq (text-input-text)
                       0
                       (1- (length (text-input-text))))))))
   (ui-draw-text-input (text-input-text)
                       (text-input-position)
                       :size (text-input-size)
                       :focus (text-input-focus))))
