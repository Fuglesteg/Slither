(uiop:define-package :slither/ui
  (:use :cl
        :slither/utils
        :slither/serialization
        :slither/core
        :slither/input
        :slither/window
        :slither/render)
  (:export :draw-text
           :ui-draw-text
           :ui-draw-text-box
           :ui-draw-button
           :button
           :text-input
           :ui-draw-rectangle))

(in-package :slither/ui)

(define-array-texture font (asdf:system-relative-pathname :slither "assets/font.png")
  :width 128
  :height 128)

(defun font-index->char (char)
  (case char
    (0 #\a) (1 #\b) (2 #\c)
    (3 #\d) (4 #\e) (5 #\f)
    (6 #\g) (7 #\h) (8 #\i)
    (9 #\j) (10 #\k) (11 #\l)
    (12 #\m) (13 #\n) (14 #\o)
    (15 #\p) (16 #\q) (17 #\r)
    (18 #\s) (19 #\t) (20 #\u)
    (21 #\v) (22 #\w) (23 #\x)
    (24 #\y) (25 #\z)
    (26 #\A) (27 #\B) (28 #\C)
    (29 #\D) (30 #\E) (31 #\F)
    (32 #\G) (33 #\H) (34 #\I)
    (35 #\J) (36 #\K) (37 #\L)
    (38 #\M) (39 #\N) (40 #\O)
    (41 #\P) (42 #\Q) (43 #\R)
    (44 #\S) (45 #\T) (46 #\U)
    (47 #\V) (48 #\W) (49 #\X)
    (50 #\Y) (51 #\Z)
    (52 #\0) (53 #\1) (54 #\2)
    (55 #\3) (56 #\4) (57 #\5)
    (58 #\6) (59 #\7) (60 #\8)
    (61 #\9) (62 #\!) (63 #\?)
    (64 #\.) (65 #\,)))

(defun char->font-index (char)
  (case char
    (#\a 0) (#\b 1) (#\c 2)
    (#\d 3) (#\e 4) (#\f 5)
    (#\g 6) (#\h 7) (#\i 8)
    (#\j 9) (#\k 10) (#\l 11)
    (#\m 12) (#\n 13) (#\o 14)
    (#\p 15) (#\q 16) (#\r 17)
    (#\s 18) (#\t 19) (#\u 20)
    (#\v 21) (#\w 22) (#\x 23)
    (#\y 24) (#\z 25)
    (#\A 26) (#\B 27) (#\C 28)
    (#\D 29) (#\E 30) (#\F 31)
    (#\G 32) (#\H 33) (#\I 34)
    (#\J 35) (#\K 36) (#\L 37)
    (#\M 38) (#\N 39) (#\O 40)
    (#\P 41) (#\Q 42) (#\R 43)
    (#\S 44) (#\T 45) (#\U 46)
    (#\V 47) (#\W 48) (#\X 49)
    (#\Y 50) (#\Z 51)
    (#\0 52) (#\1 53) (#\2 54)
    (#\3 55) (#\4 56) (#\5 57)
    (#\6 58) (#\7 59) (#\8 60)
    (#\9 61) (#\! 62) (#\? 63)
    (#\. 64) (#\, 65)))

(-> draw-text (string
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

(defun ui-draw-rectangle (position size &key (color (vec4 1.0 0.0 0.0 1.0))
                                             (anchor :left)
                                             (depth 0))
  (when slither/render::*initialized*
    (draw-rectangle position
                    size
                    color
                    :shader-program ui-color-shader-program
                    :anchor anchor
                    :layer 2
                    :depth depth)))

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
         (register-input-char :y #\y) (register-input-char :z #\z) (register-input-char :space #\Space)
         (register-input-char :0 #\0) (register-input-char :1 #\1) (register-input-char :2 #\2)
         (register-input-char :3 #\3) (register-input-char :4 #\4) (register-input-char :5 #\5)
         (register-input-char :6 #\6) (register-input-char :7 #\7) (register-input-char :8 #\8)
         (register-input-char :9 #\9) (register-input-char :! #\!) (register-input-char :? #\?)
         (register-input-char :. #\.) (register-input-char '|:,| #\,)))
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
