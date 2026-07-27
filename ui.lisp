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
  (let* ((radians (degrees->radians rotation))
         (cos-r (cos radians))
         (sin-r (sin radians)))
    (loop for char across text
          for i from 0
          do (unless (char= char #\Space)
               (let* ((offset (calculate-text-offset i :character-size size))
                      (rotated-offset (vec2 (- (* (vx offset) cos-r)
                                               (* (vy offset) sin-r))
                                            (+ (* (vx offset) sin-r)
                                               (* (vy offset) cos-r)))))
                 (draw-array-texture (v+ position rotated-offset)
                                     size
                                     (char->font-index char)
                                     font
                                     :color (vec4 1.0 1.0 1.0 1.0)
                                     :rotation rotation
                                     :layer layer
                                     :depth depth
                                     :shader-program shader-program))))))

(defun calculate-text-offset (text-index &key (row-length 30) (character-size (vec2 0.02)))
  (with-vec (character-width character-height) character-size
    (let ((whitespace character-width))
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
  (declare (type vec2 position size)
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
  (:fixed-tick
   (when (and (mouse-hover-p (button-position)
                             (v* (ui-calculate-text-offset (length (button-text))
                                                           :character-size (button-size))
                                 (vec2 0.5 1))
                             :anchor :bottom-left)
              (key-pressed-p :left-click))
     (funcall (button-on-click) *behavior*)))
  (:tick
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
  (:fixed-tick
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
         (register-input-char :period #\.) (register-input-char :comma #\,) (register-input-char :colon #\:)))
     (when (key-pressed-p :backspace)
       (when (< 0 (length (text-input-text)))
         (setf (text-input-text)
               (subseq (text-input-text)
                       0
                       (1- (length (text-input-text)))))))))
  (:tick
   (ui-draw-text-input (text-input-text)
                       (text-input-position)
                       :size (text-input-size)
                       :focus (text-input-focus))))

;; Layouting algorithm
;; Yoinked from Clay
;; 1. Fit sizing widths
;; 2. Grow widths
;; 3. Wrap text
;; 4. Fit sizing heights
;; 5. Grow heights
;; 6. Positions & Alignments
;; 7. Draw Calls

;; Macros vs. Functions vs. Tree
;; Macros can call a closing function, giving free breadth first reverse search
;; Functions might be easier to reason with and to implement for users
;; Lisp already has functionality for tree expressions

(defvar *layout* nil)
(defmacro with-layout (layout &body body)
  `(let ((*layout* ,layout))
     ,@body))

(deftype ui-element-type ()
  '(member :box :image :text))

(deftype ui-element-size-type ()
  '(member :fixed :fit :grow))

(deftype ui-element-layout-direction ()
  '(member :left-to-right :top-to-bottom))

(deftype x-alignment ()
  '(member :left :center :right))

(deftype y-alignment ()
  '(member :top :center :bottom))

(defstruct ui-element
  (type :box :type ui-element-type)

  (x 0.0 :type single-float)
  (x-alignment :left :type x-alignment)
  (y 0.0 :type single-float)
  (y-alignment :top :type y-alignment)

  (width-size-type :fit :type ui-element-size-type)
  (width 0.0 :type single-float)
  (width-min 0.0 :type single-float)
  (width-max most-positive-single-float :type single-float)
  (height-size-type :fit :type ui-element-size-type)
  (height 0.0 :type single-float)
  (height-min 0.0 :type single-float)
  (height-max most-positive-single-float :type single-float)

  (layout-direction :left-to-right :type ui-element-layout-direction)

  (background-color (vec4 0.1 0.3 0.1 0.0) :type vec4)

  (padding-left 0.0 :type single-float)
  (padding-right 0.0 :type single-float)
  (padding-top 0.0 :type single-float)
  (padding-bottom 0.0 :type single-float)

  (child-gap 0.0 :type single-float)

  (parent nil :type (or null ui-element))
  (children nil :type list)

  (text-size 0.2 :type single-float)
  (text-content "" :type string))

(defmacro define-ui-element (name type &key constructor parameters)
  `(defmacro ,name ((&key (x 0.0) (y 0.0)
                          (width nil)
                          (height nil)
                          (padding-left 0.0) (padding-right 0.0)
                          (padding-top 0.0) (padding-bottom 0.0)
                          (child-gap 0.0)
                          (background-color (vec4 0.1 0.3 0.1 0.0))
                          ,@parameters)
                    &body children)
     ,(alexandria:with-gensyms (ui-element)
        `(flet ((parse-size-definition (size-definition)
                 (etypecase size-definition
                   (null (list :size-type :fit
                               :size 0.0
                               :min 0.0
                               :max most-positive-single-float))
                   (number (list :size size-definition
                                 :min size-definition
                                 :max size-definition
                                 :size-type :fixed))
                   (cons (destructuring-bind (size-type size &key (min 0.0)
                                                        (max most-positive-single-float)) size-definition
                             (list :size size
                                   :min (if (eq size-type :fixed) size min)
                                   :max (if (eq size-type :fixed) size max)
                                   :size-type size-type))))))
          (let* ((width-size-definition (parse-size-definition width))
                 (width (getf width-size-definition :size))
                 (width-size-type (getf width-size-definition :size-type))
                 (width-min (getf width-size-definition :min))
                 (width-max (getf width-size-definition :max))
                 (height-size-definition (parse-size-definition height))
                 (height (getf height-size-definition :size))
                 (height-size-type (getf height-size-definition :size-type))
                 (height-min (getf height-size-definition :min))
                 (height-max (getf height-size-definition :max)))
            `(let ((,',ui-element
                      (make-ui-element :type ,,type
                                       :x ,x
                                       :y ,y
                                       :width ,width
                                       :width-min ,width-min
                                       :width-max ,width-max
                                       :width-size-type ,width-size-type
                                       :height ,height
                                       :height-min ,height-min
                                       :height-max ,height-max
                                       :height-size-type ,height-size-type
                                       :padding-left ,padding-left
                                       :padding-right ,padding-right
                                       :padding-top ,padding-top
                                       :padding-bottom ,padding-bottom
                                       :background-color ,background-color
                                       :child-gap ,child-gap
                                       :children (list ,@children)
                                       ,,@(loop for parameter in parameters
                                                append (let ((parameter-symbol
                                                               (etypecase parameter
                                                                 (list (first parameter))
                                                                 (symbol parameter))))
                                                         (list
                                                          (intern
                                                           (string-upcase
                                                            (symbol-name parameter-symbol))
                                                           'keyword)
                                                          parameter-symbol))))))
                (dolist (child (ui-element-children ,',ui-element))
                  (setf (ui-element-parent child) ,',ui-element))
                ,',(when constructor
                     `(funcall ,constructor ,ui-element))
                ,',ui-element))))))

(define-ui-element box :box)

(define-ui-element text :text
  :parameters ((text-content "") (text-size 0.2))
  :constructor (lambda (ui-element)
                 (setf (ui-element-width ui-element)
                       (* (length (ui-element-text-content ui-element))
                          (ui-element-text-size ui-element)))
                 ; Minimum width is equal to length of longest word
                 (setf (ui-element-width-min ui-element)
                       (* (apply #'max
                              (mapcar #'length
                                      (split-sequence:split-sequence #\Space
                                                                     (ui-element-text-content ui-element))))
                          (ui-element-text-size ui-element)))))

(define-ui-element image :image)

(defun calculate-layout (root-ui-element)
  ; Main algorithm based on Clay, See: https://github.com/nicbarker/clay
  (let ((children-first-sorted-layout
          (labels ((sort-by-child-first-order (ui-element)
                     (cond
                       ((null (ui-element-children ui-element))
                        (list ui-element))
                       (t
                        (nconc (mapcan #'sort-by-child-first-order
                                       (ui-element-children ui-element))
                               (list ui-element))))))
            (sort-by-child-first-order root-ui-element))))
    ; 1. Fit sizing widths
    (dolist (ui-element children-first-sorted-layout)
      (incf (ui-element-width ui-element)
            (+ (ui-element-padding-left ui-element)
               (ui-element-padding-right ui-element)))
      (let ((total-child-gap (* (1- (length (ui-element-children ui-element)))
                                (ui-element-child-gap ui-element))))
        (incf (ui-element-width ui-element)
              total-child-gap)
        (when-let ((parent (ui-element-parent ui-element)))
          (when (eq (ui-element-width-size-type parent)
                    :fit)
            (case (ui-element-layout-direction parent)
              (:left-to-right
               (setf (ui-element-width parent)
                     (min (ui-element-width-max parent)
                          (+ (ui-element-width parent) (ui-element-width ui-element))))
               (incf (ui-element-width-min parent)
                     (ui-element-width-min ui-element)))
              (:top-to-bottom
               (setf (ui-element-width parent)
                     (min (ui-element-width-max parent)
                          (max
                           (ui-element-width parent)
                           (ui-element-width ui-element))))
               (setf (ui-element-width-min parent)
                     (max
                      (ui-element-width-min ui-element)
                      (ui-element-width-min parent)))))))))
    ; 2. Grow & Shrink widths
    (labels ((grow-child-widths (element)
               (when (ui-element-children element)
                 (let ((remaining-width (- (ui-element-width element)
                                           (ui-element-padding-left element)
                                           (ui-element-padding-right element)
                                           (loop for child in (ui-element-children element)
                                                 sum (ui-element-width child))
                                           (* (1- (length (ui-element-children element)))
                                              (ui-element-child-gap element))))
                       (growables (remove-if-not
                                   (lambda (element)
                                     (eq (ui-element-width-size-type element)
                                         :grow))
                                   (ui-element-children element))))
                   ; Grow
                   (when growables
                     (loop while (< 0 remaining-width)
                           do (let ((smallest-width (ui-element-width (first growables)))
                                    (second-smallest-width most-positive-single-float)
                                    (width-to-add remaining-width))
                                (dolist (child growables)
                                  (when (< (ui-element-width child) smallest-width)
                                    (setf second-smallest-width smallest-width)
                                    (setf smallest-width (ui-element-width child)))
                                  (when (> (ui-element-width child) smallest-width)
                                    (setf second-smallest-width
                                          (min (ui-element-width child)
                                               second-smallest-width))
                                    (setf width-to-add (- second-smallest-width
                                                          smallest-width))))
                                (setf width-to-add (min width-to-add
                                                        (/ remaining-width
                                                           (length growables))))
                                (dolist (child growables)
                                  (let ((previous-width (ui-element-width child)))
                                    (when (= (ui-element-width child)
                                             smallest-width)
                                      (incf (ui-element-width child)
                                            width-to-add)
                                      (when (>= (ui-element-width child)
                                                (ui-element-width-max child))
                                        (setf (ui-element-width child)
                                              (ui-element-width-max child))
                                        (setf growables (remove child growables)))
                                      (decf remaining-width (- (ui-element-width child)
                                                               previous-width))))))))
                   ; Shrink
                   (when growables
                     (loop while (> 0 remaining-width)
                           do (let ((largest-width (ui-element-width (first growables)))
                                    (second-largest-width 0)
                                    (width-to-add remaining-width))
                                (dolist (child growables)
                                  (when (> (ui-element-width child) largest-width)
                                    (setf second-largest-width largest-width)
                                    (setf largest-width (ui-element-width child)))
                                  (when (< (ui-element-width child) largest-width)
                                    (setf second-largest-width
                                          (max (ui-element-width child)
                                               second-largest-width))
                                    (setf width-to-add (- second-largest-width
                                                          largest-width))))
                                (setf width-to-add (max width-to-add
                                                        (/ remaining-width
                                                           (length growables))))
                                (dolist (child growables)
                                  (let ((previous-width (ui-element-width child)))
                                    (when (= (ui-element-width child)
                                             largest-width)
                                      (incf (ui-element-width child)
                                            width-to-add)
                                      (when (<= (ui-element-width child)
                                                (ui-element-width-min child))
                                        (setf (ui-element-width child)
                                              (ui-element-width-min child))
                                        (setf growables (remove child growables)))
                                      (decf remaining-width (- (ui-element-width child)
                                                               previous-width)))))))))
                 (dolist (child (ui-element-children element))
                   (grow-child-widths child)))))
      (grow-child-widths root-ui-element))
    ; 3. Wrap text
    (labels ((wrap-text (ui-element)
               (when (eq (ui-element-type ui-element)
                         :text)
                 (let ((characters-per-line (floor (/ (ui-element-width ui-element)
                                                      (ui-element-text-size ui-element))))
                       (separator #\Space)
                       (last-separator-index nil)
                       (current-width 0))
                   (loop for char across (ui-element-text-content ui-element)
                         for i from 0
                         do (cond
                              ((and (< characters-per-line current-width)
                                    last-separator-index)
                               (setf (elt (ui-element-text-content ui-element)
                                          last-separator-index)
                                     #\Newline)
                               (setf current-width (- i last-separator-index))
                               (setf last-separator-index nil))
                              ((char= char separator)
                               (setf last-separator-index i))
                              ((char= char #\Newline)
                               (setf current-width 0))
                              (t
                               (incf current-width))))))
               (dolist (child (ui-element-children ui-element))
                 (wrap-text child))))
      (wrap-text root-ui-element))
    ; 4. Fit sizing heights
    (dolist (ui-element children-first-sorted-layout)
      (incf (ui-element-height ui-element)
            (+ (ui-element-padding-top ui-element)
               (ui-element-padding-bottom ui-element)))
      (when-let ((parent (ui-element-parent ui-element)))
        (let ((total-child-gap (* (1- (length (ui-element-children parent)))
                                  (ui-element-child-gap parent))))
          (case (ui-element-layout-direction parent)
            (:left-to-right
             (setf (ui-element-height parent)
                   (min (ui-element-height-max parent)
                        (max
                         (ui-element-height parent)
                         (ui-element-height ui-element))))
             (setf (ui-element-height-min parent)
                   (max
                    (ui-element-height-min parent)
                    (ui-element-height-min ui-element))))
            (:top-to-bottom
             (incf (ui-element-height ui-element)
                   total-child-gap)
             (setf (ui-element-height parent)
                   (max
                    (ui-element-height-max parent)
                    (+ (ui-element-height parent)
                       (ui-element-height ui-element))))
             (incf (ui-element-height-min parent)
                   (ui-element-height-min ui-element)))))))
    ; 5. Grow heights
    (labels ((grow-child-heights (element)
               (when (ui-element-children element)
                 (let ((remaining-height (- (ui-element-height element)
                                            (ui-element-padding-top element)
                                            (ui-element-padding-bottom element))))
                   (dolist (child (ui-element-children element))
                     (when (eq (ui-element-height-size-type child)
                               :grow)
                       (incf (ui-element-height child)
                             (- remaining-height (ui-element-height child)))
                       (grow-child-heights child)))))))
      (grow-child-heights root-ui-element))
    ; 6. Positions & Alignments
    (labels ((position-children (parent)
               (let ((left-offset (ui-element-padding-left parent)))
                 (dolist (child (ui-element-children parent))
                   (setf (ui-element-x child) (+ (ui-element-x parent) (* left-offset 2)))
                   (setf (ui-element-y child) (- (ui-element-y parent) (* (ui-element-padding-top parent) 2)))
                   (incf left-offset (+ (ui-element-width child)
                                        (ui-element-child-gap parent)))
                   (position-children child)))))
      (position-children root-ui-element))
    root-ui-element))

(defun ui-element-draw (ui-element &optional (sort-index 0))
  (ecase (ui-element-type ui-element)
    (:box
        (ui-draw-rectangle (vec2 (ui-element-x ui-element)
                              (ui-element-y ui-element))
                        (vec2 (ui-element-width ui-element)
                              (ui-element-height ui-element))
                        :color (ui-element-background-color ui-element)
                        :anchor :top-left
                        :depth sort-index))
    (:text
        (ui-draw-text (ui-element-text-content ui-element)
                      (vec2 (ui-element-x ui-element)
                            (ui-element-y ui-element))
                      :depth sort-index)))
  (incf sort-index)
  (dolist (child (ui-element-children ui-element))
    (ui-element-draw child sort-index)))
