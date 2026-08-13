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
           :text-input
           :ui-draw-rectangle
           :box
           :image
           :text
           :ui-layout
           :ui-layout-render
           :button
           :find-ui-component
           :ui-layout-update
           :define-ui-component
           :element-hovered-p
           :element-clicked-p
           :current-element
           :ui-mouse-position))

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
               (:depth integer)
               (:anchor anchor)))
(defun draw-text (text position &key (rotation 0)
                                     (size (vec2 0.1))
                                     (shader-program array-texture-shader-program)
                                     (layer 0)
                                     (depth 0)
                                     (anchor :top-left))
  (declare (type vec2 position)
           (type string text)
           (type number rotation)
           (type vec2 size))
  (let ((row 0)
        (column 0))
    (loop for char across text
          do (cond
               ((char= char #\Newline)
                (incf row)
                (setf column 0))
               ((char= char #\Space)
                (incf column))
               (t (let* ((offset (v* (vec2 column (- (* row 1.6))) size))
                         (rotated-offset (let* ((radians (degrees->radians rotation))
                                                (cos (cos radians))
                                                (sin (sin radians)))
                                           (vec2 (- (* (vx offset) cos)
                                                    (* (vy offset) sin))
                                                 (+ (* (vx offset) sin)
                                                    (* (vy offset) cos))))))
                    (draw-array-texture (v+ position rotated-offset)
                                        size
                                        (char->font-index char)
                                        font
                                        :color (vec4 1.0 1.0 1.0 1.0)
                                        :rotation rotation
                                        :layer layer
                                        :depth depth
                                        :shader-program shader-program
                                        :anchor anchor)
                    (incf column)))))))

(-> ui-draw-text (string vec2 &key
                         (:size vec2)
                         (:layer integer)
                         (:depth integer)
                         (:anchor anchor)))
(defun ui-draw-text (text position &key (size (vec2 0.1))
                                        (layer 2)
                                        (depth 0)
                                        (anchor :top-left))
  (declare (type vec2 position size)
           (type string text))
  (draw-text text
             position
             :rotation 0
             :size (v/ size 2)
             :depth depth
             :layer layer
             :shader-program ui-array-texture-shader-program
             :anchor anchor))

(defun ui-draw-rectangle (position size &key (color (vec4 1.0 0.0 0.0 1.0))
                                             (anchor :left)
                                             (depth 0))
  (when slither/render::*initialized*
    (draw-rectangle position
                    (v/ size 2)
                    color
                    :shader-program ui-color-shader-program
                    :anchor anchor
                    :layer 2
                    :depth depth)))

(defun ui-draw-texture (texture position size &key (color (vec4 1.0))
                                                   (anchor :top-left)
                                                   (depth 0)
                                                   (rotation 0))
  (when slither/render::*initialized*
    (draw-texture position
                  (v/ size 2)
                  texture
                  :shader-program ui-texture-shader-program
                  :rotation rotation
                  :color color
                  :depth depth
                  :layer 2
                  :anchor anchor)))

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

  (rotation 0.0 :type single-float)

  (z-index 0 :type fixnum)

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

  (text-size 20.0 :type single-float)
  (text-content "" :type string)
  (texture nil :type (or texture null)))

(defmacro define-ui-element (name type &key constructor parameters)
  `(defmacro ,name ((&key (x 0.0) (y 0.0)
                          (x-alignment :left)
                          (y-alignment :top)
                          (width nil)
                          (height nil)
                          (padding-left 0.0) (padding-right 0.0)
                          (padding-top 0.0) (padding-bottom 0.0)
                          (child-gap 0.0)
                          (background-color (vec4 0.1 0.3 0.1 0.0))
                          (layout-direction :left-to-right)
                          (rotation 0.0)
                          (z-index 0)
                          ,@parameters)
                    &body children)
     ,(alexandria:with-gensyms (ui-element)
        `(flet ((parse-size-definition (size-definition)
                 (etypecase size-definition
                   (null (list :size-type :fit
                               :size 0.0
                               :min 0.0
                               :max most-positive-single-float))
                   (keyword (list :size-type size-definition
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
                                   :size-type size-type)))
                   (t (list :size size-definition
                            :min 0.0
                            :max most-positive-single-float
                            :size-type :fixed)))))
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
                     (sb-vm:without-arena
                       (sb-vm:with-arena (*ui-arena*)
                         (make-ui-element :type ,,type
                                          :layout-direction ,layout-direction
                                          :x ,x
                                          :x-alignment ,x-alignment
                                          :y ,y
                                          :y-alignment ,y-alignment
                                          :width ,width
                                          :width-min ,width-min
                                          :width-max ,width-max
                                          :width-size-type ,width-size-type
                                          :height ,height
                                          :height-min ,height-min
                                          :height-max ,height-max
                                          :height-size-type ,height-size-type
                                          :rotation ,rotation
                                          :z-index ,z-index
                                          :padding-left ,padding-left
                                          :padding-right ,padding-right
                                          :padding-top ,padding-top
                                          :padding-bottom ,padding-bottom
                                          :background-color ,background-color
                                          :child-gap ,child-gap
                                          :children (alexandria:flatten (list ,@children))
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
                                                             parameter-symbol))))))))
                (dolist (child (ui-element-children ,',ui-element))
                  (setf (ui-element-parent child) ,',ui-element))
                ,',(when constructor
                     `(funcall ,constructor ,ui-element))
                ,',ui-element))))))

(define-ui-element box :box)

(define-ui-element text :text
  :parameters ((text-content "") (text-size 20.0))
  :constructor (lambda (ui-element)
                 (setf (ui-element-width-size-type ui-element) :fixed)
                 (setf (ui-element-height-size-type ui-element) :fixed)
                 (setf (ui-element-height ui-element)
                       (ui-element-text-size ui-element))
                 (setf (ui-element-width ui-element)
                       (* (length (ui-element-text-content ui-element))
                          (ui-element-text-size ui-element)
                          0.5))
                 ; Minimum width is equal to length of longest word
                 (setf (ui-element-width-min ui-element)
                       (* (apply #'max
                              (mapcar #'length
                                      (split-sequence:split-sequence #\Space
                                                                     (ui-element-text-content ui-element))))
                          (ui-element-text-size ui-element)
                          0.5))))

(define-ui-element image :image
  :parameters ((texture nil)))

(defun ui-element-calculate-layout (root-ui-element)
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
      (when (and (ui-element-children ui-element)
                 (eq (ui-element-layout-direction ui-element)
                     :left-to-right))
        (incf (ui-element-width ui-element)
              (* (ui-element-child-gap ui-element)
                 (1- (length (ui-element-children ui-element))))))
      (when-let ((parent (ui-element-parent ui-element)))
        (case (ui-element-layout-direction parent)
          (:left-to-right
           (setf (ui-element-width parent)
                 (min (ui-element-width-max parent)
                      (+ (ui-element-width parent)
                         (ui-element-width ui-element))))
           #+nil(incf (ui-element-width-min parent)
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
                  (ui-element-width-min parent)))))
        (setf (ui-element-width ui-element)
              (clamp (ui-element-width ui-element)
                     (ui-element-width-min ui-element)
                     (ui-element-width-max ui-element)))))
    ; 2. Grow & Shrink widths
    (labels ((grow-child-widths (element)
               (when (ui-element-children element)
                 (let ((growables (remove-if-not
                                   (lambda (element)
                                     (eq (ui-element-width-size-type element)
                                         :grow))
                                   (ui-element-children element))))
                   (case (ui-element-layout-direction element)
                     (:top-to-bottom
                      (dolist (child growables)
                        (setf (ui-element-width child)
                              (- (ui-element-width element)
                                 (ui-element-padding-left element)
                                 (ui-element-padding-right element)))))
                     (:left-to-right
                      (let ((remaining-width (- (ui-element-width element)
                                                (ui-element-padding-left element)
                                                (ui-element-padding-right element)
                                                (loop for child in (ui-element-children element)
                                                      sum (ui-element-width child))
                                                (* (max 0 (1- (length (ui-element-children element))))
                                                   (ui-element-child-gap element)))))
                        ; Grow
                        (loop while (and growables (< 0 remaining-width))
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
                                                                  previous-width)))))))
                        ; Shrink
                        (loop while (and growables (> 0 remaining-width))
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
                                                                  previous-width)))))))))))
                 (dolist (child (ui-element-children element))
                   (grow-child-widths child)))))
      (grow-child-widths root-ui-element))
    ; 3. Wrap text
    (labels ((wrap-text (ui-element)
               (when (eq (ui-element-type ui-element)
                         :text)
                 (let ((characters-per-line (floor (ui-element-width ui-element)
                                                   (/ (ui-element-text-size ui-element))))
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
                               (setf last-separator-index i)
                               (incf current-width))
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
      (when (and (ui-element-children ui-element)
                 (eq (ui-element-layout-direction ui-element)
                     :top-to-bottom))
        (incf (ui-element-height ui-element)
              (* (ui-element-child-gap ui-element)
                 (1- (length (ui-element-children ui-element))))))
      (when-let ((parent (ui-element-parent ui-element)))
        (when (eq (ui-element-height-size-type parent)
                  :fit)
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
             (setf (ui-element-height parent)
                   (min
                    (ui-element-height-max parent)
                    (+ (ui-element-height parent)
                       (ui-element-height ui-element))))
             (incf (ui-element-height-min parent)
                   (ui-element-height-min ui-element)))))))
    ; 5. Grow & Shrink heights
    (labels ((grow-child-heights (element)
               (when (ui-element-children element)
                 (let ((growables (remove-if-not
                                   (lambda (element)
                                     (eq (ui-element-height-size-type element)
                                         :grow))
                                   (ui-element-children element))))
                   (case (ui-element-layout-direction element)
                     (:left-to-right
                      (dolist (child growables)
                        (setf (ui-element-height child)
                              (- (ui-element-height element)
                                 (ui-element-padding-top element)
                                 (ui-element-padding-bottom element)))))
                     (:top-to-bottom
                      (let ((remaining-height (if (eq (ui-element-layout-direction element)
                                                      :left-to-right)
                                                  (ui-element-height element)
                                                  (- (ui-element-height element)
                                                     (ui-element-padding-top element)
                                                     (ui-element-padding-bottom element)
                                                     (loop for child in (ui-element-children element)
                                                           sum (ui-element-height child))
                                                     (* (max 0 (1- (length (ui-element-children element))))
                                                        (ui-element-child-gap element))))))
                        ; Grow
                        (when growables
                          (loop while (< 0 remaining-height)
                                do (let ((smallest-height (ui-element-height (first growables)))
                                         (second-smallest-height most-positive-single-float)
                                         (height-to-add remaining-height))
                                     (dolist (child growables)
                                       (when (< (ui-element-height child) smallest-height)
                                         (setf second-smallest-height smallest-height)
                                         (setf smallest-height (ui-element-height child)))
                                       (when (> (ui-element-height child) smallest-height)
                                         (setf second-smallest-height
                                               (min (ui-element-height child)
                                                    second-smallest-height))
                                         (setf height-to-add (- second-smallest-height
                                                                smallest-height))))
                                     (setf height-to-add (min height-to-add
                                                              (/ remaining-height
                                                                 (length growables))))
                                     (dolist (child growables)
                                       (let ((previous-height (ui-element-height child)))
                                         (when (= (ui-element-height child)
                                                  smallest-height)
                                           (incf (ui-element-height child)
                                                 height-to-add)
                                           (when (>= (ui-element-height child)
                                                     (ui-element-height-max child))
                                             (setf (ui-element-height child)
                                                   (ui-element-height-max child))
                                             (setf growables (remove child growables)))
                                           (decf remaining-height (- (ui-element-height child)
                                                                     previous-height))))))))
                        ; Shrink
                        (when growables
                          (loop while (and growables (> 0 remaining-height))
                                do (let ((largest-height (ui-element-height (first growables)))
                                         (second-largest-height 0)
                                         (height-to-add remaining-height))
                                     (dolist (child growables)
                                       (when (> (ui-element-height child) largest-height)
                                         (setf second-largest-height largest-height)
                                         (setf largest-height (ui-element-height child)))
                                       (when (< (ui-element-height child) largest-height)
                                         (setf second-largest-height
                                               (max (ui-element-height child)
                                                    second-largest-height))
                                         (setf height-to-add (- second-largest-height
                                                                largest-height))))
                                     (setf height-to-add (max height-to-add
                                                              (/ remaining-height
                                                                 (length growables))))
                                     (dolist (child growables)
                                       (let ((previous-height (ui-element-height child)))
                                         (when (= (ui-element-height child)
                                                  largest-height)
                                           (incf (ui-element-height child)
                                                 height-to-add)
                                           (when (<= (ui-element-height child)
                                                     (ui-element-height-min child))
                                             (setf (ui-element-height child)
                                                   (ui-element-height-min child))
                                             (setf growables (remove child growables)))
                                           (decf remaining-height (- (ui-element-height child)
                                                                     previous-height))))))))))))
                 (dolist (child (ui-element-children element))
                   (grow-child-heights child)))))
      (grow-child-heights root-ui-element))
    ; 6. Positions & Alignments
    (labels ((position-children (parent)
               (flet ((alignment-offset (alignment remaining)
                        (if (<= remaining 0)
                            0
                            (ecase alignment
                              ((:left :top) 0)
                              (:center (/ remaining 2))
                              ((:right :bottom) remaining)))))
                 (let* ((left-to-right (eq (ui-element-layout-direction parent)
                                           :left-to-right))
                        (children (ui-element-children parent))
                        (parent-content-width (- (ui-element-width parent)
                                                 (ui-element-padding-left parent)
                                                 (ui-element-padding-right parent)))
                        (parent-content-height (- (ui-element-height parent)
                                                  (ui-element-padding-top parent)
                                                  (ui-element-padding-bottom parent)))
                        (total-child-gap (* (ui-element-child-gap parent)
                                            (max 0 (1- (length children)))))
                        (total-children-extent (+ total-child-gap
                                                  (loop for child in children
                                                        sum (if left-to-right
                                                                (ui-element-width child)
                                                                (ui-element-height child)))))
                        (main-axis-alignment (if left-to-right
                                                 (ui-element-x-alignment parent)
                                                 (ui-element-y-alignment parent)))
                        (cross-axis-alignment (if left-to-right
                                                  (ui-element-y-alignment parent)
                                                  (ui-element-x-alignment parent)))
                        (offset (+ (if left-to-right
                                       (ui-element-padding-left parent)
                                       (ui-element-padding-top parent))
                                   (alignment-offset
                                    main-axis-alignment
                                    (- (if left-to-right
                                           parent-content-width
                                           parent-content-height)
                                       total-children-extent)))))
                   (dolist (child children)
                     (let ((position-along-axis (+ (if left-to-right
                                                       (ui-element-x parent)
                                                       (ui-element-y parent))
                                                   offset))
                           (position-across-axis (+ (if left-to-right
                                                        (ui-element-y parent)
                                                        (ui-element-x parent))
                                                    (if left-to-right
                                                        (ui-element-padding-top parent)
                                                        (ui-element-padding-left parent))
                                                    (alignment-offset
                                                     cross-axis-alignment
                                                     (- (if left-to-right
                                                            parent-content-height
                                                            parent-content-width)
                                                        (if left-to-right
                                                            (ui-element-height child)
                                                            (ui-element-width child)))))))
                       (cond
                         (left-to-right
                          (incf (ui-element-x child) position-along-axis)
                          (incf (ui-element-y child) position-across-axis))
                         (t ; top-to-bottom
                          (incf (ui-element-y child) position-along-axis)
                          (incf (ui-element-x child) position-across-axis))))
                     (incf offset (+ (if left-to-right
                                         (ui-element-width child)
                                         (ui-element-height child))
                                     (ui-element-child-gap parent)))
                     (position-children child))))))
      (position-children root-ui-element))
    root-ui-element))

(defun ui-element-draw (ui-element &optional (sort-index 0))
  (incf sort-index (ui-element-z-index ui-element))
  (ecase (ui-element-type ui-element)
    (:box
        (ui-draw-rectangle (vec2 (ui-element-x ui-element)
                                 (- (ui-element-y ui-element)))
                           (vec2 (ui-element-width ui-element)
                                 (ui-element-height ui-element))
                           :color (ui-element-background-color ui-element)
                           :anchor :top-left
                           :depth sort-index))
    (:text
        (ui-draw-text (ui-element-text-content ui-element)
                      (vec2 (ui-element-x ui-element)
                            (- (ui-element-y ui-element)))
                      :size (vec2 (ui-element-text-size ui-element))
                      :anchor :top
                      :depth sort-index))
    (:image
        (let* ((texture (ui-element-texture ui-element))
               (texture-width (texture-width texture))
               (texture-height (texture-height texture))
               (element-width (ui-element-width ui-element))
               (element-height (ui-element-height ui-element))
               (scale (min (/ (max element-width 1.0) texture-width)
                           (/ (max element-height 1.0) texture-height)
                           1.0))
               (draw-width (* texture-width scale))
               (draw-height (* texture-height scale))
               (offset-x (/ (- element-width draw-width) 2))
               (offset-y (/ (- element-height draw-height) 2)))
          (ui-draw-texture texture
                           (vec2 (+ (ui-element-x ui-element) offset-x)
                                 (- (+ (ui-element-y ui-element) offset-y)))
                           (vec2 draw-width draw-height)
                           :anchor :top-left
                           :depth sort-index
                           :rotation (ui-element-rotation ui-element)))))
  (incf sort-index)
  (dolist (child (ui-element-children ui-element))
    (ui-element-draw child (1+ sort-index))))

(defvar *ui-arena* (sb-vm:new-arena (* 16 1024 1024))) ; 16 MBs
(defvar *ui-arena-previous* (sb-vm:new-arena (* 16 1024 1024)))

(defun swap-arenas ()
  (rotatef *ui-arena*
           *ui-arena-previous*))

(defvar *ui-layout* nil)

(defun ui-layout-render ()
  (when *ui-layout*
    (dolist (root-ui-element *ui-layout*)
      (ui-element-draw root-ui-element))))

(defun ui-layout-update ()
  (setf *ui-layout* nil)
  (sb-vm:rewind-arena *ui-arena*)
  (swap-arenas)
  (clear-components))

(defmacro ui-layout (&body body)
  `(progn
     (push (ui-element-calculate-layout
            ,@body)
           *ui-layout*)))

(defun ui-mouse-position ()
  (handler-case
      (v* (m* (minv slither/render::*ui-view-matrix*)
              (normalized-screen-space-mouse-position))
          (vec2 1.0 -1.0))
    (error () (vec2))))

(defun mouse-hover-p (position size)
  (v< position
      (ui-mouse-position)
      (v+ position size)))

(defclass ui-component ()
  ((id
   :type string
   :initarg :id
   :accessor ui-component-id)
   (previous-element-result
    :type ui-element
    :accessor ui-component-previous-element-result)))

(defmethod slot-unbound (class (ui-component ui-component) slot-name)
  nil)

(defvar *ui-components* (make-hash-table :test 'equal))

(defun register-ui-component (ui-component)
  (setf (gethash (ui-component-id ui-component)
                 *ui-components*)
        ui-component))

(defun ui-component-registered-p (ui-component)
  (find-ui-component (ui-component-id ui-component)))

(defun find-ui-component (id)
  (gethash id *ui-components*))

(defvar *current-element* nil)

(defun current-element ()
  (when (and *current-element*
             (typep *current-element* 'ui-element))
    *current-element*))

(defun element-hovered-p ()
  (when-let ((current-element (current-element)))
      (mouse-hover-p (vec2 (ui-element-x current-element)
                           (ui-element-y current-element))
                     (vec2 (ui-element-width current-element)
                           (ui-element-height current-element)))))

(defun element-clicked-p ()
  (and (element-hovered-p)
       (key-pressed-p :left-click)))

(defvar *current-component* nil)
(defun current-component ()
  *current-component*)

(defvar *currently-used-components* nil)

(defun clear-components ()
  (do-hash-table (id component *ui-components*)
    (declare (ignore component))
    (unless (member id *currently-used-components*)
      (remhash id *ui-components*)))
  (setf *currently-used-components* nil))

(defmacro define-ui-component (name options &body body)
  (let* ((data (getf options :data))
         (props (getf options :props))
         (function-symbol (intern (format nil "~a%" (symbol-name name))))
         (props-keyword-list (mapcan (lambda (prop)
                                       (let ((prop (etypecase prop
                                                     (symbol prop)
                                                     (list (first prop)))))
                                         (list (intern (symbol-name prop)
                                                       :keyword)
                                               prop)))
                                     props)))
    `(progn
       (defclass ,name (ui-component)
         ,data)
       (defun ,function-symbol (&key ,@(mapcan (lambda (prop)
                                                 (list
                                                  (etypecase prop
                                                    (list (first prop))
                                                    (symbol prop))))
                                        props))
         (with-slots ,data (current-component)
           (sb-vm:without-arena
             ,@body)))
       (defmacro ,name ((&key id ,@props) &rest children)
         (declare (ignore children))
         `(let* ((component (or (find-ui-component ,id)
                                (let ((ui-component (make-instance ',',name :id ,id)))
                                  (register-ui-component ui-component)
                                  ui-component)))
                 (*current-component* component)
                 (*current-element* (ui-component-previous-element-result component)))
            (push ,id *currently-used-components*)
            (let ((element-result (,',function-symbol
                                   ,,@props-keyword-list)))
              (setf (ui-component-previous-element-result component)
                    element-result)
              element-result))))))

(defvar *selected-input* nil)

(define-ui-component text-input
    (:data (text)
     :props ((background-color (vec4 0.1 0.1 0.1 1.0))))
  (when (element-clicked-p)
    (setf *selected-input* (current-component)))
  (let ((selectedp (eq *selected-input* (current-component))))
    (when selectedp
      (flet ((register-input-char (input-keyword-symbol character)
               (when (key-pressed-p input-keyword-symbol)
                 (setf text (format nil "~a~a"
                                    (or text "")
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
        (register-input-char :period #\.) (register-input-char :comma #\,) (register-input-char :colon #\:))
      (when (key-pressed-p :backspace)
        (when (< 0 (length text))
          (setf text
                (subseq text
                        0
                        (1- (length text)))))))
    (box (:width (:fit 0.0 :min 200.0)
          :padding-left 10.0
          :padding-right 10.0
          :padding-top 10.0
          :padding-bottom 10.0
          :height 50.0
          :background-color (if selectedp
                                (v+ background-color
                                    (vec4 0.1))
                                background-color))
      (text (:background-color (vec4 1.0 1.0 1.0 1.0)
             :text-size 50.0
             :text-content (or text "")))
      (and selectedp
           (box (:width 3.0
                 :height 40.0
                 :background-color (vec4 1.0)))))))

(define-ui-component button
    (:props ((on-click '#'identity)
             (button-text "Button")
             (x-alignment :left)
             (y-alignment :top)))
  (when (element-clicked-p)
    (funcall on-click (current-component)))
  (box (:padding-left 10.0
        :padding-right 10.0
        :padding-top 10.0
        :padding-bottom 10.0
        :background-color (if (element-hovered-p)
                              (vec4 0.2 0.2 0.2 1.0)
                              (vec4 0.1 0.1 0.1 1.0))
        :x-alignment x-alignment
        :y-alignment y-alignment)
    (text (:text-content button-text
           :text-size 50.0))))
