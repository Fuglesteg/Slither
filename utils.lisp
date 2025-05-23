(uiop:define-package #:slither/utils
  (:use #:cl 
        #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :continuable
           :symbol->camel-case
           :function-symbol->global-variable-symbol
           :defmemo
           :random-color
           :rotation->vec2
           :random-float
           :random-element
           :clamp
           :smoothstep))

(in-package #:slither/utils)

(defmacro defmemo (name &body body)
  (let ((memo (gensym "MEMO")))
    `(let (,memo)
       (defun ,name ()
         (unless ,memo
           (setf ,memo
                 (progn ,@body)))
         ,memo))))

(defun clamp (value min max)
  (cond
    ((< value min) min)
    ((> value max) max)
    (t value)))

(defun smoothstep (value edge0 edge1)
  (let ((value (clamp (/ (- value edge0) (- edge1 edge0)) 0 1)))
    (* value value (- 3.0 (* 2.0 value)))))

(defun symbol->camel-case (symbol)
   (loop for character across (string symbol)
         with should-capitalize = nil
         when (char= character #\-)
         do (setf should-capitalize t)
         else
         collect (if should-capitalize
                     (char-upcase character)
                     (char-downcase character))
         into result
         and do (setf should-capitalize nil)
         finally (return (coerce result 'string))))

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun rotation->vec2 (degrees)
  (let ((radians (degrees->radians degrees)))
    (vec2 (cos radians)
          (sin radians))))

(defun degrees->radians (degrees)
  (* degrees (/ pi 180)))

(defun random-float (&optional (range 1) (precision 10))
  (float (* (/ (random precision) precision) range)))

(defun randomly-negate (number)
  (case (random 2)
    (0 (- number))
    (1 number)))

(defun radians->degrees (radians)
  (* radians (/ 180 pi)))

(defmethod angle-towards ((from vec2) (to vec2))
  (with-vector (x y) (direction (vscale (v- to from) 1))
    (radians->degrees (atan x y))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun random-color ()
  (vec3 (random-float) (random-float) (random-float)))