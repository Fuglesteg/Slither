(uiop:define-package #:slither/utils
  (:use #:cl
        #:org.shirakumo.fraf.math.matrices
        #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames (:glfw :org.shirakumo.fraf.glfw))
  (:export :continuable
           :symbol->camel-case
           :function-symbol->global-variable-symbol
           :defmemo
           :random-color
           :degrees->radians
           :rotation->vec2
           :random-float
           :random-element
           :clamp
           :smoothstep
           :ensure-non-keyword-symbol
           :lambda-list-bindings
           :vec2->rotation
           :mat2->mat3
           :safe-vscale
           :lerp
           :rotation-lerp
           :vector-read-integer
           :integer->byte-array))

(in-package #:slither/utils)

(defun safe-vscale (a s)
  (let ((sum (+ (vx a) (vy a))))
    (if (< -1.0e-24 sum 1.0e-24)
        a
        (vscale a s))))

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

(defun lerp (start end step)
  (+ (* (- end start) (clamp step 0 1))
     start))

(defun rotation-lerp (start end step)
  (let ((diff (- (mod (+ (- end start) 540) 360) 180)))
    (mod (+ start
            (* diff step)
            360)
         360)))

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

(defun degrees->radians (degrees)
  (* degrees (/ pi 180)))

(defun rotation->vec2 (degrees)
  (let ((radians (degrees->radians degrees)))
    (vec2 (cos radians)
          (sin radians))))

(defun random-float (&optional (range 1) (precision 10))
  (float (* (/ (random precision) precision) range)))

(defun mat2->mat3 (mat2)
  (let ((mat3 (meye 3)))
    (mtransfer mat3 mat2 :w 2 :h 2)
    mat3))

(defun randomly-negate (number)
  (case (random 2)
    (0 (- number))
    (1 number)))

(defun radians->degrees (radians)
  (* radians (/ 180 pi)))

(defun vec2->rotation (vec)
  (radians->degrees (atan (vx vec)
                          (vy vec))))

(defmethod angle-towards ((from vec2) (to vec2))
  (vec2->rotation (vscale (v- to from) 1)))

(defun random-element (list)
  (nth (random (length list)) list))

(defun random-color ()
  (vec3 (random-float) (random-float) (random-float)))

(defun lambda-list-bindings (lambda-list)
  "Gets the binding symbols from a lambda list"
  (let (bindings parsing-keys)
    (loop for argument in lambda-list
          do (let ((binding (cond ((eq argument '&key) (setf parsing-keys t) nil)
                                  ((member argument lambda-list-keywords) nil)
                                  ((symbolp argument) argument)
                                  ((consp argument) (car argument))
                                  (t nil))))
               (when binding
                 (when parsing-keys
                   (push (intern (symbol-name binding) :keyword) bindings))
                 (push binding bindings))))
    (nreverse bindings)))

(defun ensure-non-keyword-symbol (symbol)
  (etypecase symbol
    (keyword (intern (symbol-name symbol)))
    (symbol symbol)
    (string (intern symbol))))

(declaim (ftype (function ((vector (unsigned-byte 8)) &key (:bytes integer)) integer)
                vector-read-integer))
(defun vector-read-integer (vector &key (bytes 1))
  "Read the amount of bytes from vector as one integer"
  (apply #'logior (loop for byte below bytes
                        collect (ash (aref vector byte) (- (* (1- bytes) 8)
                                                           (* byte 8))))))

(declaim (ftype (function (integer) (vector (unsigned-byte 8) 4))
                integer->byte-array))
(defun integer->byte-array (integer)
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents (list (ldb (byte 8 24) integer)
                                      (ldb (byte 8 16) integer)
                                      (ldb (byte 8 8) integer)
                                      (ldb (byte 8 0) integer))))
