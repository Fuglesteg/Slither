(uiop:define-package #:slither/audio
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames (:harmony :org.shirakumo.fraf.harmony)
                    (:mixed :org.shirakumo.fraf.mixed))
  (:import-from #:slither/assets
                #:defasset
                #:register-asset)
  (:export #:sound-play
           #:sound-stop
           #:audio-init
           #:defsound
           #:listener-position))

(in-package #:slither/audio)

(defvar *initialized* nil)
(defvar *run-on-init* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defsound (name path)
    (let ((function
            `(lambda ()
               (register-asset ',name ,path :sound))))
      (if *initialized*
          `(funcall ,function)
          `(push ,function *run-on-init*)))))

(defun audio-init ()
  (harmony:maybe-start-simple-server
   :mixers (list :music
                 (cons :effect 'mixed:plane-mixer)))
  (loop for function in *run-on-init*
        do (funcall function))
  (setf *initialized* t)
  (setf *run-on-init* nil))

(defvar *listener-position* (vec2 0))

(defun listener-position ()
  *listener-position*)

(defun (setf listener-position) (new-position)
  (setf *listener-position* new-position))

(defun sound-play (sound &key position velocity)
  (harmony:play sound
                :reset t
                :location (with-vec (x y) (v- (v* position -1) (listener-position))
                            (list x y))
                :velocity velocity))

(defun sound-stop (sound)
  (harmony:stop sound))

(defclass sound ()
  ((file
    :initarg :file
    :accessor sound-file)
   (data
    :initarg :data
    :accessor sound-data)
   (samplerate
    :initarg :samplerate
    :accessor sound-samplerate)
   (channels
    :initarg :channels
    :accessor sound-channels)
   (encoding
    :initarg :encoding
    :accessor sound-encoding)))

(defun load-mp3 (path)
  (let ((file (cl-mpg123:make-file path)))
    (cl-mpg123:connect file)
    (cl-mpg123:file-format file)
    (let ((data #()))
      (loop for bytes = (cl-mpg123:process-to-vector file)
            until (= (length bytes) 0)
            do (setf data (concatenate '(vector (unsigned-byte 8)) data bytes)))
      (let ((processed-sound
              (multiple-value-bind (samplerate channels encoding) (cl-mpg123:file-format file)
                (make-instance 'sound
                               :data data
                               :samplerate samplerate
                               :channels channels
                               :encoding encoding))))
        (cl-mpg123:disconnect file)
        processed-sound))))

(defmethod sound->pack ((sound sound))
  (with-slots (data samplerate channels encoding) sound
    (let ((pack (mixed:make-pack :encoding encoding
                                 :channels channels
                                 :samplerate samplerate)))
      #+nil(setf (mixed:size pack) (length data))
      #+nil(mixed:with-buffer-tx (buffer-data start size pack :direction :output)
        (loop for point across data
              for i from 0 below size
              do (setf (aref buffer-data (+ i start)) point))
        (mixed:finish size))
      pack)))

(defclass sound-source (mixed:source)
  ((sound
    :initarg :sound
    :accessor sound-source-sound)
   (head
    :initarg head
    :accessor sound-source-head
    :initform 0)))

(defmethod initialize-instance :after ((source sound-source) &key &allow-other-keys)
  (mixed:start source))

(defmethod mixed:start ((source sound-source))
  (with-slots (data samplerate channels encoding) (sound-source-sound source)
    (setf (mixed:samplerate (mixed:pack source)) samplerate)
    (setf (mixed:channels (mixed:pack source)) channels)
    (setf (mixed:encoding (mixed:pack source)) encoding)))

(defmethod mixed:mix ((source sound-source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (let ((read 0))
        (loop for i from start below (+ start size)
              for sound-data across (subseq (sound-data (sound-source-sound source))
                                            (sound-source-head source))
              do (setf (aref data i) sound-data)
                 (incf read))
        (cond
          ((< 0 read)
           (incf (mixed:byte-position source) read)
           (incf (sound-source-head source) read)
           (mixed:finish-write (mixed:data-ptr) size))
          ((= 0 read)
           (setf (mixed:done-p source) t)))))))

(defmethod mixed:frame-count ((source sound-source))
  (/ (length (sound-data (sound-source-sound source))) (mixed:framesize (mixed:pack source))))

(defmethod mixed:seek-to-frame ((source sound-source) position)
  (setf (sound-source-head source) (* position (mixed:framesize (mixed:pack source)))))

(defmethod harmony:make-source-for ((source sound) &rest initargs)
  (apply #'sound->source source initargs))

(defmethod sound->source ((sound sound) &rest initargs)
  (let ((source (apply #'make-instance 'sound-source :sound sound initargs)))
    source))
