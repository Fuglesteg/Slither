(uiop:define-package #:slither/audio
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
	#:slither/render
	#:slither/utils)
  (:local-nicknames (:harmony :org.shirakumo.fraf.harmony)
                    (:mixed :org.shirakumo.fraf.mixed)
		    (:vorbis :org.shirakumo.fraf.vorbis))
  (:import-from #:slither/assets
                #:defasset
                #:register-asset)
  (:export #:sound-play
	   #:music-play
	   #:audio-init
           #:defsound
           #:listener-position
	   #:sound-make-voice
	   #:sound-make-music-voice
	   #:voice-stop
	   #:voice-location
	   #:voice-volume))

(in-package #:slither/audio)

(defvar *initialized* nil)
(defvar *run-on-init* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defsound (name path &key (repeat nil)
				  (repeat-start 0.0))
    `(progn
       (defvar ,name (make-instance 'sound
				    :repeat ,repeat
				    :repeat-start ,repeat-start))
       (let ((function
               (lambda ()
                 (register-asset ',name ,path :sound)
                 (setf (sound-asset ,name)
                       ',name))))
         (if *initialized*
             (funcall function)
             (push function *run-on-init*))))))

(defclass sound ()
  ((asset
    :initarg :asset
    :initform nil
    :accessor sound-asset)
   (repeat
    :initarg :repeat
    :initform nil
    :type boolean
    :accessor sound-repeat)
   (repeat-start
    :initarg :repeat-start
    :initform 0.0
    :type single-float
    :accessor sound-repeat-start)))

(defun vorbis-file->source (file-path)
  (let* ((file (vorbis:open file-path))
	 (pcm-data-buffer (mixed:make-buffer (vorbis:sample-count file))))
    (vorbis:decode-frame file (list (mixed:data pcm-data-buffer)))
    (let ((pcm-vector-source (make-instance 'mixed:pcm-vector-source)))
      (setf (mixed:data-buffer pcm-vector-source)
	    pcm-data-buffer)
      pcm-vector-source)))

(defun audio-init ()
  (unless *initialized*
    (mixed:init)
    (vorbis:init)
    (sb-int:with-float-traps-masked (:divide-by-zero :overflow)
      (harmony:maybe-start-simple-server
       ;; :effects '((mixed:biquad-filter :filter :lowpass :name :lowpass))
       ;; (list (list 'mixed:compressor
       ;; 	    :threshold 48))
       :samplerate 48000
       :output-channels 2
       :latency 0.04
       :mixers '((:music mixed:basic-mixer :effects ((mixed:biquad-filter :filter :lowpass :name :music-lowpass)))
		 (:effect mixed:plane-mixer :min-distance 10 :max-distance 100))))
    (setf (mixed:volume :effect) 1.0)
    (setf (mixed:volume :music) 0.5)
    (setf (mixed:volume :master) 0.5)
    (loop for function in *run-on-init*
          do (funcall function))
    (setf *initialized* t)
    (setf *run-on-init* nil)))

(defvar *listener-position* (vec2 0))

(defun listener-position ()
  *listener-position*)

(defun (setf listener-position) (new-position)
  (setf *listener-position* new-position))

(defun sound-make-voice (sound &key (volume 1.0)
				 (on-end :disconnect)
				 (voice-class 'harmony:voice)
				 (mixer :effect))
  (let* ((voice (harmony:create (or (slither/assets::asset-data (sound-asset sound))
				    (error "No asset data for sound ~a" (sound-asset sound)))
				:volume volume
				; :channels 2
				:on-end on-end
				:class voice-class
				:mixer mixer))
	 (source (harmony:source voice)))
    (setf (harmony:repeat source)
	  (sound-repeat sound))
    (setf (harmony:repeat-start source)
	  (sound-repeat-start sound))
    voice))

(defun sound-make-music-voice (sound &key (volume 1.0)
			       (on-end :disconnect)
			       (voice-class 'harmony:music-segment))
  (sound-make-voice sound
		    :volume volume
		    :on-end on-end
		    :voice-class voice-class
		    :mixer :music))

(defmethod music-play ((sound sound) &key (volume 1.0))
	   (let ((voice (sound-make-voice sound
					  :voice-class 'harmony:music-segment)))
	     (harmony:play voice
			   :mixer :music
			   :volume volume)
	     voice))

(defmethod music-play ((sound harmony:voice) &key (volume 1.0))
	   (harmony:play sound
			 :mixer :music
			 :volume volume))

(defmethod sound-play ((sound sound) &key (position (vec2))
				       (velocity (vec2))
				       (volume 1.0))
  (let ((voice (sound-make-voice sound
				 :volume volume
				 :on-end :free)))
    (sound-play voice
		:volume volume
		:position position
		:velocity velocity)))

(defmethod sound-play ((sound harmony:voice) &key (position (vec2))
					       (velocity (vec2))
					       (volume 1.0))
  (harmony:play sound
                :reset t
		:volume volume
                :location (varr (position->location position))
                :velocity (varr velocity))
  sound)

(defun voice-stop (voice)
  (harmony:stop voice))

(defun position->location (position)
  "Takes a world position and converts it to audio coordinates"
  (let ((offset (v- position (listener-position))))
    (v* (rotation->vec2
	 (screen-space-rotate
	  (vec2->rotation offset)))
	(vlength offset))))

(-> voice-location (harmony:voice))
(defun voice-location (voice)
  (mixed:location voice))

(-> (setf voice-location) (vec2 harmony:voice))
(defun (setf voice-location) (new-value voice)
  (ignore-errors ; Don't understand why I'm getting an error from harmony here
   (setf (mixed:location voice)
	 (varr (position->location new-value)))))

(-> voice-volume (harmony:voice) single-float)
(defun voice-volume (voice)
  (mixed:volume voice))

(-> (setf voice-volume) (single-float harmony:voice))
(defun (setf voice-volume) (new-value voice)
  (setf (mixed:volume voice)
	new-value))

#|

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
|#
