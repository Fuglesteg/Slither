(uiop:define-package :slither/assets
  (:use #:cl)
  #+dev(:local-nicknames (:notify :org.shirakumo.file-notify))
  (:export #:asset-data
           #:asset-reload
           #:defasset))

(in-package :slither/assets)

(defvar *assets* nil)

(defun find-asset (symbol)
  (assoc symbol *assets*))

(defun asset-data (symbol)
  (cdr (find-asset symbol)))

(defmacro alist-push-or-replace (key value alist)
  `(let ((pair (assoc ,key ,alist)))
     (if pair
         (rplacd pair ,value)
         (push (cons ,key ,value) ,alist))))

(defun add-asset (symbol data)
  (alist-push-or-replace symbol data *assets*))

(deftype asset-type ()
  '(member :text :bytes :png))

(defun read-file-bytes (pathname)
  (with-open-file (stream pathname
                          :element-type '(unsigned-byte 8))
    (loop for byte = (read-byte stream nil)
          while byte
          collect byte)))
                          

(declaim (ftype (function (symbol (or pathname string) &optional asset-type) t) register-asset))
(defun register-asset (symbol path &optional (asset-type :text))
  (add-asset symbol (case asset-type
                      (:text (uiop:read-file-string path))
                      (:bytes (read-file-bytes path))
                      (:png (pngload:load-file path
                                               :flatten t
                                               :flip-y t)))))

#+dev (defvar *file-paths* nil)
#+dev (defun find-file-path (symbol)
        (cdr (assoc symbol *file-paths*)))
#+dev (defun add-file-path (symbol path)
        (alist-push-or-replace symbol path *file-paths*))
#+dev (defun find-file-path-symbol (path)
        (car (rassoc path *file-paths*)))

(defmacro defasset (name path &optional (asset-type :text))
  `(progn (register-asset ',name ,path ,asset-type)
          #+dev (notify:watch ,path :events '(:modify))
          #+dev (add-file-path ',name ,path)))

#+dev (notify:with-events (file event)
        (declare (ignore event))
         (register-asset (find-file-path-symbol file) file))
