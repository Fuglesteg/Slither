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

(defun register-asset (symbol path)
  (add-asset symbol (uiop:read-file-string path)))

#+dev (defvar *file-paths* nil)
#+dev (defun find-file-path (symbol)
        (cdr (assoc symbol *file-paths*)))
#+dev (defun add-file-path (symbol path)
        (alist-push-or-replace symbol path *file-paths*))
#+dev (defun find-file-path-symbol (path)
        (car (rassoc path *file-paths*)))

(defmacro defasset (symbol path)
  `(progn (register-asset ,symbol ,path)
          #+dev (notify:watch ,path :events '(:modify))
          #+dev (add-file-path ,symbol ,path)))

#+dev (notify:with-events (file event)
        (declare (ignore event))
         (register-asset (find-file-path-symbol file) file))
