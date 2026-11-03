#!/usr/bin/env -S guix shell --rebuild-cache -m manifest.scm -- bash -c 'LD_LIBRARY_PATH=${LIBRARY_PATH} sbcl --script $0 $1 $2 $3 $4'

(require 'asdf)

(pushnew :dev *features*)

(asdf:load-system :micros)

(destructuring-bind (&optional (port "4005") . features) (uiop:command-line-arguments)
  (when features
    (loop for feature in features
          do (pushnew (intern (string-upcase feature) :keyword)
                      *features*)))
  (let ((port (parse-integer port)))
    (micros:create-server :dont-close t :port port)))

(asdf:load-asd (merge-pathnames #P"slither.asd" (uiop:getcwd)))
(asdf:load-system :slither :force t)

(read)
(exit)
