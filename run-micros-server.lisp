#!/usr/bin/env -S guix shell -m manifest.scm -- bash -c 'LD_LIBRARY_PATH=${LIBRARY_PATH} sbcl --script $0'

(require 'asdf)

(pushnew :dev *features*)

(asdf:load-system :micros)
(micros:create-server :dont-close t)

(asdf:load-asd (merge-pathnames #P"slither.asd" (uiop:getcwd)))
(asdf:load-system :slither :force t)

(read)
(quit)