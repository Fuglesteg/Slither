(use-modules (guix packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages text-editors))

(define slither (load "guix.scm"))

(packages->manifest (append (filter package?
                                    (map cadr
                                         (package-development-inputs slither)))
                            (list sbcl-micros
                                  sbcl)))