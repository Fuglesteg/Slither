(use-modules (guix packages)
             (guix build-system asdf)
             (gnu packages lisp-xyz)
             (fuglesteg packages lisp)
             (guix gexp)
             ((guix licenses) #:prefix licenses:))

(define slither-dev
  (package
   (inherit (load "guix.scm"))
   (arguments
    (list #:phases
          (modify-phases %standard-phases
                         (add-after 'unpack 'add-dev-feature
                                    #~(substitute "slither.asd"
                                                  (("(asdf:defsystem #:slither")
"(pushnew :dev *features*)\n\n(asdf:defsystem #:slither"))))))))

slither-dev
