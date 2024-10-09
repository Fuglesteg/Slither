(use-modules (guix packages)
             (guix build-system asdf)
             (gnu packages lisp-xyz)
             (fuglesteg packages lisp)
             (guix gexp)
             ((guix licenses) #:prefix licenses:))


(define slither
  (package 
   (name "slither")
   (version "a0.0.1")
   (source (local-file "./" #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (inputs (list
            sbcl-3d-math
            sbcl-glfw
            sbcl-static-vectors))
   (synopsis "") (description "") (home-page "") (license licenses:gpl3+)))

slither
