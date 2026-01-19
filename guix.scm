(use-modules (guix packages)
             (guix build-system asdf)
             (gnu packages lisp-xyz)
             (fuglesteg packages lisp)
             (guix gexp)
             ((guix licenses) #:prefix licenses:))

(define slither
  (package
   (name "slither")
   (version "0.0.3")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (inputs (list
            sbcl-cl-opengl
            sbcl-3d-math
            sbcl-harmony
            sbcl-cl-mixed
            sbcl-glfw
            sbcl-static-vectors
            sbcl-file-notify
            sbcl-pngload
            sbcl-ieee-floats
            sbcl-alexandria
            sbcl-serapeum))
   (synopsis "") (description "") (home-page "") (license licenses:gpl3+)))

slither
