(use-modules (guix packages)
             (gnu packages lisp)
             (fuglesteg packages lem))

(concatenate-manifests
  (list
    (package->development-manifest (load "./guix.scm"))
    (packages->manifest (list lem sbcl-micros sbcl))))
