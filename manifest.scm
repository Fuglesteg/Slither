(use-modules (guix packages)
             (gnu packages lisp)
             (gnu packages text-editors))

(concatenate-manifests
  (list
    (package->development-manifest (load "./guix.scm"))
    (packages->manifest (list lem sbcl-micros sbcl))))
