(use-modules (guix packages)
             (gnu packages lisp))

(load "/home/andy/code/lem-guix-packaging/package.scm")

(concatenate-manifests
  (list
    (package->development-manifest (load "./guix.scm"))
    (packages->manifest (list lem sbcl-micros sbcl))))
