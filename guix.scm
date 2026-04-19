(use-modules (guix packages)
             (guix git-download)
             (guix transformations)
             (guix build-system asdf)
             (gnu packages lisp-xyz)
             (gnu packages stb)
             (gnu packages xiph)
             (gnu packages commencement)
             (fuglesteg packages lisp)
             (guix gexp)
             ((guix licenses) #:prefix licenses:))

(define-public sbcl-cl-vorbis-patched
  (let ((commit "c5835cd7091aea9e2e389ad359d244542d637758")
        (revision "0"))
    (package
      (name "sbcl-cl-vorbis")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/cl-vorbis")
               (commit commit)))
         (file-name (git-file-name "cl-vorbis" version))
         (sha256
          (base32 "0713pl5c2khfpf8m3h1l2y0ilack7akf580h70jq6qcrnq3h4b40"))
         (snippet
          `(delete-file-recursively "static"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
           (add-after 'unpack 'build-stb-vorbis
             (lambda _
               (invoke #$(file-append gcc-toolchain "/bin/gcc")
                       "-shared"
                       "-o" "stb_vorbis.so"
                       ; CFLAGS
                       "-O3" "-ftree-vectorize" "-fPIC" "-g"
                       "stb_vorbis_patch.c"
                       ; LDFLAGS
                       "-l" "m")
               (install-file "stb_vorbis.so"
                             (string-append #$output "/lib"))))
           (add-after 'build-stb-vorbis 'fix-paths
             (lambda _
               (substitute* "low-level.lisp"
                 (("libvorbis-lin-amd64.so")
                  (string-append #$output "/lib/stb_vorbis.so"))))))))
      (inputs
       (list sbcl-cffi
             sbcl-documentation-utils
             sbcl-static-vectors
             sbcl-trivial-features
             sbcl-trivial-garbage))
      (home-page "https://shirakumo.github.io/cl-vorbis/")
      (synopsis "OGG/Vorbis decoding using stb_vorbis for Common Lisp")
      (description "This package provides CFFI bindings for the
@code{stb_vorbis} audio library to Common Lisp.")
      (license licenses:zlib))))

(define sbcl-cl-mixed-patched
  ((package-input-rewriting `((,sbcl-cl-vorbis . ,sbcl-cl-vorbis-patched)))
   sbcl-cl-mixed))

(define sbcl-harmony-patched
  ((package-input-rewriting `((,sbcl-cl-mixed . ,sbcl-cl-mixed-patched)))
   sbcl-harmony))

(define slither
  (package
   (name "slither")
   (version "0.0.3")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (inputs
    (list sbcl-cl-opengl
          sbcl-3d-math
          sbcl-harmony-patched
          sbcl-glfw
          sbcl-static-vectors
          sbcl-file-notify
          sbcl-pngload
          sbcl-ieee-floats
          sbcl-alexandria
          sbcl-serapeum))
   (synopsis "") (description "") (home-page "") (license licenses:gpl3+)))

slither
