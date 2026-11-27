(use-modules (guix packages)
             (guix git-download)
             (guix transformations)
             (guix build-system asdf)
             (gnu packages lisp-xyz)
             (gnu packages stb)
             (gnu packages xiph)
             (gnu packages audio)
             (gnu packages commencement)
             (fuglesteg packages lisp)
             (guix gexp)
             ((guix licenses) #:prefix license:))

(define-public sbcl-cl-vorbis-patched
  (let ((commit "9ada11295eba8b556c596bc2f33836177bd71520")
        (revision "0"))
    (package
      (name "sbcl-cl-vorbis")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://shirakumo.org/project/cl-vorbis")
               (commit commit)))
         (file-name (git-file-name "cl-vorbis" version))
         (sha256
          (base32 "0xs3pjxvpp7ijvnnwy4m2jm3363d8nvpa6a2648r5rqxs5c7yd1r"))
         (modules '((guix build utils)))
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
      (license license:zlib))))

(define-public libmixed-latest
  (let ((commit "bd124ac136ad9bc17c165298ccc94d6107b6d89f"))
    (package
     (inherit libmixed)
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://shirakumo.org/project/libmixed")
             (commit commit)))
       (file-name (git-file-name "libmixed" "2.4.0"))
       (sha256
        (base32 "0gmwjw12qxdjd2s1wgzsgqn6dykig5j5vpsfva7wia91kjsdl982")))))))

(define-public sbcl-cl-mixed-latest
  ;; There are no releases, but this commit is entitled
  ;; "Bump to libmixed-2.4.0".
  (let ((commit "1b3c5de52bd55eafe7ab996a44950dfd8ca92cb7")
        (revision "3"))
    (package
     (inherit sbcl-cl-mixed)
     (version (git-version "2.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://shirakumo.org/project/cl-mixed")
             (commit commit)))
       (file-name (git-file-name "cl-mixed" version))
       (sha256
        (base32 "1jihxinpmcxvc0fqki3n8bk1v8hipp7m6d959zbs9ajpgcnhkclk"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled libraries.
        `(begin
           (delete-file-recursively "static")))))
     (arguments
      '(#:asd-systems '("cl-mixed"
                        "cl-mixed-examples"
                        "cl-mixed-flac"
                        "cl-mixed-vorbis"
                        "cl-mixed-alsa"
                        "cl-mixed-jack"
                        "cl-mixed-mpg123"
                        "cl-mixed-mpt"
                        "cl-mixed-out123"
                        "cl-mixed-pulse"
                        "cl-mixed-pipewire"
                        "cl-mixed-sdl2"
                        "cl-mixed-wav")
        #:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
          (lambda* (#:key inputs #:allow-other-keys)
           (substitute* "low-level.lisp"
                        (("libmixed.so.2")
                         (search-input-file inputs "/lib/libmixed.so.2"))))))))
     (inputs
      (list libmixed-latest
            sbcl-alexandria
            sbcl-cffi
            sbcl-cl-flac
            sbcl-cl-mpg123
            sbcl-cl-out123
            sbcl-cl-vorbis-patched
            sbcl-documentation-utils
            sbcl-sdl2
            sbcl-static-vectors
            sbcl-trivial-features)))))

(define-public sbcl-text-draw
  (let ((commit "cdf62f1604908a082f0d4ca45fba00ceacabee1e")
        (revision "0"))
    (package
     (name "sbcl-text-draw")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://shinmera.com/project/text-draw")
             (commit commit)))
       (file-name (git-file-name "text-draw" version))
       (sha256
        (base32 "1zcxvq5hcp6h5p3c9vw2fqxpz3zfifwl3j7k22vl28bcp8x98x9d"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-documentation-utils
            sbcl-trivial-gray-streams))
     (home-page "")
     (synopsis "")
     (description "")
     (license license:zlib))))

(define-public sbcl-harmony-latest
  (let ((commit "9a339ba97f1fa84184c25745e40c107299420852")
        (revision "0"))
    (package
     (inherit sbcl-harmony)
     (version (git-version "2.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://shirakumo.org/project/harmony")
             (commit commit)))
       (file-name (git-file-name "cl-harmony" version))
       (sha256
        (base32 "0k94379y913kj3js061nbmrz02a9byixxkq0h7gqchs9id1wpcsn"))))
     (inputs
      (list sbcl-atomics
            sbcl-text-draw
            sbcl-bordeaux-threads
            sbcl-cl-mixed-latest
            sbcl-stealth-mixin
            sbcl-trivial-features)))))

(define slither
  (package
   (name "slither")
   (version "0.0.3")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (inputs
    (list sbcl-cl-opengl
          sbcl-3d-math
          sbcl-harmony-latest
          sbcl-glfw
          sbcl-static-vectors
          sbcl-file-notify
          sbcl-pngload
          sbcl-ieee-floats
          sbcl-alexandria
          sbcl-serapeum))
   (synopsis "") (description "") (home-page "") (license license:gpl3+)))

slither
