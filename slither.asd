(asdf:defsystem #:slither
  :author "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :maintainer "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :license "GPL3"
  :version "0.0.2"
  :around-compile (lambda (next)
                    #+dev (proclaim '(optimize (debug 3) (safety 3) (speed 1)))
                    #-dev (proclaim '(optimize (debug 0) (safety 0) (speed 3)))
                    (funcall next))
  :depends-on (#:cl-opengl
               #:3d-math
               #:sb-bsd-sockets
               #:ieee-floats
               #:glfw
               #:pngload
               #:harmony
               #:cl-mixed-vorbis
               #:cl-mixed-sdl2
               #:static-vectors
               (:feature :dev #:file-notify)
               #:alexandria
               #:serapeum)
  :serial t
  :components ((:file "utils")
               (:module "core"
                :serial t
                :components
                ((:file "package")
                 (:file "serialization")
                 (:file "entities")
                 (:file "behaviors")
                 (:file "systems")
                 (:file "game-loop")))
               (:file "assets")
               (:file "audio")
               (:file "window")
               (:file "input")
               (:module "render"
                :serial t
                :components
                ((:file "uniform")
                 (:file "texture")
                 (:file "array-texture")
                 (:file "shader")
                 (:file "shader-program")
                 (:file "vertex")
                 (:file "render")))
               (:file "ui")
               (:file "scenes")
               (:module "networking"
                :serial t
                :components
                ((:file "socket")
                 (:file "protocol")
                 (:file "connection")
                 (:file "client-prediction")
                 (:file "networked")
                 (:file "client")
                 (:file "actions")
                 (:file "server")
                 (:file "networking")))
               (:file "behaviors")
               (:file "physics")
               (:file "slither")))
