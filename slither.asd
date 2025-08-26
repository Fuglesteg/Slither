(asdf:defsystem #:slither
  :author "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :maintainer "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :license "GPL3"
  :version "0.0.2"
  :depends-on (#:cl-opengl
               #:3d-math
               #:glfw
               #:pngload
               #:harmony
               #:cl-mixed-mpg123
               (:feature :linux #:cl-mixed-pulse)
               (:feature :windows #:cl-mixed-wasapi)
               #:static-vectors
               (:feature :dev #:file-notify)
               #:alexandria
               #:serapeum)
  :serial t
  :components ((:file "utils")
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
               (:file "entities")
               (:file "behaviors")
               (:file "scenes")
               (:file "slither")))