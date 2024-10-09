(asdf:defsystem #:slither
  :author "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :maintainer "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :license "GPL3"
  :version "0.0.1"
  :depends-on (#:cl-opengl
               #:3d-math
               #:org.shirakumo.fraf.glfw
               #:static-vectors)
  :components ((:file "slither")))