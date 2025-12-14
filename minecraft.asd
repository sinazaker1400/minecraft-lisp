(defsystem "minecraft"
  :version "0.1.0"
  :author "Sina"
  :license "MIT"
  :depends-on (:lispbuilder-sdl :cl-opengl :cl-glu)
  :components ((:file "package")
               (:file "defs" :depends-on ("package"))
               (:file "world-generation" :depends-on ("defs"))
               (:file "rendering" :depends-on ("defs" "world-generation"))
               (:file "input" :depends-on ("defs"))
               (:file "opengl-setup" :depends-on ("defs"))
               (:file "world-interaction" :depends-on ("defs" "world-generation" "rendering"))
               (:file "main" :depends-on ("defs" "rendering" "input" "world-generation" "world-interaction" "opengl-setup")))
  :description "A Minecraft-inspired 3D game in Lisp")
