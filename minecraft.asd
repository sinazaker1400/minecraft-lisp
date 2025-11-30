  (defsystem "minecraft"
  :version "0.1.0"
  :author "Sina"
  :license "MIT" ; Or your chosen license
  :depends-on (:lispbuilder-sdl :cl-opengl :cl-glu) ; List dependencies
  :components ((:file "package")             ; Order matters if files depend on each other
               (:file "defs" :depends-on ("package"))
               (:file "world-generation" :depends-on ("defs"))
               (:file "rendering" :depends-on ("defs" "world-generation")) ; rendering might use geometry calc
               (:file "input" :depends-on ("defs"))
               (:file "main" :depends-on ("defs" "rendering" "input" "world-generation")))
  :description "A Minecraft-inspired 3D game in Lisp")
