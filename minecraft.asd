(defsystem "minecraft"
  :version "0.1.0"
  :author "Sina"
  :license "MIT" ; Or your chosen license
  :depends-on (:lispbuilder-sdl :cl-opengl :cl-glu) ; List dependencies
  :components ((:file "package")             ; Order matters if files depend on each other
               (:file "defs" :depends-on ("package"))
               (:file "world-generation" :depends-on ("defs"))
               (:file "rendering" :depends-on ("defs" "world-generation"))
               (:file "input" :depends-on ("defs"))
               (:file "world-interaction" :depends-on ("defs" "world-generation" "rendering")) ; Add this line
               (:file "main" :depends-on ("defs" "rendering" "input" "world-generation" "world-interaction"))) ; Add dependency to main if it calls functions from world-interaction
  :description "A Minecraft-inspired 3D game in Lisp")