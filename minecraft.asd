(defsystem "minecraft"
  :version "0.1.0"
  :author "Sina"
  :license "MIT"
  :depends-on (:lispbuilder-sdl :cl-opengl :cl-glu)
  :components ((:file "minecraft"))
  :description "A Minecraft-like game in Lisp")
