(asdf:defsystem :minecraft
  :name "minecraft"
  :version "0.1.0"
  :author "Sina Zaker"
  :license "MIT"
  :description "A Minecraft-like 3D voxel engine in Common Lisp"
  :depends-on (:sdl2
               :cl-opengl
               :cl-glu)
  :serial t
  :components ((:file "package")
               (:file "defs")
               (:file "world-generation")
               (:file "input")
               (:file "world-interaction")
               (:file "opengl-setup")
               (:file "rendering")
               (:file "main")))
