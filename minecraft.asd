(asdf:defsystem :minecraft
  :name "minecraft-lisp"
  :version "0.1.0"
  :description "A Minecraft-like 3D voxel engine in Common Lisp"
  :author "Sina Zaker"
  :license "MIT"
  :depends-on (:cl-opengl :lispbuilder-sdl :alexandria :cffi)
  :serial t
  :components
  ((:file "package")
   (:file "defs")
   (:file "world-generation")
   (:file "input")
   (:file "world-interaction")
   (:file "opengl-setup")
   (:file "rendering")
   (:file "main")))
