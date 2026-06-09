(asdf:defsystem #:minecraft
  :description "A Minecraft-like 3D game in Common Lisp"
  :author "Sina Zaker"
  :license "MIT"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx
               #:cl-opengl
               #:cl-glu
               #:cl-glop)
  :serial t
  :components ((:file "package")
               (:file "defs")
               (:file "opengl-setup")
               (:file "world-generation")
               (:file "input")
               (:file "world-interaction")
               (:file "rendering")
               (:file "main")))
