;; Define the package for the 3D Minecraft game
(defpackage #:minecraft-3d
  ;; Use the standard Common Lisp and LispBuilder-SDL packages
  (:use #:cl #:lispbuilder-sdl)
  ;; Import the OpenGL and GLU packages (using prefixes like gl: and glu:)
  ;; We don't import :color to avoid conflicts between lispbuilder-sdl:color and cl-opengl:color
  ;; We will use gl:color, gl:vertex, etc. explicitly in rendering code.
  ;; (:import-from #:cl-opengl #:gl:color #:gl:vertex #:gl:with-primitives ...) ; Example, not needed here
  ;; Export the main entry point function
  (:export #:start-game))

;; Switch to the newly defined package for subsequent definitions
(in-package #:minecraft-3d)