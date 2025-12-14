(in-package #:minecraft-3d)

;; Added OpenGL initialization and setup functions
(defun init-opengl (width height)
  "Initialize OpenGL context and settings"
  (gl:enable :depth-test)
  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :color-material)
  (gl:color-material :front-and-back :ambient-and-diffuse)
  
  ;; Setup lighting
  (gl:light :light0 :position #(1.0 1.0 1.0 0.0))
  (gl:light :light0 :ambient  #(0.2 0.2 0.2 1.0))
  (gl:light :light0 :diffuse  #(0.8 0.8 0.8 1.0))
  (gl:light :light0 :specular #(1.0 1.0 1.0 1.0))
  
  (gl:material :front-and-back :specular #(1.0 1.0 1.0 1.0))
  (gl:material :front-and-back :shininess 100.0))

(defun setup-opengl (width height)
  "Setup OpenGL projection and viewport"
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60.0 (/ width height) 0.1 100.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun get-block-color (block-type)
  "Returns RGB color values for a given block type"
  (case block-type
    (grass  (values 0.2 0.8 0.2))
    (dirt   (values 0.6 0.5 0.3))
    (stone  (values 0.5 0.5 0.5))
    (wood   (values 0.4 0.2 0.1))
    (otherwise (values 1.0 1.0 1.0))))
