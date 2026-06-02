(in-package #:minecraft-3d)

(defun init-opengl (width height)

  (declare (ignore width height))

  (gl:enable :depth-test)
  (gl:enable :cull-face)

  (gl:cull-face :back)

  (gl:shade-model :smooth)

  (gl:enable :color-material)

  (gl:clear-color
   0.5
   0.7
   1.0
   1.0))

(defun setup-opengl (width height)

  (gl:viewport 0 0 width height)

  (gl:matrix-mode :projection)
  (gl:load-identity)

  (glu:perspective
   70.0
   (/ (float width)
      (float height))
   0.1
   500.0)

  (gl:matrix-mode :modelview)
  (gl:load-identity))