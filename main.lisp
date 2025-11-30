(in-package #:minecraft-3d)

;; Update start-game to accept and use the seed
(defun start-game (&optional (seed 12345)) ; Add an optional seed parameter
  "Initialize and start the 3D game with a given seed"
  ;; Initialize the global random state with the seed using our workaround
  (initialize-randomness seed)

  (sdl:with-init ()
    (sdl:window *window-width* *window-height*
                :flags '(sdl-cffi::sdl-opengl)
                :title-caption "Minecraft 3D Lisp"
                :icon-caption "Minecraft 3D Lisp")

    ;; Initialize OpenGL settings
    (init-opengl *window-width* *window-height*)
    (setup-opengl *window-width* *window-height*)

    (let ((player (make-game-player)))
      ;; Initial render
      (render-world player)

      ;; Main game loop
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event ()
         (setup-opengl *window-width* *window-height*)
         (render-world player)
         (sdl:update-display))
        (:key-down-event (:key key)
                         (case key
                           (:sdl-key-escape (sdl:push-quit-event))))
        (:idle ()
               (handle-input player)
               (render-world player)
               (sdl:update-display))))))