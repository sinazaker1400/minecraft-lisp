(defpackage :minecraft-3d
  (:use :common-lisp :cl-opengl :cffi)
  (:export
   ;; Main functions
   #:run-game
   
   ;; Game state
   #:*window*
   #:*running*
   
   ;; Player
   #:game-player
   #:make-game-player
   #:game-player-x
   #:game-player-y
   #:game-player-z
   #:game-player-rot-x
   #:game-player-rot-y
   
   ;; World
   #:get-block
   #:set-block
   #:break-block
   #:place-block
   #:get-chunk
   
   ;; Input
   #:handle-input
   #:update-mouse-look
   #:get-delta-time
   #:perform-raycast
   
   ;; Rendering
   #:render-scene
   
   ;; OpenGL setup
   #:initialize-window
   #:shutdown-window
   #:swap-buffers
   #:handle-events))
