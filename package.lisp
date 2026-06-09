(defpackage :minecraft-3d
  (:use :common-lisp)
  (:export
   ;; Main
   #:run-game
   #:*running*
   
   ;; Game state
   #:game-player
   #:make-game-player
   #:game-player-x
   #:game-player-y
   #:game-player-z
   #:game-player-rot-x
   #:game-player-rot-y
   
   ;; World
   #:*minecraft-world*
   #:get-block
   #:set-block
   #:break-block
   #:place-block
   #:get-chunk
   
   ;; Input/Time
   #:handle-input
   #:get-delta-time
   #:update-mouse-look
   #:perform-raycast
   #:*move-speed*
   #:*mouse-sensitivity*
   
   ;; Rendering
   #:render-scene
   
   ;; OpenGL
   #:initialize-window
   #:shutdown-window))

(in-package :minecraft-3d)
