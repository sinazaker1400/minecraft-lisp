(defpackage :minecraft-3d
  (:use :cl 
        :lispbuilder-sdl
        :lispbuilder-sdl-gfx
        :cl-opengl
        :cl-glu)
  (:export #:main
           #:run-game
           #:game-player
           #:game-player-x
           #:game-player-y
           #:game-player-z
           #:game-player-rot-x
           #:game-player-rot-y
           #:game-state
           #:game-state-player
           #:game-state-world
           #:chunk
           #:chunk-blocks
           #:chunk-x
           #:chunk-z
           #:world-coords-to-chunk-coords
           #:world->local
           #:get-block
           #:set-block
           #:break-block
           #:place-block
           #:handle-input
           #:handle-mouse-look
           #:update-mouse-look
           #:raycast-to-block
           #:generate-chunk
           #:calculate-height
           #:get-chunk
           #:render-game
           #:can-occupy-space-p))

(in-package :minecraft-3d)

;; Global game world storage
(defparameter *minecraft-world* (make-hash-table :test 'equal))

;; Game instance (will be set when game starts)
(defparameter *game-state* nil)

;; Seed for world generation (can be changed before starting game)
(defparameter *world-seed* 12345)
