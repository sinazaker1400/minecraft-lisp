(in-package :minecraft-3d)

;; ====== GAME STATE ======
(defparameter *running* t)
(defparameter *game-player* (make-game-player))

(defun handle-events ()
  "Process SDL events"
  (sdl:process-events
    (:quit-event () (setf *running* nil))
    (:key-down-event
     (:key key)
     (when (eq key :sdl-key-escape)
       (setf *running* nil)))))

(defun update-game (dt)
  "Update game state"
  (handle-input *
