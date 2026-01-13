(in-package #:minecraft-3d)

;; Main game state management

(defun initialize-game-state (seed)
  "Initialize the complete game state"
  (let ((world (initialize-world-with-seed seed))
        (player (make-player
                 :x 8.0 :y 80.0 :z 8.0  ; Start above ground
                 :rotation-x 0.0 :rotation-y 0.0
                 :velocity-x 0.0 :velocity-y 0.0 :velocity-z 0.0
                 :health 20
                 :hunger 20
                 :oxygen 20
                 :inventory (initialize-player-inventory)
                 :selected-slot 0
                 :active-item nil
                 :on-ground nil
                 :in-water nil
                 :in-lava nil
                 :sprinting nil
                 :sneaking nil
                 :flying nil
                 :creative-mode nil)))
    (make-game-state
     :world world
     :player player
     :time-of-day 0.5  ; Start at noon
     :weather :clear
     :difficulty :normal
     :game-mode :survival
     :active-entities '()
     :active-light-sources '()
     :seed seed)))

(defun update-game-state (game-state delta-time)
  "Update the entire game state"
  ;; Update day/night cycle
  (update-day-night-cycle game-state delta-time)
  
  ;; Update player physics
  (update-player-physics (game-state-player game-state) delta-time)
  
  ;; Update all entities
  (update-entities game-state delta-time)
  
  ;; Handle mob spawning
  (update-mob-spawning game-state)
  
  ;; Update chunk loading/unloading based on player position
  (update-chunk-loading game-state))

(defun update-chunk-loading (game-state)
  "Load/unload chunks based on player position"
  (let* ((player (game-state-player game-state))
         (player-chunk-x (floor (player-x player) +chunk-size+))
         (player-chunk-z (floor (player-z player) +chunk-size+))
         (render-distance 4))  ; Load chunks within 4 blocks of player
    
    ;; Determine which chunks should be loaded
    (let ((chunks-to-load '())
          (current-chunks (hash-table-keys (game-state-world game-state))))
      ;; Add chunks within render distance
      (loop for dx from (- render-distance) to render-distance
            do (loop for dz from (- render-distance) to render-distance
                     do (let ((chunk-x (+ player-chunk-x dx))
                              (chunk-z (+ player-chunk-z dz)))
                          (push (list chunk-x chunk-z) chunks-to-load))))
      
      ;; Load needed chunks
      (dolist (chunk-key chunks-to-load)
        (unless (gethash chunk-key (game-state-world game-state))
          (let ((chunk (generate-terrain-for-chunk (first chunk-key) (second chunk-key) 
                                                   (game-state-seed game-state))))
            (setf (gethash chunk-key (game-state-world game-state)) chunk))))
      
      ;; Unload chunks that are too far
      (dolist (chunk-key current-chunks)
        (let ((chunk-x (first chunk-key))
              (chunk-z (second chunk-key)))
          (when (> (max (abs (- chunk-x player-chunk-x))
                        (abs (- chunk-z player-chunk-z)))
                   (1+ render-distance))
            (remhash chunk-key (game-state-world game-state))))))))

(defun hash-table-keys (hash-table)
  "Get all keys from a hash table"
  (let ((keys '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             hash-table)
    keys))

(defun save-game-state (game-state filename)
  "Save the game state to a file"
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-circle* t))  ; Allow circular references in printing
      (write (list :world (game-state-world game-state)
                   :player (game-state-player game-state)
                   :time-of-day (game-state-time-of-day game-state)
                   :weather (game-state-weather game-state)
                   :difficulty (game-state-difficulty game-state)
                   :game-mode (game-state-game-mode game-state)
                   :active-entities (game-state-active-entities game-state)
                   :seed (game-state-seed game-state))
             :stream stream))))

(defun load-game-state (filename)
  "Load the game state from a file"
  (with-open-file (stream filename
                          :direction :input)
    (let* ((*read-eval* nil)  ; Disable evaluation during read
           (data (read stream))
           (world (getf data :world))
           (player (getf data :player))
           (time-of-day (getf data :time-of-day))
           (weather (getf data :weather))
           (difficulty (getf data :difficulty))
           (game-mode (getf data :game-mode))
           (active-entities (getf data :active-entities))
           (seed (getf data :seed)))
      (make-game-state
       :world world
       :player player
       :time-of-day time-of-day
       :weather weather
       :difficulty difficulty
       :game-mode game-mode
       :active-entities active-entities
       :active-light-sources '()
       :seed seed))))

;; Input handling for the game
(defun handle-player-input (game-state key pressed)
  "Handle player input"
  (let ((player (game-state-player game-state)))
    (case key
      (:w (if pressed
              (if (player-sneaking player)
                  (setf (player-velocity-z player) -0.5)
                  (setf (player-velocity-z player) -1.0))
              (setf (player-velocity-z player) 0.0)))
      (:s (if pressed
              (if (player-sneaking player)
                  (setf (player-velocity-z player) 0.5)
                  (setf (player-velocity-z player) 1.0))
              (setf (player-velocity-z player) 0.0)))
      (:a (if pressed
              (if (player-sneaking player)
                  (setf (player-velocity-x player) -0.5)
                  (setf (player-velocity-x player) -1.0))
              (setf (player-velocity-x player) 0.0)))
      (:d (if pressed
              (if (player-sneaking player)
                  (setf (player-velocity-x player) 0.5)
                  (setf (player-velocity-x player) 1.0))
              (setf (player-velocity-x player) 0.0)))
      (:space (when (and pressed (player-on-ground player))
                (player-jump player)))
      (:left-shift (setf (player-sneaking player) pressed))
      (:left-control (setf (player-sprinting player) pressed))
      ;; Number keys for hotbar selection
      (:1 (when pressed (switch-hotbar-slot player 0)))
      (:2 (when pressed (switch-hotbar-slot player 1)))
      (:3 (when pressed (switch-hotbar-slot player 2)))
      (:4 (when pressed (switch-hotbar-slot player 3)))
      (:5 (when pressed (switch-hotbar-slot player 4)))
      (:6 (when pressed (switch-hotbar-slot player 5)))
      (:7 (when pressed (switch-hotbar-slot player 6)))
      (:8 (when pressed (switch-hotbar-slot player 7)))
      (:9 (when pressed (switch-hotbar-slot player 8)))
      ;; Mouse buttons for block interaction
      (:mouse-left (when (and pressed (not (player-creative-mode player)))
                     (break-block (game-state-world game-state) player)))
      (:mouse-right (when pressed
                      (let ((selected-item (get-selected-item player)))
                        (when selected-item
                          (place-block (game-state-world game-state) player 
                                       (get-block-id-from-item selected-item)))))))))

(defun get-block-id-from-item (item-id)
  "Get the block ID corresponding to an item ID"
  (case item-id
    (:grass 1)
    (:dirt 2)
    (:stone 3)
    (:wood 4)
    (:leaves 5)
    (:sand 7)
    (:gravel 8)
    (:glass 14)
    (otherwise 0)))

;; Game mode functions
(defun set-game-mode (game-state mode)
  "Set the game mode (survival, creative, etc.)"
  (setf (game-state-game-mode game-state) mode)
  (case mode
    (:creative 
     (setf (player-creative-mode (game-state-player game-state)) t)
     (setf (player-health (game-state-player game-state)) 20)
     (setf (player-hunger (game-state-player game-state)) 20))
    (:survival 
     (setf (player-creative-mode (game-state-player game-state)) nil))))

(defun set-difficulty (game-state difficulty)
  "Set the game difficulty (peaceful, easy, normal, hard)"
  (setf (game-state-difficulty game-state) difficulty))