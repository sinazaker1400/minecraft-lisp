(in-package #:minecraft-3d)

;; Physics system with gravity and collision detection

(defun update-player-physics (player delta-time)
  "Update player physics including gravity, movement, and collision"
  ;; Apply gravity
  (unless (player-on-ground player)
    (decf (player-velocity-y player) (* +gravity-strength+ delta-time)))
  
  ;; Update position based on velocity
  (incf (player-x player) (* (player-velocity-x player) delta-time))
  (incf (player-y player) (* (player-velocity-y player) delta-time))
  (incf (player-z player) (* (player-velocity-z player) delta-time))
  
  ;; Handle collisions
  (handle-player-collisions player)
  
  ;; Apply friction when on ground
  (if (player-on-ground player)
      (progn
        (setf (player-velocity-x player) (* (player-velocity-x player) 0.8))
        (setf (player-velocity-z player) (* (player-velocity-z player) 0.8))
        ;; Stop very small velocities
        (when (< (abs (player-velocity-x player)) 0.01)
          (setf (player-velocity-x player) 0.0))
        (when (< (abs (player-velocity-z player)) 0.01)
          (setf (player-velocity-z player) 0.0)))
      ;; Air resistance
      (progn
        (setf (player-velocity-x player) (* (player-velocity-x player) 0.99))
        (setf (player-velocity-z player) (* (player-velocity-z player) 0.99))))
  
  ;; Check if player is in water or lava
  (check-fluid-collision player))

(defun handle-player-collisions (player)
  "Handle collisions between player and blocks"
  (let* ((player-width 0.6)
         (player-height 1.8)
         (player-depth 0.6)
         (x (player-x player))
         (y (player-y player))
         (z (player-z player))
         (world (get-current-world)))
    
    ;; Check collision in X-axis
    (let ((new-x x))
      (loop for dx in (list (- (/ player-width 2)) (/ player-width 2))
            do (loop for dy in (list 0 player-height)
                     do (let ((block-x (floor (+ x dx)))
                              (block-y (floor (+ y dy)))
                              (block-z (floor z)))
                          (when (and (block-exists-at world block-x block-y block-z)
                                     (is-solid-block world block-x block-y block-z))
                            (if (> dx 0)
                                (setf new-x (- (1+ block-x) (/ player-width 2)))
                                (setf new-x (+ block-x (/ player-width 2))))))))
      (setf (player-x player) new-x))
    
    ;; Check collision in Y-axis (for ground detection)
    (let ((new-y y)
          (on-ground nil))
      (loop for dx in (list (- (/ player-width 2)) 0 (/ player-width 2))
            do (loop for dz in (list (- (/ player-depth 2)) 0 (/ player-depth 2))
                     do (let* ((block-x (floor (+ x dx)))
                               (block-y (floor (1- (+ y player-height))))
                               (block-z (floor (+ z dz)))
                               (block-id (get-block-at world block-x block-y block-z)))
                          (when (and block-id 
                                     (> block-id 0)
                                     (is-solid-block world block-x block-y block-z))
                            (setf new-y (- block-y player-height))
                            (setf on-ground t)
                            (setf (player-velocity-y player) 0.0)))))
      (setf (player-y player) new-y)
      (setf (player-on-ground player) on-ground))
    
    ;; Check collision in Z-axis
    (let ((new-z z))
      (loop for dx in (list (- (/ player-width 2)) (/ player-width 2))
            do (loop for dy in (list 0 player-height)
                     do (let ((block-x (floor (+ x dx)))
                              (block-y (floor (+ y dy)))
                              (block-z (floor (+ z (/ player-depth 2)))))
                          (when (and (block-exists-at world block-x block-y block-z)
                                     (is-solid-block world block-x block-y block-z))
                            (if (> dz 0)
                                (setf new-z (- (1+ block-z) (/ player-depth 2)))
                                (setf new-z (+ block-z (/ player-depth 2))))))))
      (setf (player-z player) new-z))))

(defun block-exists-at (world x y z)
  "Check if a block exists at the given coordinates"
  (let ((block-id (get-block-at world x y z)))
    (and block-id (> block-id 0))))

(defun is-solid-block (world x y z)
  "Check if the block at the given coordinates is solid"
  (let ((block-id (get-block-at world x y z)))
    (when block-id
      (let ((block-type (gethash block-id *block-types-hash*)))
        (and block-type (block-type-solid block-type))))))

(defun check-fluid-collision (player)
  "Check if player is in water or lava"
  (let* ((world (get-current-world))
         (x (player-x player))
         (y (player-y player))
         (z (player-z player)))
    ;; Check if feet are in fluid
    (let ((block-id (get-block-at world (floor x) (floor y) (floor z))))
      (when block-id
        (let ((block-type (gethash block-id *block-types-hash*)))
          (when block-type
            (cond
              ((string= (block-type-name block-type) "Water")
               (setf (player-in-water player) t)
               (setf (player-in-lava player) nil))
              ((string= (block-type-name block-type) "Lava")
               (setf (player-in-water player) nil)
               (setf (player-in-lava player) t)
               ;; Apply damage if in lava
               (decf (player-health player) (* 0.1 (player-velocity-y player))))
              (t
               (setf (player-in-water player) nil)
               (setf (player-in-lava player) nil))))))))

(defun player-jump (player)
  "Make the player jump if on ground"
  (when (player-on-ground player)
    (incf (player-velocity-y player) +jump-strength+)
    (setf (player-on-ground player) nil)))

(defun update-entity-physics (entity delta-time)
  "Update physics for an entity"
  ;; Apply gravity to entities
  (decf (entity-velocity-y entity) (* +gravity-strength+ delta-time))
  
  ;; Update position
  (incf (entity-x entity) (* (entity-velocity-x entity) delta-time))
  (incf (entity-y entity) (* (entity-velocity-y entity) delta-time))
  (incf (entity-z entity) (* (entity-velocity-z entity) delta-time))
  
  ;; Apply friction
  (setf (entity-velocity-x entity) (* (entity-velocity-x entity) 0.95))
  (setf (entity-velocity-z entity) (* (entity-velocity-z entity) 0.95))
  
  ;; Check if entity is on ground
  (let ((world (get-current-world)))
    (when (block-exists-at world 
                           (floor (entity-x entity)) 
                           (1- (floor (+ (entity-y entity) (entity-height entity)))) 
                           (floor (entity-z entity)))
      (setf (entity-velocity-y entity) 0.0))))

(defun entity-block-collision (entity)
  "Check and handle collisions between entity and blocks"
  (let* ((x (entity-x entity))
         (y (entity-y entity))
         (z (entity-z entity))
         (width (entity-width entity))
         (height (entity-height entity))
         (world (get-current-world)))
    
    ;; Simple collision detection - check if feet are on a block
    (let ((block-id (get-block-at world (floor x) (floor (1- (+ y height))) (floor z))))
      (when (and block-id (> block-id 0))
        (setf (entity-y entity) (- (floor (1- (+ y height))) height))
        (setf (entity-velocity-y entity) 0.0)))))