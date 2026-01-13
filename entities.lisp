(in-package #:minecraft-3d)

;; Entity and mob system

(defun create-entity (x y z entity-type)
  "Create a new entity at the given position"
  (let ((entity (make-entity
                 :id (gensym)
                 :x x :y y :z z
                 :health 20
                 :max-health 20
                 :velocity-x 0 :velocity-y 0 :velocity-z 0
                 :width 0.6 :height 1.8 :depth 0.6
                 :entity-type entity-type
                 :ai-state :idle
                 :ai-target nil
                 :equipped-items '()
                 :active-effect nil)))
    ;; Set specific properties based on entity type
    (case entity-type
      (:zombie (setf (entity-health entity) 20
                     (entity-max-health entity) 20
                     (entity-width entity) 0.6
                     (entity-height entity) 1.8))
      (:cow (setf (entity-health entity) 10
                  (entity-max-health entity) 10
                  (entity-width entity) 0.9
                  (entity-height entity) 1.4))
      (:pig (setf (entity-health entity) 10
                  (entity-max-health entity) 10
                  (entity-width entity) 0.9
                  (entity-height entity) 0.9))
      (:chicken (setf (entity-health entity) 4
                      (entity-max-health entity) 4
                      (entity-width entity) 0.4
                      (entity-height entity) 0.7)))
    entity))

(defun spawn-entity (game-state x y z entity-type)
  "Spawn an entity in the game world"
  (let ((entity (create-entity x y z entity-type)))
    (push entity (game-state-active-entities game-state))
    entity))

(defun update-entities (game-state delta-time)
  "Update all entities in the game world"
  (dolist (entity (game-state-active-entities game-state))
    ;; Update entity physics
    (update-entity-physics entity delta-time)
    
    ;; Update entity AI based on type
    (case (entity-entity-type entity)
      (:zombie (update-zombie-ai entity game-state delta-time))
      (:cow (update-cow-ai entity game-state delta-time))
      (:pig (update-pig-ai entity game-state delta-time))
      (:chicken (update-chicken-ai entity game-state delta-time)))))

(defun update-zombie-ai (zombie game-state delta-time)
  "Update zombie AI behavior"
  (let ((player (game-state-player game-state))
        (distance (calculate-distance (entity-x zombie) (entity-y zombie) (entity-z zombie)
                                      (player-x player) (player-y player) (player-z player))))
    (cond
      ;; If player is close, chase them
      ((< distance 16)
       (setf (entity-ai-state zombie) :chasing)
       (let ((dx (- (player-x player) (entity-x zombie)))
             (dz (- (player-z player) (entity-z zombie)))
             (dist (sqrt (+ (expt (- (player-x player) (entity-x zombie)) 2)
                            (expt (- (player-z player) (entity-z zombie)) 2)))))
         (when (> dist 0)
           (setf (entity-velocity-x zombie) (* (/ dx dist) 0.5))
           (entity-velocity-z zombie) (* (/ dz dist) 0.5))))
      ;; If player is far, wander randomly
      (t
       (setf (entity-ai-state zombie) :idle)
       ;; Occasionally change direction when wandering
       (when (< (random 1.0) 0.01)
         (setf (entity-velocity-x zombie) (- (random 1.0) 0.5))
         (entity-velocity-z zombie) (- (random 1.0) 0.5))))))

(defun update-cow-ai (cow game-state delta-time)
  "Update cow AI behavior"
  (setf (entity-ai-state cow) :idle)
  ;; Cows wander randomly and occasionally stop to eat grass
  (when (< (random 1.0) 0.005)
    (if (< (random 1.0) 0.8)
        ;; Wander
        (progn
          (setf (entity-velocity-x cow) (- (random 1.0) 0.5))
          (entity-velocity-z cow) (- (random 1.0) 0.5))
        ;; Stop and eat
        (setf (entity-velocity-x cow) 0.0
              (entity-velocity-z cow) 0.0))))

(defun update-pig-ai (pig game-state delta-time)
  "Update pig AI behavior"
  (setf (entity-ai-state pig) :idle)
  ;; Pigs wander randomly
  (when (< (random 1.0) 0.003)
    (setf (entity-velocity-x pig) (- (random 1.0) 0.5))
    (entity-velocity-z pig) (- (random 1.0) 0.5)))

(defun update-chicken-ai (chicken game-state delta-time)
  "Update chicken AI behavior"
  (setf (entity-ai-state chicken) :idle)
  ;; Chickens wander randomly but more erratically
  (when (< (random 1.0) 0.02)
    (setf (entity-velocity-x chicken) (- (random 1.0) 0.5))
    (entity-velocity-z chicken) (- (random 1.0) 0.5)))

(defun calculate-distance (x1 y1 z1 x2 y2 z2)
  "Calculate 3D distance between two points"
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2)
           (expt (- z2 z1) 2))))

(defun entity-damage (entity damage)
  "Apply damage to an entity"
  (decf (entity-health entity) damage)
  (when (<= (entity-health entity) 0)
    (entity-die entity)))

(defun entity-die (entity)
  "Handle entity death"
  (setf (entity-health entity) 0)
  ;; In a real implementation, we would remove the entity from the world
  ;; and potentially drop items
  (case (entity-entity-type entity)
    (:zombie (drop-item-at (entity-x entity) (entity-y entity) (entity-z entity) :rotten-flesh 1))
    (:cow (drop-item-at (entity-x entity) (entity-y entity) (entity-z entity) :leather 1)
          (drop-item-at (entity-x entity) (entity-y entity) (entity-z entity) :beef 1))
    (:pig (drop-item-at (entity-x entity) (entity-y entity) (entity-z entity) :leather 1)
          (drop-item-at (entity-x entity) (entity-y entity) (entity-z entity) :porkchop 1))
    (:chicken (drop-item-at (entity-x entity) (entity-y entity) (entity-z entity) :feather 1)
              (drop-item-at (entity-x entity) (entity-y entity) (entity-z entity) :chicken 1))))

(defun drop-item-at (x y z item-id count)
  "Drop an item at the specified location"
  ;; In a real implementation, this would create a physical item entity
  ;; that players can pick up
  (format t "Dropping ~a ~a at (~a, ~a, ~a)~%" count item-id x y z))

(defun find-entities-in-radius (game-state x y z radius)
  "Find all entities within a certain radius of a point"
  (remove-if-not 
    (lambda (entity)
      (< (calculate-distance x y z 
                            (entity-x entity) (entity-y entity) (entity-z entity)) 
         radius))
    (game-state-active-entities game-state)))