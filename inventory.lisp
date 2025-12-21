(in-package #:minecraft-3d)

;; Inventory management system
(defparameter *inventory-size* 36) ; 9 hotbar slots + 27 main inventory slots
(defparameter *hotbar-size* 9)

(defun initialize-player-inventory ()
  "Initialize a player's inventory with empty slots"
  (let ((inventory (make-array *inventory-size*)))
    (loop for i from 0 below *inventory-size*
          do (setf (aref inventory i)
                   (make-inventory-slot :item-id :air :count 0 :durability 0)))
    inventory))

(defun find-item-in-inventory (player item-id)
  "Find the first slot containing the specified item"
  (loop for i from 0 below *inventory-size*
        for slot = (aref (player-inventory player) i)
        when (eq (inventory-slot-item-id slot) item-id)
          return i
        finally (return nil)))

(defun find-empty-slot (player)
  "Find the first empty slot in the player's inventory"
  (loop for i from 0 below *inventory-size*
        for slot = (aref (player-inventory player) i)
        when (eq (inventory-slot-item-id slot) :air)
          return i
        finally (return nil)))

(defun add-item-to-inventory (player item-id count)
  "Add items to the player's inventory, stacking where possible"
  (let ((item-found (find-item-in-inventory player item-id)))
    (if item-found
        ;; Try to stack with existing items
        (let* ((slot (aref (player-inventory player) item-found))
               (item-def (find item-id *item-list* :key #'item-id))
               (max-stack (if item-def (item-max-stack-size item-def) 64))
               (current-count (inventory-slot-count slot)))
          (if (< current-count max-stack)
              ;; Stack with existing items
              (let ((new-count (min max-stack (+ current-count count))))
                (setf (inventory-slot-count slot) new-count)
                (let ((remaining (- count (- new-count current-count))))
                  (when (> remaining 0)
                    (add-item-to-inventory player item-id remaining))))
              ;; If can't stack more, find empty slot
              (let ((empty-slot (find-empty-slot player)))
                (if empty-slot
                    (setf (aref (player-inventory player) empty-slot)
                          (make-inventory-slot :item-id item-id :count remaining :durability 0))
                    ;; Inventory is full, drop items
                    (format t "Inventory full, dropping ~a items~%" remaining)))))
        ;; No existing stack found, find empty slot
        (let ((empty-slot (find-empty-slot player)))
          (if empty-slot
              (setf (aref (player-inventory player) empty-slot)
                    (make-inventory-slot :item-id item-id :count count :durability 0))
              ;; Inventory is full, drop items
              (format t "Inventory full, dropping items~%"))))))

(defun remove-item-from-inventory (player item-id count)
  "Remove items from the player's inventory"
  (let ((item-slot-index (find-item-inventory player item-id)))
    (if item-slot-index
        (let* ((slot (aref (player-inventory player) item-slot-index))
               (current-count (inventory-slot-count slot)))
          (if (>= current-count count)
              (progn
                (decf (inventory-slot-count slot) count)
                (when (<= (inventory-slot-count slot) 0)
                  (setf (inventory-slot-item-id slot) :air
                        (inventory-slot-count slot) 0))
                t) ; successfully removed
              nil)) ; not enough items
        nil))) ; item not found

(defun get-selected-item (player)
  "Get the currently selected item in the hotbar"
  (let ((selected-slot-index (player-selected-slot player)))
    (when (< selected-slot-index *hotbar-size*)
      (let ((slot (aref (player-inventory player) selected-slot-index)))
        (when (not (eq (inventory-slot-item-id slot) :air))
          (inventory-slot-item-id slot))))))

(defun get-selected-slot-content (player)
  "Get the content of the selected hotbar slot"
  (let ((selected-slot-index (player-selected-slot player)))
    (when (< selected-slot-index *hotbar-size*)
      (aref (player-inventory player) selected-slot-index))))

(defun switch-hotbar-slot (player slot-index)
  "Switch to a different hotbar slot"
  (when (and (>= slot-index 0) (< slot-index *hotbar-size*))
    (setf (player-selected-slot player) slot-index)))

;; Inventory UI functions
(defun render-inventory-ui (player)
  "Render the inventory UI overlay"
  (declare (ignore player))
  ;; This would render the inventory UI in a real implementation
  (format t "Rendering inventory UI~%"))

(defun open-inventory-screen (player)
  "Open the full inventory screen"
  (declare (ignore player))
  ;; This would open the inventory screen in a real implementation
  (format t "Opening inventory screen~%"))