;; Define constants and basic data structures
(defconstant +block-size+ 32)
(defconstant +chunk-size+ 16)
(defconstant +world-width+ 16)
(defconstant +world-height+ 9)
(defconstant +screen-width+ (* +world-width+ +block-size+))
(defconstant +screen-height+ (* +world-height+ +block-size+))
(defconstant +gravity-strength+ 0.5)
(defconstant +jump-strength+ 8.0)
(defconstant +day-cycle-duration+ 120.0) ; seconds for full day/night cycle

;; Block types with properties
(defstruct block-type
  id
  name
  texture-path
  solid
  transparent
  breakable
  light-passing
  hardness ; how many hits to break (0 = unbreakable)
  stack-size
  drop-item)

;; Create block type instances
(defparameter *air-block* (make-block-type
                           :id 0
                           :name "Air"
                           :texture-path nil
                           :solid nil
                           :transparent t
                           :breakable nil
                           :light-passing t
                           :hardness 0
                           :stack-size 0
                           :drop-item nil))

(defparameter *grass-block* (make-block-type
                             :id 1
                             :name "Grass"
                             :texture-path "textures/grass.png"
                             :solid t
                             :transparent nil
                             :breakable t
                             :light-passing nil
                             :hardness 1
                             :stack-size 64
                             :drop-item '(:dirt 1)))

(defparameter *dirt-block* (make-block-type
                            :id 2
                            :name "Dirt"
                            :texture-path "textures/dirt.png"
                            :solid t
                            :transparent nil
                            :breakable t
                            :light-passing nil
                            :hardness 1
                            :stack-size 64
                            :drop-item '(:dirt 1)))

(defparameter *stone-block* (make-block-type
                             :id 3
                             :name "Stone"
                             :texture-path "textures/stone.png"
                             :solid t
                             :transparent nil
                             :breakable t
                             :light-passing nil
                             :hardness 3
                             :stack-size 64
                             :drop-item '(:cobblestone 1)))

(defparameter *wood-block* (make-block-type
                            :id 4
                            :name "Wood"
                            :texture-path "textures/wood.png"
                            :solid t
                            :transparent nil
                            :breakable t
                            :light-passing nil
                            :hardness 2
                            :stack-size 64
                            :drop-item '(:wood 1)))

(defparameter *leaves-block* (make-block-type
                              :id 5
                              :name "Leaves"
                              :texture-path "textures/leaves.png"
                              :solid nil
                              :transparent t
                              :breakable t
                              :light-passing t
                              :hardness 1
                              :stack-size 64
                              :drop-item '(:stick 1)))

(defparameter *water-block* (make-block-type
                             :id 6
                             :name "Water"
                             :texture-path "textures/water.png"
                             :solid nil
                             :transparent t
                             :breakable nil
                             :light-passing t
                             :hardness 0
                             :stack-size 0
                             :drop-item nil))

(defparameter *sand-block* (make-block-type
                            :id 7
                            :name "Sand"
                            :texture-path "textures/sand.png"
                            :solid t
                            :transparent nil
                            :breakable t
                            :light-passing nil
                            :hardness 1
                             :stack-size 64
                             :drop-item '(:sand 1)))

(defparameter *gravel-block* (make-block-type
                              :id 8
                              :name "Gravel"
                              :texture-path "textures/gravel.png"
                              :solid t
                              :transparent nil
                              :breakable t
                              :light-passing nil
                              :hardness 1
                              :stack-size 64
                              :drop-item '(:gravel 1)))

(defparameter *coal-ore-block* (make-block-type
                                :id 9
                                :name "Coal Ore"
                                :texture-path "textures/coal-ore.png"
                                :solid t
                                :transparent nil
                                :breakable t
                                :light-passing nil
                                :hardness 2
                                :stack-size 64
                                :drop-item '(:coal 1)))

(defparameter *iron-ore-block* (make-block-type
                                :id 10
                                :name "Iron Ore"
                                :texture-path "textures/iron-ore.png"
                                :solid t
                                :transparent nil
                                :breakable t
                                :light-passing nil
                                :hardness 3
                                :stack-size 64
                                :drop-item '(:raw-iron 1)))

(defparameter *gold-ore-block* (make-block-type
                                :id 11
                                :name "Gold Ore"
                                :texture-path "textures/gold-ore.png"
                                :solid t
                                :transparent nil
                                :breakable t
                                :light-passing nil
                                :hardness 3
                                :stack-size 64
                                :drop-item '(:raw-gold 1)))

(defparameter *diamond-ore-block* (make-block-type
                                   :id 12
                                   :name "Diamond Ore"
                                   :texture-path "textures/diamond-ore.png"
                                   :solid t
                                   :transparent nil
                                   :breakable t
                                   :light-passing nil
                                   :hardness 4
                                   :stack-size 64
                                   :drop-item '(:diamond 1)))

(defparameter *lava-block* (make-block-type
                            :id 13
                            :name "Lava"
                            :texture-path "textures/lava.png"
                            :solid nil
                            :transparent t
                            :breakable nil
                            :light-passing t
                            :hardness 0
                            :stack-size 0
                            :drop-item nil))

(defparameter *glass-block* (make-block-type
                             :id 14
                             :name "Glass"
                             :texture-path "textures/glass.png"
                             :solid t
                             :transparent t
                             :breakable t
                             :light-passing t
                             :hardness 1
                             :stack-size 64
                             :drop-item '(:glass 1)))

(defparameter *block-types-hash*
  (alexandria:plist-hash-table
    '(0 *air-block*
      1 *grass-block*
      2 *dirt-block*
      3 *stone-block*
      4 *wood-block*
      5 *leaves-block*
      6 *water-block*
      7 *sand-block*
      8 *gravel-block*
      9 *coal-ore-block*
      10 *iron-ore-block*
      11 *gold-ore-block*
      12 *diamond-ore-block*
      13 *lava-block*
      14 *glass-block*)
    :test 'equal))

;; Item definitions
(defstruct item
  id
  name
  icon-path
  max-stack-size
  category) ; :block, :tool, :weapon, :armor, :food, :misc

(defparameter *item-list*
  (list
    (make-item :id :air :name "Air" :icon-path nil :max-stack-size 0 :category :misc)
    (make-item :id :grass :name "Grass Block" :icon-path "icons/grass.png" :max-stack-size 64 :category :block)
    (make-item :id :dirt :name "Dirt" :icon-path "icons/dirt.png" :max-stack-size 64 :category :block)
    (make-item :id :stone :name "Stone" :icon-path "icons/stone.png" :max-stack-size 64 :category :block)
    (make-item :id :wood :name "Wood Planks" :icon-path "icons/wood.png" :max-stack-size 64 :category :block)
    (make-item :id :leaves :name "Leaves" :icon-path "icons/leaves.png" :max-stack-size 64 :category :block)
    (make-item :id :sand :name "Sand" :icon-path "icons/sand.png" :max-stack-size 64 :category :block)
    (make-item :id :gravel :name "Gravel" :icon-path "icons/gravel.png" :max-stack-size 64 :category :block)
    (make-item :id :coal :name "Coal" :icon-path "icons/coal.png" :max-stack-size 64 :category :misc)
    (make-item :id :raw-iron :name "Raw Iron" :icon-path "icons/raw-iron.png" :max-stack-size 64 :category :misc)
    (make-item :id :raw-gold :name "Raw Gold" :icon-path "icons/raw-gold.png" :max-stack-size 64 :category :misc)
    (make-item :id :diamond :name "Diamond" :icon-path "icons/diamond.png" :max-stack-size 64 :category :misc)
    (make-item :id :glass :name "Glass" :icon-path "icons/glass.png" :max-stack-size 64 :category :block)
    (make-item :id :stick :name "Stick" :icon-path "icons/stick.png" :max-stack-size 64 :category :misc)
    ;; Tools
    (make-item :id :wooden-pickaxe :name "Wooden Pickaxe" :icon-path "icons/wooden-pickaxe.png" :max-stack-size 1 :category :tool)
    (make-item :id :stone-pickaxe :name "Stone Pickaxe" :icon-path "icons/stone-pickaxe.png" :max-stack-size 1 :category :tool)
    (make-item :id :iron-pickaxe :name "Iron Pickaxe" :icon-path "icons/iron-pickaxe.png" :max-stack-size 1 :category :tool)
    (make-item :id :wooden-shovel :name "Wooden Shovel" :icon-path "icons/wooden-shovel.png" :max-stack-size 1 :category :tool)
    (make-item :id :stone-shovel :name "Stone Shovel" :icon-path "icons/stone-shovel.png" :max-stack-size 1 :category :tool)
    (make-item :id :iron-shovel :name "Iron Shovel" :icon-path "icons/iron-shovel.png" :max-stack-size 1 :category :tool)
    (make-item :id :wooden-axe :name "Wooden Axe" :icon-path "icons/wooden-axe.png" :max-stack-size 1 :category :tool)
    (make-item :id :stone-axe :name "Stone Axe" :icon-path "icons/stone-axe.png" :max-stack-size 1 :category :tool)
    (make-item :id :iron-axe :name "Iron Axe" :icon-path "icons/iron-axe.png" :max-stack-size 1 :category :tool)
    ;; Weapons
    (make-item :id :wooden-sword :name "Wooden Sword" :icon-path "icons/wooden-sword.png" :max-stack-size 1 :category :weapon)
    (make-item :id :stone-sword :name "Stone Sword" :icon-path "icons/stone-sword.png" :max-stack-size 1 :category :weapon)
    (make-item :id :iron-sword :name "Iron Sword" :icon-path "icons/iron-sword.png" :max-stack-size 1 :category :weapon)
    ;; Armor
    (make-item :id :leather-helmet :name "Leather Helmet" :icon-path "icons/leather-helmet.png" :max-stack-size 1 :category :armor)
    (make-item :id :leather-chestplate :name "Leather Chestplate" :icon-path "icons/leather-chestplate.png" :max-stack-size 1 :category :armor)
    (make-item :id :leather-leggings :name "Leather Leggings" :icon-path "icons/leather-leggings.png" :max-stack-size 1 :category :armor)
    (make-item :id :leather-boots :name "Leather Boots" :icon-path "icons/leather-boots.png" :max-stack-size 1 :category :armor)
    ;; Food
    (make-item :id :apple :name "Apple" :icon-path "icons/apple.png" :max-stack-size 64 :category :food)
    (make-item :id :bread :name "Bread" :icon-path "icons/bread.png" :max-stack-size 64 :category :food)))

;; Entity types
(defstruct entity
  id
  x y z
  health
  max-health
  velocity-x velocity-y velocity-z
  width height depth
  entity-type ; :player, :zombie, :cow, :pig, :chicken, :skeleton
  ai-state ; for mobs: :idle, :chasing, :attacking, :fleeing
  ai-target ; target for AI
  equipped-items ; list of currently equipped items
  active-effect ; status effects like poison, regeneration, etc.
  )

;; Inventory slot
(defstruct inventory-slot
  item-id
  count
  durability) ; for tools/weapons

;; Player structure
(defstruct player
  x y z
  rotation-x rotation-y ; pitch and yaw
  velocity-x velocity-y velocity-z
  health
  hunger
  oxygen
  inventory ; array of inventory-slot structs
  selected-slot ; index of currently selected hotbar slot (0-8)
  active-item ; currently held item
  on-ground
  in-water
  in-lava
  sprinting
  sneaking
  flying
  creative-mode)

;; World chunk structure
(defstruct chunk
  x z
  blocks ; 3D array [x y z] of block IDs
  lighting ; 3D array of light values
  loaded
  dirty ; needs to be redrawn
  entities ; list of entities in this chunk
  )

;; Biome types
(defstruct biome
  id
  name
  temperature
  humidity
  height-variance
  tree-density
  flower-density
  grass-density
  water-level
  underground-blocks ; list of block types for underground generation
  surface-blocks) ; list of block types for surface generation

;; Create biome definitions
(defparameter *biome-plains*
  (make-biome
    :id :plains
    :name "Plains"
    :temperature 0.8
    :humidity 0.4
    :height-variance 0.1
    :tree-density 0.05
    :flower-density 0.2
    :grass-density 0.8
    :water-level 63
    :underground-blocks '(:stone :coal-ore :iron-ore :gold-ore :diamond-ore)
    :surface-blocks '(:grass :dirt :sand :gravel)))

(defparameter *biome-forest*
  (make-biome
    :id :forest
    :name "Forest"
    :temperature 0.7
    :humidity 0.8
    :height-variance 0.2
    :tree-density 0.4
    :flower-density 0.1
    :grass-density 0.6
    :water-level 63
    :underground-blocks '(:stone :coal-ore :iron-ore :gold-ore)
    :surface-blocks '(:grass :dirt :wood :leaves :sand :gravel)))

(defparameter *biome-desert*
  (make-biome
    :id :desert
    :name "Desert"
    :temperature 1.0
    :humidity 0.0
    :height-variance 0.3
    :tree-density 0.01
    :flower-density 0.0
    :grass-density 0.1
    :water-level 63
    :underground-blocks '(:stone :coal-ore :iron-ore)
    :surface-blocks '(:sand :gravel :sandstone)))

(defparameter *biome-ocean*
  (make-biome
    :id :ocean
    :name "Ocean"
    :temperature 0.5
    :humidity 1.0
    :height-variance 0.05
    :tree-density 0.0
    :flower-density 0.0
    :grass-density 0.0
    :water-level 62
    :underground-blocks '(:stone :coal-ore :iron-ore :gravel)
    :surface-blocks '(:sand :gravel :dirt)))

(defparameter *biome-list*
  (list
    *biome-plains*
    *biome-forest*
    *biome-desert*
    *biome-ocean*))

;; Game state structure
(defstruct game-state
  world ; hash table of chunks
  player
  time-of-day ; 0.0 to 1.0 representing time through day cycle
  weather ; :clear, :rain, :thunder
  difficulty ; :peaceful, :easy, :normal, :hard
  game-mode ; :survival, :creative, :adventure
  active-entities ; list of all active entities in the world
  active-light-sources ; list of light-emitting entities/blocks
  seed ; world generation seed
  )
