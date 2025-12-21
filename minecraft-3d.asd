(defsystem "minecraft-3d"
  :version "1.0.0"
  :author "AI Assistant"
  :license "MIT"
  :depends-on ("sdl2" "sdl2-image" "sdl2-mixer" "sdl2-ttf" "trivial-gamekit" "alexandria")
  :components ((:file "package")
               (:file "defs")
               (:file "inventory")
               (:file "world-interaction")
               (:file "biome-generation")
               (:file "physics")
               (:file "entities")
               (:file "day-night-cycle")
               (:file "game-state")
               (:file "main"))
  :description "A 3D Minecraft-like game implemented in Common Lisp"
  :long-description "A comprehensive 3D block-based game with biomes, entities, inventory, physics, and more.")