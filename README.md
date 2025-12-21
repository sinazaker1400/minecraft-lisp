# Minecraft 3D Game in Common Lisp

This is a comprehensive Minecraft-like 3D game implemented in Common Lisp with the following features:

## Features Implemented

### Core Gameplay
- **3D World Generation**: Procedurally generated worlds with different biomes based on seeds
- **Block System**: Multiple block types including grass, dirt, stone, wood, leaves, water, sand, gravel, ores, and more
- **Block Interaction**: Ability to break and place blocks in the world
- **Inventory System**: Full inventory management with hotbar and storage slots
- **Physics System**: Gravity, collision detection, and player movement physics
- **Day/Night Cycle**: Dynamic lighting and time progression system

### Biomes & World Generation
- **Seed-based Generation**: Consistent world generation from a seed value
- **Multiple Biomes**: Plains, forest, desert, and ocean biomes with unique characteristics
- **Terrain Features**: Trees, flowers, grass, and underground ore generation
- **Height Variation**: Natural terrain with hills and valleys

### Items & Tools
- **Block Items**: All basic blocks can be collected and placed
- **Tools**: Pickaxes, shovels, axes, and swords with different tiers
- **Weapons**: Various weapons for combat
- **Armor**: Protective equipment for players
- **Food Items**: Items that restore health

### Entities & Mobs
- **Passive Mobs**: Cows, pigs, and chickens
- **Hostile Mobs**: Zombies that spawn at night
- **AI System**: Basic AI for mob behavior and movement
- **Combat System**: Attack and health mechanics

### Advanced Systems
- **Gravity Physics**: Realistic falling and jumping mechanics
- **Fluid System**: Water and lava with appropriate interactions
- **Lighting System**: Dynamic lighting based on time of day and block placement
- **Chunk Loading**: Dynamic chunk loading/unloading for performance
- **Game Modes**: Survival and creative modes

## Installation

1.  Install dependencies on your host using Guix (as used in development):
    ```bash
    guix install sbcl sdl2 glu
    ```
    Or install these packages depending on your operating system.
2.  Ensure you have [Quicklisp](https://www.quicklisp.org/beta/) installed.
3.  Clone the repository to "~/quicklisp/local-projects/":
    ```bash
    git clone https://github.com/sinazaker1400/minecraft-lisp.git ~/quicklisp/local-projects/minecraft
    ```
4.  Load and run the game.

    From repl:
    ```bash
    (ql:quickload :minecraft)
    (minecraft-3d:main)
    ```

    From bash:
    ```bash
    sbcl --eval '(ql:quickload :minecraft)' --eval '(minecraft-3d:main)'
    ```

    From the isolated shell of guix:
    ```bash
    guix shell --pure sbcl sdl2 glu
    export LD_LIBRARY_PATH=/run/current-system/profile/lib:$HOME/.guix-profile/lib:$LD_LIBRARY_PATH
    sbcl --eval '(ql:quickload :minecraft)' --eval '(minecraft-3d:main)'
    ```

## Usage

- **WASD**: Move player
- **Mouse**: Look around
- **Left Click**: Break blocks
- **Right Click**: Place blocks
- **Space**: Jump
- **Shift**: Sneak
- **Ctrl**: Sprint
- **1-9**: Select hotbar slots

## Technical Architecture

The game is structured into several modules:

- `defs.lisp`: Data structures and constants
- `inventory.lisp`: Inventory management system
- `world-interaction.lisp`: Block placement and breaking mechanics
- `biome-generation.lisp`: World and biome generation
- `physics.lisp`: Physics and collision detection
- `entities.lisp`: Entity and mob system
- `day-night-cycle.lisp`: Time and lighting system
- `game-state.lisp`: Game state management
- `main.lisp`: Main game loop and rendering

## Running the Game

To run the game, execute the run.lisp script:

```lisp
(load "run.lisp")
```

## Dependencies

This game requires:
- Common Lisp implementation (SBCL, CCL, etc.)
- SDL2 libraries
- OpenGL
- The following Common Lisp libraries:
  - `sdl2`
  - `sdl2-image`
  - `sdl2-mixer`
  - `sdl2-ttf`
  - `trivial-gamekit`
  - `alexandria`

## Game Modes

- **Survival Mode**: Limited resources, hunger, health management
- **Creative Mode**: Unlimited resources, flying, instant block breaking

## Day/Night Cycle

The game features a complete day/night cycle that affects:
- Lighting conditions
- Mob spawning (hostile mobs spawn at night)
- Visual atmosphere

## Future Enhancements

Potential future additions could include:
- Advanced crafting system
- More complex structures and villages
- Weather systems
- Multiplayer support
- Advanced redstone-like mechanics
- More diverse biomes and mobs
- Sound system
- Advanced UI/HUD elements
