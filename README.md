# Minecraft Lisp

A 3D block-based game inspired by Minecraft, written in Common Lisp using LispBuilder-SDL and CL-OpenGL.

## Features

*   3D first-person perspective
*   WASD movement and mouse look (arrow keys for now)
*   Procedurally generated terrain based on a seed
*   Ability to walk around and explore the generated world
*   Basic block types (grass, dirt, stone)

## Installation

1.  Ensure you have [SBCL](https://www.sbcl.org/) installed.
2.  Ensure you have [Quicklisp](https://www.quicklisp.org/beta/) installed.
3.  Clone the repository:
    ```bash
    git clone https://github.com/sinazaker1400/minecraft-lisp.git
    cd minecraft-lisp
    ```
4.  Install dependencies using Guix (as used in development):
    ```bash
    guix install sbcl-lispbuilder-sdl cl-opengl cl-glu
    ```
    (Or ensure `lispbuilder-sdl`, `cl-opengl`, `cl-glu` are available via Quicklisp/ASDF)
5.  Run the game:
    ```bash
    sbcl --load ~/quicklisp/setup.lisp --eval '(require :asdf)' --eval '(asdf:load-system :lispbuilder-sdl)' --eval '(asdf:load-system :cl-opengl)' --eval '(asdf:load-system :cl-glu)' --eval '(load "minecraft.lisp")' --eval '(minecraft-3d:start-game)'
    ```

## Usage

*   `WASD`: Move forward, backward, strafe left, strafe right
*   `Space`: Move up (fly/jump)
*   `LShift`: Move down (fly/descend)
*   `Arrow Keys (Left/Right)`: Look left/right (yaw)
*   `Arrow Keys (Up/Down)`: Look up/down (pitch)
*   `ESC`: Quit the game

The world is generated based on a seed (currently hardcoded as 12345 in `render-world`).

## Current Status

This is an early prototype demonstrating 3D rendering, basic movement, and seed-based terrain generation.

## Next Steps

*   Implement a chunk-based world system for larger worlds.
*   Add block breaking and placing.
*   Implement collision detection.
*   Add textures.
*   Improve lighting.
*   Add an inventory system.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
