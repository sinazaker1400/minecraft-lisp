# Minecraft Lisp

A 3D block-based game inspired by Minecraft, written in Common Lisp using LispBuilder-SDL and CL-OpenGL.

## Features

*   3D first-person perspective
*   WASD movement and mouse look (arrow keys for now)
*   Procedurally generated terrain based on a seed
*   Ability to walk around and explore the generated world
*   Basic block types (grass, dirt, stone)

## Installation

1.  Install dependencies on your host using Guix (as used in development):
    ```bash
    guix install sbcl sdl12-compat glu
    ```
    Or install these three packages depending on your operating system.
2.  Ensure you have [Quicklisp](https://www.quicklisp.org/beta/) installed.
3.  Clone the repository to "~/quicklisp/local-projects/":
    ```bash
    git clone https://github.com/sinazaker1400/minecraft-lisp.git ~/quicklisp/local-projects/minecraft
    ```
4.  Load and run the game.

    From repl:
    ```bash
    (ql:quickload :minecraft)
    (minecraft-3d:start-game)
    ```

    From bash:
    ```bash
    sbcl --eval '(ql:quickload :minecraft)' --eval '(minecraft-3d:start-game)'
    ```

    From the isolated shell of guix:
    ```bash
    guix shell --pure sbcl sdl12-compat glu
    export LD_LIBRARY_PATH=/run/current-system/profile/lib:$HOME/.guix-profile/lib:$LD_LIBRARY_PATH
    sbcl --eval '(ql:quickload :minecraft)' --eval '(minecraft-3d:start-game)'
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
