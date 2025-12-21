;; Define the package for the 3D Minecraft game
(defpackage #:minecraft-3d
  (:use #:cl #:sdl2 #:sdl2-image #:sdl2-mixer #:sdl2-ttf #:trivial-gamekit)
  (:export #:main))

(in-package #:minecraft-3d)
