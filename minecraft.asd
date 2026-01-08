;; File: ~/quicklisp/local-projects/minecraft-lisp/minecraft.asd
(defsystem "minecraft"
  :version "0.1.0"
  :author "Sina"
  :license "MIT"
  ;; Explicitly list the dependencies here to ensure they are loaded first
  :depends-on (:lispbuilder-sdl :cl-opengl :cl-glu :lispbuilder-sdl-image :lispbuilder-sdl-mixer :lispbuilder-sdl-ttf)
  :components ((:file "package") ; This file should now find SDL2 available
               (:file "defs" :depends-on ("package"))
               (:file "world-generation" :depends-on ("defs"))
               (:file "rendering" :depends-on ("defs" "world-generation"))
               (:file "input" :depends-on ("defs"))
               (:file "opengl-setup" :depends-on ("defs"))
               (:file "world-interaction" :depends-on ("defs" "world-generation" "rendering"))
               ;; Add the new components from the qwen-code branch
               (:file "biome-generation" :depends-on ("defs" "world-generation"))
               (:file "entities" :depends-on ("defs" "world-interaction"))
               (:file "physics" :depends-on ("defs" "world-interaction"))
               (:file "game-state" :depends-on ("defs"))
               (:file "inventory" :depends-on ("defs" "game-state"))
               (:file "day-night-cycle" :depends-on ("defs"))
               (:file "main" :depends-on ("defs" "rendering" "input" "world-generation" "world-interaction" "opengl-setup" "biome-generation" "entities" "physics" "game-state" "inventory" "day-night-cycle")))
  :description "A Minecraft-inspired 3D game in Lisp"
  ;; Keep the :around-compile hook to point ASDF to the vendor directory
  :around-compile (lambda (next)
                    (let ((asdf:*central-registry*
                            (append (list (merge-pathnames "vendor/" (asdf:system-source-directory 'minecraft)))
                                    asdf:*central-registry*)))
                      (funcall next)))
  ;; Optionally, inherit the existing configuration after adding vendor
  ;; :in-order-to ((test-op (test-op "minecraft/tests"))) ; If you have tests
  )