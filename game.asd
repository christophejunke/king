(in-package :asdf-user)

(defsystem #:game
  :depends-on (#:sdl2 #:sdl2-ttf #:local-time #:cl-opengl #:sb-cga #:chanl)
  :serial t
  :components ((:module framework
                :serial nil
                :components
                ((:file "packages")
                 (:file "globals" :depends-on ("packages"))
                 (:file "dpi" :depends-on ("globals"))
                 (:file "resources" :depends-on ("globals"))
                 (:file "spritesheet-trees" :depends-on ("globals"))
                 (:file "spritesheet-textures" :depends-on ("spritesheet-trees"))
                 (:file "actors" :depends-on ("spritesheet-textures") )
                 (:file "fonts" :depends-on ("dpi"))
                 (:file "signals" :depends-on ())))

               (:file "packages")
               (:file "kingdom")
               (:file "main")))
