(in-package :asdf-user)

(defsystem #:game-framework
  :depends-on (#:sdl2
               #:sdl2-ttf
               #:sdl2-image

               #:local-time ;; why
               
               #:cl-opengl
               #:cl-ode)
  
  :serial nil
  :components ((:file "packages")
               (:file "messages" :depends-on ("packages"))
               (:file "globals" :depends-on ("packages"))
               (:file "dpi" :depends-on ("globals"))
               (:file "resources" :depends-on ("globals"))
               (:file "transform" :depends-on ())
               (:file "spritesheet-trees" :depends-on ("globals" "transform"))
               (:file "spritesheet-textures" :depends-on ("spritesheet-trees"))
               (:file "actors" :depends-on ("spritesheet-textures") )
               (:file "fonts" :depends-on ("dpi"))
               (:file "signals" :depends-on ())))
