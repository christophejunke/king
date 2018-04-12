(in-package :asdf-user)

(defsystem #:game
  :depends-on (#:game-framework)
  :serial t
  :components ((:file "packages")
               (:file "kingdom")
               (:file "main")))


