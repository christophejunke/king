(defpackage :fumiko (:use
                     :cl
                     :game.framework.spritesheet-trees
                     :game.framework.resources))
(in-package :fumiko)

(define-spritesheet fumiko ()
  `(nil (:file ,(sprite-path "fumiko" "Fumiko")
         :transform ,(scale 24 32)
         :order '(:row :col)
         :animation :cyclic)))

