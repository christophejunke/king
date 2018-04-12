(defpackage #:castle
  (:use #:cl
        #:game.framework.spritesheet-trees
        #:game.framework.spritesheet-textures
        #:game.framework.resources))
(in-package :castle)

(define-spritesheet castle ()
  (:animation #'animation-reducer)
  `(nil (:file ,(sprite-path "misc" "simple-tileset")
         :order (:row :col)
         :animation :cyclic
         transform ,(scale 32 32)
         transform ,(translate -1 -1))
        (cat (:row 1 :col 1))))

