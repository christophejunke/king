(defpackage #:gamedev-market
  (:use #:cl
        #:sdl2
        #:game.framework.spritesheet-trees
        #:game.framework.spritesheet-textures
        #:game.framework.resources))
(in-package :gamedev-market)

(define-spritesheet market ()
  (:animation #'animation-reducer)
  `(_ (:file ,(sprite-path "market")
       :order (:row :col)
       transform ,(scale 16 16)
       transform ,(translate -1 -1))
      (market
       (:col (1 . 4))
       ,@(loop
           for row from 1 upto 16
           for name = row
           collect `(,name (:row ,row)
                           down
                           (right (transform ,(translate 4 0)))
                           (up (transform ,(translate 8 0)))
                           (left (transform ,(translate 12 0))))))))

(find-spritesheet 'market)

