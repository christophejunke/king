(defpackage #:villagers
  (:use #:cl
        #:game.framework.spritesheet-trees
        #:game.framework.spritesheet-textures
        #:game.framework.transform        
        #:game.framework.resources))
(in-package :villagers)

(define-spritesheet villagers ()
  (:animation #'animation-reducer)
  `(_ (:file ,(sprite-path "misc" "villagers")
       :order (:row :col)
       transform ,(scale 16 16)
       transform ,(move -1 -1))
      (peoples (:row 2 :col (:range 9 1)) nil)))

(find-spritesheet 'villagers)

(probe-file "RES:SPRITES;MISC;VILLAGERS.PNG.NEWEST")
"/home/chris/src/king/resources/sprites/misc/villagers.png"
;; (format nil "~30,'0,' ,3:b" 3020304)
