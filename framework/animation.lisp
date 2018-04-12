(defpackage :game.framework.animation (:use :cl))
(in-package :game.framework.animation)

(define-spritesheet forest ()
  (:animation #'animation-reducer)
  `(_ (:file ,(sprite-path "forest" "characters")
       :order (:row :col)
       :animation :cyclic
       :transform ,(scale 32 32)
       :transform ,(move -1 -1))
      (snake (:row 4)
             (walk (:col (:range 1 4))
                   .
                   #2=((forward ()
                                (right)
                                (left (:flip :horizontal)))
                       (backward (:animation :reverse)
                                 ;; here, we flip differently: right means going to
                                 ;; the right while facing left.
                                 (right (:flip :horizontal))
                                 (left))))
             #1=(idle (:col 1)
                      (right)
                      (left (:flip :horizontal))))
      ((:each (goldie (:row 1))
              (king   (:row 2))
              (helmet (:row 3)))
       ()
       (_ (:animation :sequence)
          (slash (:col (12 11 12 13)))
          (punch (:col (14 12)))
          (hit (:col (9 10 9))))
       (jump ()
             (prepare (:col 5))
             (upward (:col 6))
             (downward (:col 7))
             (landing (:col 8)))      
       (climb (:col (:range 19 22)))
       
       ((:each (run (:col (:range 15 18)))
               (walk (:col (:range 1 4))))
        ;; forward/backward left/right too
        () . #2#)
       
       ;; idle left/right too
       #1#)))

