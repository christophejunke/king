(in-package #:game.framework.actors)

(defclass actor (sprite) ())

(defmethod initialize-instance :after
    ((a actor) &key x y width height &allow-other-keys)
  (setf (sprite-dest-rect a) (make-rect x y width height)))

