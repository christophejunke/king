(in-package #:game.framework.globals)

(defvar *window*)
(defvar *renderer*)
(defvar *gl*)

;; GAME
(defvar *game*)

;; Other things

(defgeneric cleanup (thing))

(defgeneric on-key-down (thing &rest args)
  (:method (x &key)))

(defgeneric on-key-up (thing &rest args)
  (:method (x &key)))

(defmethod on-key-down ((sequence sequence) &rest args)
  (some (lambda (x) (apply #'on-key-down x args)) sequence))

(defmethod on-key-up ((sequence sequence) &rest args)
  (some (lambda (x) (apply #'on-key-up x args)) sequence))

(defgeneric update (thing &key &allow-other-keys)
  (:method (x &key))
  (:method ((sequence sequence) &rest args)
    (map nil (lambda (u) (apply #'update u args)) sequence)))

(defgeneric display (thing &key &allow-other-keys)
  (:method (x &key))
  (:method ((sequence sequence) &rest args)
    (map nil (lambda (u) (apply #'display u args)) sequence)))


