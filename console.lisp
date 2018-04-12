(in-package :kingdom)

;; for interactive use

(defun is-class (class)
  (let ((class (find-class class)))
    (lambda (object)
      (eq class (class-of object)))))

(defun is-instance (class)
  (let ((class (find-class class)))
    (lambda (object)
      (typep object class))))

(run 'followers)

(trace compute-next-move)
(trace scancode-move)
(trace on-key-down)
(trace on-key-up)

(untrace)

(add-golden 10)

(eval-in-game
  (restart-main-loop))

(eval-in-game
  (game-active-object *game*))

(defclass pimple ()
  ((rate :accessor pimple-rate :initform 2 :initarg :rate)
   (frame :accessor pimple-frame :initform 0 :initarg :frame))
  (:default-initargs :y (@ 5.5)
                     :x (@ 20)
                     :width (@ .5)
                     :height (@ .5)))

(defmethod update :around ((p pimple) &key &allow-other-keys)
  (setf (pimple-frame p)
        (mod (1+ (pimple-frame p)) (pimple-rate p)))
  (when (zerop (pimple-frame p))
    (call-next-method)
    (unless (plusp (random 5))
      (call-next-method))))

(defclass mover (actor)
  ((speed :initform -3 :accessor speed-of :initarg :speed)))

(defclass people (actor pimple)
  ())

(defclass moving-people (people mover) ())

(defmethod update ((mover mover) &key &allow-other-keys)
  (setf #1=(rect-x (sprite-dest-rect mover))
        (- (mod (+ #1# (speed-of mover) (@ 2))
                (+ (sdl2:get-window-size *window*) (@ 4)))
           (@ 2) ))
  (call-next-method))

(current-game)

(defclass text-objet ()
  (text :initform "" :initarg :text :accessor text-of))

(eval-in-game
  (pop (game-active-object *game*)))

(eval-in-game (print (class-of (first (game-active-object *game*)))) (terpri))

(map () (lambda (u) (change-class u 'mover)) ($filter 'golden-follower))

(eval-in-game
  (dotimes (i 1)
    (push (make-instance 'mover
                         :description *robot-walk-left*
                         :speed -2
                         :x (+ (@ 12) (random (@ 4)))
                         :y (@ 5)
                         :width (@ 1)
                         :height (@ 1))
          (game-active-object *game*))))

(defun change-sprite (value &rest tree-path)
  (spritef (sprite-description value) tree-path))

 (eval-in-game
  (change-sprite (find (find-class 'mover)
                       (game-active-object *game*)
                       :key #'class-of)
                 'right))

(defun most-recent ()
  (eval-in-game (first (game-active-object (current-game)))))

(incf (rect-y (sprite-dest-rect (most-recent))) 10)

(eval-in-game
 (game.framework.spritesheet-textures::activate-spritesheet 'viking))

(eval-in-game
  (game.framework.spritesheet-textures::%update-sprite
   (first (game-active-object (current-game)))))

(eval-in-game game.framework.spritesheet-textures::*active-textures*)

(eval-in-game
  (setf (second game.framework.spritesheet-textures::*active-textures*)
        (game.framework.spritesheet-textures::make-textures-from-spritesheet
         (find-spritesheet 'viking)))

  (game.framework.spritesheet-textures::%update-sprite
   (first (game-active-object (current-game))))
  )

(eval-in-game (pop game.framework.spritesheet-textures::*active-textures*))

(eval-in-game
  (setf (slot-value (first (game-active-object (current-game)))
                    'game.framework.spritesheet-textures::%textures)
        (second game.framework.spritesheet-textures::*active-textures*)))

(loop for object in (game-active-object (current-game))
      when (typep object 'mover)
        do (incf (rect-y (sprite-dest-rect object))))

;; (describe *slime-hero*)
;; #<MOVER {100AEFF133}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   %TEXTURES    = #S(GAME.FRAMEWORK.SPRITESHEET-TEXTURES::TEXTURES..
;;   DESCRIPTION  = #<SPRITE-DESCRIPTION [NAME *ROBOT-WALK-LEFT*][PARENT LEFT]>
;;   ANIMATOR     = :CYCLIC
;;   DEST-RECT    = #<SDL2-FFI:SDL-RECT x 622 y 326 w 64 h 64 {100AF40FE3}>
;;   TEXTURE      = #<SDL2-FFI:SDL-TEXTURE {#X7FFFD6F3A200}>
;;   RECTANGLES   = #(#<SDL2-FFI:SDL-RECT x 0 y 768 w 128 h 128 {100785CDE3}>..
;;   CURSOR       = 5
;;   SPEED        = -3

(eval-in-game
  (spritef (sprite-description
            (find-king))
           '(king)))

(eval-in-game
  (loop for (a _) on game.framework.spritesheet-textures::*active-textures* by #'cddr collect a))

(defvar *slime-hero* (find-object (is-class 'mover)))

(eval-in-game
  (game.framework.spritesheet-textures::%update-sprite *slime-hero*))

(defclass viking-background-mover (mover)
  ()
  (:default-initargs :x (+ (@ 12) (random (@ 4)))
                     :y (+ (@ 5) 5)
                     :width (@ 1)
                     :height (@ 1) ))

(eval-in-game
  (dotimes (i 1)
    (push (make-instance 'mover
                         :description *sellsword-walk-left* 
                         :speed 5
                         )
          (game-active-object *game*))))

(defclass sellsword (viking-background-mover) ())
(defclass sellsword-left (sellsword) ()
  (:default-initargs :speed -5 :description *sellsword-walk-left*))
(defclass sellsword-right (sellsword) ()
  (:default-initargs :speed 5 :description *sellsword-walk-right*))

(defclass robot (viking-background-mover) ()
  (:default-initargs :y (+ 10 (@ 5))))

(defclass robot-left (robot) ()
  (:default-initargs :speed -3 :description *robot-walk-left*))
(defclass robot-right (robot) ()
  (:default-initargs :speed 3 :description *robot-walk-right*))

(eval-in-game
  (push (make-instance 'sellsword-right)
        (game-active-object *game*)))

(eval-in-game
  (push (make-instance 'robot-left)
        (game-active-object *game*)))

(let ((*game* (current-game)))
  (let ((all (game-active-object *game*)))
    (setf (game-active-object *game*)
          (remove-if (is-instance 'sellsword-right) all))))

(dolist (robot (remove-if-not (is-instance 'robot)
                              (game-active-object (current-game))))
  (return (rect-y (sprite-dest-rect robot))))

(setf (game-active-object (current-game))
      (remove-if (is-instance 'follower)
                 (game-active-object (current-game))))

(eval-in-game
  (dotimes (i 1)
    (push (make-instance 'mover
                         :description *viking-walk-left*
                         :speed -6
                         :x (+ (@ 12) (random (@ 4)))
                         :y (@ 5)
                         :width (@ 1)
                         :height (@ 1))
          (game-active-object *game*))))

(eval-in-game
  (dotimes (i 1)
    (push (make-instance 'mover
                         :description *slime-small-walk-right* 
                         :speed -4
                         :x (+ (@ 12) (random (@ 4)))
                         :y (+ (@ 5) 5)
                         :width (@ 1)
                         :height (@ 1))
          (game-active-object *game*))))

(with-accessors ((speed speed-of))  (first (game-active-object (current-game)))
  (setf speed (- speed)))

(eval-in-game
  (push (make-instance 'actor :description *robot-shoot-left* :x (@ 8) :y (+ (@ 5) 5) :width (@ 1) :height (@ 1))
        (game-active-object *game*)))

(pop (game-active-object (current-game)))

(eval-in-game
  (dotimes (i 50)
    (push (make-instance 'moving-people
                         :description (switch-to-sprite
                                       (switch-to-sprite *pimples-0-woman-right-run* (random 3))
                                       (alexandria:random-elt '(man woman)))
                         :speed 3
                         :rate 1
                         :width (@ .5)
                         :height (@ .5)
                         :y (@ 5.5)
                         :cursor (random 2)
                         :x (random (@ 4)))
          (game-active-object *game*))))

(eval-in-game
  (make-instance 'moving-people
                 :description (switch-to-sprite
                               (switch-to-sprite *pimples-0-woman-right-run* (random 3))
                               (alexandria:random-elt '(man woman)))
                 :speed 3
                 :rate 1
                 :cursor (random 2)
                 :x (random (@ 4))))

(spritef (sprite-description
          (find-if (lambda (u) (typep u 'mover)) (game-active-object (current-game))))
         'snake)

(eval-in-game
  (setf (sprite-description
         (find-if (lambda (u) (typep u 'follower)) (game-active-object *game*)))
        *robot-walk-right*))

(eval-in-game
  (incf (rect-y (sprite-dest-rect 
                 (find-if (lambda (u) (typep u 'follower)) (game-active-object *game*))))
        2))

(eval-in-game
  (change-class
   (find-if (lambda (u) (typep u 'follower)) (game-active-object *game*))
   'human-follower))

(define-command $remove-if (predicate)
  (setf (game-active-object *game*)
        (remove-if predicate (game-active-object *game*))))

(define-command $filter (type)
  (remove-if-not (lambda (u) (typep u type)) (game-active-object *game*)))

(define-command $pop ()
  (pop (game-active-object *game*)))

($remove-if (of-type 'golden-follower))

(map () (lambda (p) (change-class p 'garbage))
     (remove-if-not (lambda (u) (typep u 'people)) (game-active-object (current-game))))

(eval-in-game
 (setf (game-active-object *game*)
       (remove-if (lambda (p) (typep p 'people))
                  (game-active-object *game*))))

(map 'list #'sprite-description ($filter 'mover))

(dolist (u ($filter 'mover))
  (setf (speed-of u) (1+ (random  7)))
  (spritef (sprite-description u) 'right)
  (spritef (sprite-description u) 'run))

(let ((list
        (shuffle
         (remove-if-not (lambda (u) (typep u 'mover)) (game-active-object (current-game))))))
  (map ()
       #'reverse-mover
       (subseq list 0 (floor (length list) 2))))

(change-class (first (game-active-object (current-game))) 'moving-people)

(spritef (sprite-description (first (game-active-object (current-game))))
         'snake)

;; (loop for (a b) on list while b always (char= (last-elt a) (first-elt b)))

(eval-in-game
  (push
   (make-instance 'actor :description *boom*
                         :x (@ 10)
                         :y (@ 5)
                         :height (@ 1)
                         :width (@ 1))
   (game-active-object *game*)))

(eval-in-game
  (push
   (make-instance 'actor :description *flame*
                         :x (@ 8)
                         :y (@ 5)
                         :height (@ 1)
                         :width (@ 1))
   (game-active-object *game*)))

(eval-in-game
  (push (make-instance 'people
                       :description *pimples-0-ork-left-run*
                       :x (@ 10))
        (game-active-object *game*)))

(defun reverse-mover (mover)
  (setf (speed-of mover) (- (speed-of mover)))
  (spritef (sprite-description mover)
           (if (plusp (speed-of mover))
               'kingdom:right
               'kingdom:left)))

(defclass garbage () ())

(push :mover *features*)

(defun moverp (x) (typep x 'mover))

(change-class 
 (find-if (lambda (u) (typep u 'golden-follower)) (game-active-object (current-game)))
 'garbage)



(reverse-mover
 (find-if #'moverp (game-active-object (current-game))))

(setf *game* (current-game))

(game.framework.spritesheet-trees:spritef
 (sprite-description (find-objects #'kingp))
 'snake)

;;(cl-user::test)

(eval-in-game (game-active-object *game*))

(eval-in-game (remove-if-not (lambda (u) (typep u 'mover)) (game-active-object *game*)))

(eval-in-game
  (pop (game-active-object *game*)))

(spritef (sprite-description (first (game-active-object (current-game))))
         '(ork v1 dead))

(eval-in-game (sdl2:get-window-flags *window*))

(first (game-active-object (current-game)))




(eval-in-game
  (push
   (make-instance 'actor
                  :description *pimples-0-woman-climb* 
                  :x (@ 7)
                  :y (@ 5.5)
                  :width (@ 1/2)
                  :height (@ 1/2))
   (game-active-object *game*)))

(eval-in-game
  (loop repeat 7
        do (add-golden-thing)))

(defun all-golden ()
  (remove-if-not (lambda (u) (typep u 'golden-thing))
                 (game-active-object *game*)))

(define-command clear-game ()
  (setf (game-active-object *game*)
        (list (find-king))))

(setf (game-active-object (current-game))
      (remove-if (is-class 'moving-people)
                 (game-active-object (current-game))))

(lone-king)

(eval-in-game
  (add-golden))

(eval-in-game
  (print (ground-level (second (game-static-object *game*)))))

(eval-in-game
  (print (king-speed (find-king))))

(eval-in-game
  (setf (game-steps *game*) 3))

(eval-in-game
  (setf (sprite-description (first *game*))
        *king-walk-forward-right* ))

(cl-user::test)
