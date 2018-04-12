(in-package :kingdom)

;; for interactive use

(run 'main-loop)

(trace compute-next-move)
(trace scancode-move)
(trace on-key-down)
(trace on-key-up)

(untrace)

(add-golden 10)

(eval-in-game
  (restart-main-loop))

(eval-in-game
  (pop (game-active-object *game*)))

(defclass mover (actor)
  ((speed :initform -3 :accessor speed-of :initarg :speed))
  (:default-initargs :y (@ 5.3)
                     :x (@ 20)
                     :width (@ .7)
                     :height (@ .7)))

(defmethod update ((mover mover) &key &allow-other-keys)
  (setf #1=(rect-x (sprite-dest-rect mover))
        (- (mod (+ #1# (speed-of mover) (@ 2))
                (+ (sdl2:get-window-size *window*) (@ 4)))
           (@ 2) ))
  (call-next-method))

(defmethod display ((mover mover) &key &allow-other-keys)
  (call-next-method))

(eval-in-game
  (change-class (first (game-active-object *game*))
                'mover))

(eval-in-game (print (class-of (first (game-active-object *game*)))) (terpri))

(eval-in-game
  (push (make-instance 'mover
                       :description *helmet-run-forward-left*
                       :x (@ 13)
                       :y (+ (@ 5.3) 3))
        (game-active-object *game*))
  (push (make-instance 'mover
                       :description *snake-crawl-left*
                       :x (@ 13.7)
                       :y (+ (@ 5.3) 3))
        (game-active-object *game*)))

(eval-in-game
  (pop (game-active-object *game*)))



(eval-in-game
  (loop repeat 7 do (add-golden-thing)))

(defun all-golden ()
  (remove-if-not (lambda (u) (typep u 'golden-thing))
                 (game-active-object *game*)))

(define-command clear-game ()
  (setf (game-active-object *game*)
        (list (find-king))))

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
