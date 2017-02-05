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

(eval-in-game
  (push (make-instance 'actor
                       :description *goldie-punch*    
                       :x (@ 13)
                       :y (@ 5)
                       :width (@ 1)
                       :height (@ 1))
        (game-active-object *game*)))

(eval-in-game
  (pop (game-active-object *game*)))



(eval-in-game
  (loop repeat 7
        do (add-golden)))

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
