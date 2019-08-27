(in-package :kingdom)

(defclass animated-sprite-description (sprite-description)
  ((transition-points :accessor transitions-points :initform nil)))

(defmethod initialize-instance :after
    ((description animated-sprite-description) &rest args)
  (declare (ignore args))
  (with-accessors ((transitions transitions-points)
                   (rectangles sprite-description-rectangles))
      description
    (setf transitions (copy-sequence 'vector rectangles))
    (map-into transitions #'first transitions)
    (map-into rectangles #'second rectangles)))

(defstruct tagged-coordinate value tag)

(defgeneric tagged-coordinate-reducer (old new)
  (:method ((old tagged-coordinate) new)
    (tagged-coordinate-reducer (tagged-coordinate-value old) new))
  (:method ((old number) (new tagged-coordinate))
    new)
  (:method ((old number) (new number))
    new)
  (:method ((old number) tag)
    (make-tagged-coordinate :value old :tag tag)))

(defun animated-tile-callback (&key tile-indices env transform)
  (flet ((decode-tag (coord)
           (etypecase coord
             (integer coord)
             (tagged-coordinate (values (tagged-coordinate-value coord)
                                        (tagged-coordinate-tag coord)))
             (cons (values (second coord)
                           (cons (first coord)
                                 (last coord)))))))
    (let ((row (getf tile-indices :row))
          (col (getf tile-indices :col))
          (idx (getf tile-indices :index)))
      (multiple-value-bind (col col-tag) (decode-tag col)
        (multiple-value-bind (row row-tag) (decode-tag row)
          (list (append col-tag row-tag)
                (game.framework.spritesheet-trees::sdl-rect-from-env
                 :tile-indices `(:index ,idx
                                 :row ,row
                                 :col ,col)
                 :env env
                 :transform transform)))))))

(define-spritesheet forest ()
  (:animation #'animation-reducer)
  (:row #'tagged-coordinate-reducer)
  (:col #'tagged-coordinate-reducer)
  `(_ (:file ,(sprite-path "forest" "characters")
       :order (:row :col)
       :animation :cyclic
       :transform ,(scale 32)
       :transform ,(move -1)
       :tile-callback ,#'animated-tile-callback
       :sprite-description-class animated-sprite-description)
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
          (slash (:col (12 (:on 11 (:damage hit)) 12 13)))
          (punch (:col (14 12)))
          (hit (:col (9 10 9))))
       (jump ()
             (prepare (:col 5)
                      (left (:flip :horizontal))
                      (right))
             (upward (:col 6)
                     (left (:flip :horizontal))
                     (right))
             (downward (:col 7)
                       (left (:flip :horizontal))
                       (right))
             (landing (:col 8)
                      (left (:flip :horizontal))
                      (right)))
       (climb (:col (:range 19 22)))
       
       ((:each (run (:col (:range 15 18) :speed :fast))
               (walk (:col (:range 1 4))))
        ;; forward/backward left/right too
        () . #2#)
       
       ;; idle left/right too
       #1#)))

(defun tile-indices (sprite)
  (let* ((env (sprite-description-env sprite))
         (order (getf env :order))
         (result))
    (game.framework.spritesheet-trees::map-tile-indices-in-order
     order
     env
     (lambda (&rest coords)
       (push coords  result)))
    (nreverse result)))

;;
;;
;; (length (spritesheet-sprites (find-spritesheet 'pimples))) => 168
;;

(define-spritesheet pimples ()
  `(pimples
    (:file ,(sprite-path "pimples.bmp")
     :row 0 :col 0
     :order (:row :col)
     :transform ,(scale 16 16))

    ;; Dummy sprite. For example:
    ;;
    ;; (switch-to-sprite *pimples-empty*
    ;;                   (list (random 3)
    ;;                         (random-elt '(ork skeleton ghost)
    ;;                         (random-elt '(left right)
    ;;                         'run))
    ;;
    nil

    ;; right part 
    (nil
     (:transform ,(move 26 1))
         
     ;; Each variant in a different column (:index will be 0, 1, 2)
     ((:each (:index ())
             (:index (:transform ,(move 6 0)))
             (:index (:transform ,(move 12 0))))
      ()
      (nil (:class creature)
           ((:each (man (:row 0))
                   (woman (:row 1))
                   (ork (:row 2))
                   (beast (:row 3))
                   (monk (:row 4))
                   (skeleton (:row 5))
                   (ghost (:row 6)))
            ()
            (climb (:col 3))
            (dead (:col 5))
            ((:each (left (:flip :horizontal))
                    (right))
             ()
             (idle (:col 0))
             (run (:col (1 2)))
             (attack (:col 4)))))

          

      ;; Other creatures
      (bat (:row 7)
           (move (:col (0 1))
                 (left (:flip :horizontal))
                 (right))
           (dead (:col 5)))))))


(define-spritesheet dirt-tiles ()
  `(dirt
    (:file ,(sprite-path "forest" "dirt-tiles")
     :row 0
     :col 0
     :animation :static
     :order (:row :col)
     :transform ,(scale 24))
    (_ (:transform ,(move 8 0))
       ((:each (dirt (:row 0))
               (rock (:row 4))
               (brick (:row 8))
               (glow  (:row 12))
               (green (:row 12 :transform ,(move -8 0))))
        ()
        (test (:col 0))))))

(define-spritesheet flame ()
  `(flame (:file ,(sprite-path "flame")
           :order (:row :col)
           :row 0
           :col (0 1 2 3)
           :transform ,(scale 128 128))))

(define-spritesheet explosion ()
  `(boom (:file ,(sprite-path "boom")
          :order (:row :col)
          :row ,(iota 8)
          :col ,(iota 8)
          :transform ,(scale 128 128))))

(defvar *game* nil)

(defgeneric set-color-from (x))

(defstruct background r g b)

(defmethod set-color-from ((b background))
  (set-render-draw-color *renderer*
                         (background-r b)
                         (background-g b)
                         (background-b b)
                         255))

(defmethod display ((b background) &key &allow-other-keys)
  (set-color-from b)
  (render-clear *renderer*))

(defstruct (ground (:include background)) level)

(defmethod display ((g ground) &key &allow-other-keys)
  (multiple-value-bind (width height)
      (get-window-size *window*)
    (set-color-from g)
    (with-accessors ((level ground-level)) g
      (render-fill-rect
       *renderer*
       (make-rect 0 (- height level) width level)))))

;; ================================================================

(defclass has-speed ()
  ((speed :initarg :speed :accessor speed-of :initform 1)))

(defclass king (actor has-speed)
  ((state :initform :idle :accessor king-state)
   (speed :accessor king-speed)
   (next-move :initform nil :accessor king-next-move)
   (dx :initform 0 :accessor king-dx)
   (dy :initform 0 :accessor king-dy))
  (:default-initargs :description *king-idle-right* 
                     :width (game-tile-size *game*)
                     :height (game-tile-size *game*)))

(defun scancode-move (scancode)
  (cond
    ((scancode= scancode :scancode-space) :stop)
    ((scancode= scancode :scancode-left) :left)
    ((scancode= scancode :scancode-right) :right)
    ((scancode= scancode :scancode-down) :down)
    ((scancode= scancode :scancode-up) :up)))

(defun jumping-state-p (state)
  (or (eq state :jump)
      (and (consp state)
           (eq :jump (first state)))))

(defun compute-next-move (state key pressed)
  (cond
    ((not pressed) state)
    (t (case key
         (:up :jump)
         ((:stop :down) :idle)
         (:left :walking-left)
         (:right :walking-right)))))

;; (defmethod display :before ((a actor) &key &allow-other-keys)
;;   (set-render-draw-color *renderer* 0 0 0 255)
;;   (render-fill-rect *renderer* (sprite-dest-rect a)))

(defmethod on-key-down ((k king) &key scancode)
  (setf (king-next-move k)
        (compute-next-move (king-next-move k)
                           (scancode-move scancode)
                           :pressed)))

(defmethod on-key-up ((k king) &key scancode)
  (setf (king-next-move k)
        (compute-next-move (king-next-move k)
                           (scancode-move scancode)
                           nil)))

;;
;; Generalize for non-kings
;;

(defmethod update ((king king) &key &allow-other-keys)
  (with-accessors ((state king-state)
                   (next king-next-move)
                   (sprite sprite-description)
                   (dx king-dx)
                   (dy king-dy)
                   (speed king-speed)
                   (rect sprite-dest-rect))
      king
    ;; allow actions only when beginning the sprite animation loop
    (when (call-next-method)
      (tagbody
         (if (jumping-state-p state)
             (destructuring-bind (jump jump-state direction) state
               (case jump-state
                 (:prepare
                  (setf dx (floor dx 10))
                  (setf dy -5)
                  (spritef sprite '(jump upward))
                  (setf state `(,jump :upward ,direction)))
                 
                 (:upward
                  (setf dy 5)
                  (spritef sprite 'downward)
                  (setf state `(,jump :downward ,direction)))

                 (:downward
                  (setf dy 0)
                  (ccase direction
                    (:left
                     (setf next :walking-left)
                     (go enter-walk-left))
                    (:right
                     (setf next :walking-right)
                     (go enter-walk-right))
                    (:up
                     (setf next :idle)
                     (go enter-idle))))))
             (case state
               (:idle
                (case next
                  (:walking-left (go enter-walk-left))
                  (:walking-right (go enter-walk-right))
                  (:jump (go enter-jump-up))))

               (:walking-left
                (case next
                  (:walking-left (go enter-walk-left))
                  (:walking-right (go enter-walk-right))
                  (:jump (go enter-jump-left))
                  (:idle
                   (go enter-idle))))
               
               (:walking-right
                (case next
                  (:walking-left (go enter-walk-left))
                  (:walking-right (go enter-walk-right))
                  (:jump (go enter-jump-right))
                  (:idle (go enter-idle))))))
         (go end)

       enter-jump-up
         (setf state '(:jump :prepare :up))
         (go enter-jump)
         
       enter-jump-right
         (setf state '(:jump :prepare :right))
         (go enter-jump)
         
       enter-jump-left
         (setf state '(:jump :prepare :left))
         (go enter-jump)
         
       enter-jump
         (spritef sprite '(jump prepare))
         (go end)
         
       enter-idle
         (setf dx 0)
         (setf state :idle)
         (spritef sprite 'idle)
         (go end)
         
       enter-walk-right
         (spritef sprite '(walk forward right))
         (setf state :walking-right)
         (setf dx speed)
         (go end)

       enter-walk-left
         (spritef sprite '(walk forward left))
         (setf state :walking-left)
         (setf dx (- speed))
         (go end)

       end
         (call-next-method)
         (incf (rect-x rect) dx)
         (incf (rect-y rect) dy)))))

(defclass follower (actor)
  ((target :initarg :target :accessor follower-target)
   (right :initarg :right :accessor follower-right-spec)
   (left :initarg :left :accessor follower-left-spec))
  (:default-initargs :width (game-tile-size *game*)
                     :height (game-tile-size *game*)))

(defclass human-follower (follower) ()
  (:default-initargs :right '(walk forward right)
                     :left '(walk forward left)))

(defclass snake-follower (follower) ()
  (:default-initargs
   :description *snake-crawl-left*
   :right '(crawl right)
   :left '(crawl left)))

(defclass has-physics ()
  ((box :accessor physics-box :initarg :box)))

(defclass golden (human-follower has-physics)
  ()
  (:default-initargs
   :description *goldie-idle-left*))

(defclass helmet (human-follower)
  ()
  (:default-initargs :description *helmet-idle-left*))

(defmethod update ((follower follower) &key &allow-other-keys)
  (with-accessors ((target follower-target)
                   (sprite sprite-description)
                   (pos sprite-dest-rect)
                   (right follower-right-spec)
                   (left follower-left-spec))
      follower
    (let ((dist (- (rect-x (sprite-dest-rect target)) (rect-x pos))))
      (if (>= (abs dist) (@ 1))
          (cond ((plusp dist)
                 (incf (rect-x pos))
                 (spritef sprite right))
                (t (decf (rect-x pos))
                   (spritef sprite left)))
          (spritef sprite
                   (if (plusp dist)
                       '(idle right)
                       '(idle left)))))
    (call-next-method)))

(defclass king-follower (human-follower)
  ()
  (:default-initargs :description *king-idle-left*))

(defclass golden-follower (golden follower) ())
(defclass helmet-follower (helmet follower) ())

(defun add-follower (&optional (class 'golden-follower) (x (+ (random (@ 18)) -1)))
  (push (make-instance class
                       :target (find-king)
                       :speed (ceiling (* 1/64 (game-tile-size *game*)))
                       :x x
                       :y (@ 5))
        (game-active-object *game*)))

;; (eval-in-game
;;   (push (make-instance 'king-follower
;;                        :target (find-objects (lambda (u) (typep u 'golden)))
;;                        :x (random (@ 16))
;;                        :y (@ 5))
;;         (game-active-object *game*)))


;; (change-class (find-objects (lambda (u) (typep u 'king-follower)))
;;               'snake-follower)

;; (spritef (sprite-description (find-objects (lambda (u) (typep u 'follower))))
;;          'goldie)

;; ================================================================

(defclass game ()
  ((window :accessor game-window :initarg :window :initform *window*)
   (steps :initform 4 :accessor game-steps)
   (%step-counter :initform 0 :accessor %step-counter)
   (static :accessor game-static-object :initform nil)
   (active-object :accessor game-active-object :initform nil)
   (tile-size :accessor game-tile-size :initform 64 :initarg :tile-size)
   (width :accessor game-width :initform 16)
   (height :accessor game-height :initform 10)))

(defun @ (x &optional (game *game*))
    (round (* x (game-tile-size game))))

(defmethod (setf game-steps) :after (s (g game))
  (setf (%step-counter g) (clamp (%step-counter g) 0 s)))

(defmethod (setf game-width) :around (new (g game))
  (call-next-method (clamp new 10 100) g))

(defmethod (setf game-height) :around (new (g game))
  (call-next-method (clamp new 10 100) g))

(defmethod (setf game-tile-size) :around (new (g game))
  (call-next-method (clamp new 10 256) g))

(defun resize-game-window (&optional (g *game*))
  (set-window-size (game-window g)
                   (* (game-width g) (game-tile-size g))
                   (* (game-height g) (game-tile-size g))))

(defmethod on-key-up ((g game) &rest args)
  (apply #'on-key-up (game-active-object g) args))

(defmethod update ((g game) &key)
  (update (game-active-object g)))

(defmethod update :around ((g game) &key)
  (when (zerop (%step-counter g))
    (call-next-method))
  (setf (%step-counter g)
        (mod (1+ (%step-counter g))
             (game-steps g))))

(defmethod display ((g game) &key)
  (display (game-static-object g))
  (display (game-active-object g)))

(defun kingp (u) (typep u 'king))

(defun find-object (test)
  (find-if test (game-active-object *game*)))

(defun find-king ()
  (find-if #'kingp (game-active-object *game*)))

(defmethod on-key-down ((g game) &rest args &key scancode)
  (when (scancode= scancode :scancode-f1)
    (restart-main-loop))
  (when (scancode= scancode :scancode-f2)
    (add-follower))
  (when (scancode= scancode :scancode-f3)
    (setf (game-active-object g) (list (find-king))))
  #+mover
  (when (scancode= scancode :scancode-f4)
    (reverse-mover
     (find-if #'moverp (game-active-object (current-game)))))
  (apply #'on-key-down (game-active-object g) args))

(defmethod on-key-up ((g game) &rest args)
  (apply #'on-key-up (game-active-object g) args))

;; (define-spritesheet viking ()
;;   (:animation #'animation-reducer)
;;   (let* ((flip '(:flip :horizontal))
;;          (left `(left (right ,flip)))
;;          (right `(right (left ,flip)))
;;          (face-left `((walk () ,@left) (idle (:col 0) ,@left)))
;;          (face-right `((walk () ,@right) (idle (:col 0) ,@right))))
;;     `(_ (:order (:row :col)
;;                 :animation :cyclic
;;                 :transform ,(scale 128)
;;                 :file ,(or (probe-file #1=#P"/home/chris/all.png")
;;                            (error "No such file: ~a" #1#))
;;                 :col (:range 0 5))
        
;;         ((:each (viking (:row 0))
;;                 (sellsword (:row 1))) ()
;;          ,@face-left)

;;         ((:each (skeleton (:row 2))
;;                 (hero (:row 5))) ()
;;          ,@face-right)
        
;;         (slime () ((:each (big (:row 3))
;;                           (small (:row 4))) ()
;;                    ,@face-right))

;;         (robot (:row 6)
;;                ,@face-left
;;                (shoot (:row 7 :col (0 1 2 3))
;;                       ,@left)))))











;; (define-spritesheet test ()
;;   `(test
;;        (:order (:row :col)
;;         :animation :cyclic
;;         :transform ,(scale 32)
;;         :transform ,(move -1)
;;         :tile-callback ,#'animated-tile-callback
;;         :sprite-description-class sprite-description)
;;      (walk (:sprite-description-class animated-sprite-description)
;;            ((:each (:index (:file "one"))
;;                    (:index (:file "two")))
;;             ()
;;             (snake (:row 4 :col 3))))
;;      (other (:row 1 :col 1 :file "zero"))))
