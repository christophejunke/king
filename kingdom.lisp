(in-package :kingdom)

(define-spritesheet *forest* ()
  (:animation #'animation-reducer)
  `(nil (:file ,(sprite-path "forest" "characters")
         :order (:row :col)
         :animation :cyclic
         transform ,(scale 32 32)
         transform ,(translate -1 -1))
        (snake (:row 4)
               (crawl (:col (1 2 3 4))
                      (right)
                      (left (:flip :horizontal))))
        ((:each (goldie (:row 1))
                (king   (:row 2))
                (helmet (:row 3)))
         ()
         (nil (:animation :sequence)
              (slash (:col (12 11 12 13)))
              (punch (:col (14 12)))
              (hit (:col (9 10 9))))
         (jump ()
               (prepare (:col 5))
               (upward (:col 6))
               (downward (:col 7))
               (landing (:col 8)))      
         (climb (:col (19 20 21 22)))

         (idle (:col 15)
               (right)
               (left (:flip :horizontal)))
         
         ((:each (run (:col (15 16 17 18)))
                 (walk (:col (1 2 3 4))))
          ()
          (forward ()
                   (right)
                   (left (:flip :horizontal)))
          (backward (:animation :reverse)
                    ;; here, we flip differently: right means going to
                    ;; the right while facing left.
                    (right (:flip :horizontal))
                    (left))))))

;; (defparameter *pimples*
;;   (compile-spritesheet-tree
;;    `(PIMPLES (:file ,(resources:sprite "pimples")
;;               :transform ,(scale 32 32))
;;              (CHARACTERS (:transform ,(translate 16 1))))))

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

(defclass king (actor)
  ((state :initform :idle :accessor king-state)
   (speed :initform 12 :accessor king-speed)
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

(defun compute-next-move (state key pressed)
  (if pressed
      (case key
        ((:stop :up :down) :idle)
        (:left :walking-left)
        (:right :walking-right))
      state))

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
         (case state
           (:idle
            (case next
              (:walking-left (go enter-walk-left))
              (:walking-right (go enter-walk-right))))

           (:walking-left
            (case next
              (:walking-left (go enter-walk-left))
              (:walking-right (go enter-walk-right))
              (:idle
               (setf sprite *king-idle-left*)
               (go enter-idle))))
           
           (:walking-right
            (case next
              (:walking-left (go enter-walk-left))
              (:walking-right (go enter-walk-right))
              (:idle
               (setf sprite *king-idle-right*)
               (go enter-idle)))))

         (go end)
         
       enter-idle
         (setf dx 0)
         (setf state :idle)     
         (go end)
         
       enter-walk-right
         (setf sprite *king-walk-forward-right*)
         (setf state :walking-right)
         (setf dx speed)
         (go end)

       enter-walk-left
         (setf sprite *king-walk-forward-left*)
         (setf state :walking-left)
         (setf dx (- speed))
         (go end)

       end
         (call-next-method)
         (incf (rect-x rect) dx)
         (incf (rect-y rect) dy)))))








(defclass golden-thing (actor)
  ((target :initarg :target :accessor golden-thing-target)
   (proximity :initform (@ 1) :accessor golden-thing-proximity))
  (:default-initargs :description *goldie-idle-left*  
                     :width (game-tile-size *game*)
                     :height (game-tile-size *game*)))

(defmethod update ((golden-thing golden-thing) &key &allow-other-keys)
  (with-accessors ((king golden-thing-target)
                   (sprite sprite-description)
                   (pos sprite-dest-rect)
                   (margin golden-thing-proximity))
      golden-thing
    (let ((dist (- (rect-x (sprite-dest-rect king)) (rect-x pos))))
      (if (>= (abs dist) margin)
          (cond ((plusp dist)
                 (incf (rect-x pos))
                 (setf sprite *goldie-walk-forward-right*))
                (t (decf (rect-x pos))
                   (setf sprite *goldie-walk-forward-left*)))
          (setf sprite
                (if (plusp dist)
                    *goldie-idle-right*
                    *goldie-idle-left*))))
    (call-next-method)))

(defun add-golden-thing (&optional (x (+ (random (@ 18)) -1)))
  (push (make-instance 'golden-thing
                       :target (find-king)
                       :x x
                       :y (@ 5))
        (game-active-object *game*)))

;; ================================================================

(defclass game ()
  ((window :accessor game-window :initarg :window :initform *window*)
   (steps :initform 4 :accessor game-steps)
   (%step-counter :initform 0 :accessor %step-counter)
   (static :accessor game-static-object :initform nil)
   (active-object :accessor game-active-object :initform nil)
   (tile-size :accessor game-tile-size :initform 64)
   (width :accessor game-width :initform 16)
   (height :accessor game-height :initform 10)))

(defun @ (x &optional (game *game*))
  (* x (game-tile-size game)))

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

(defun find-king ()
  (find-if #'kingp (game-active-object *game*)))

(defmethod on-key-down ((g game) &rest args &key scancode)
  (when (scancode= scancode :scancode-f1)
    (restart-main-loop))
  (when (scancode= scancode :scancode-f2)
    (add-golden-thing))
  (when (scancode= scancode :scancode-f3)
    (setf (game-active-object g) (list (find-king))))
  (apply #'on-key-down (game-active-object g) args))

(defmethod on-key-up ((g game) &rest args)
  (apply #'on-key-up (game-active-object g) args))
