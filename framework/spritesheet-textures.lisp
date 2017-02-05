(in-package #:game.framework.spritesheet-textures)

(defparameter *active-textures* nil)

(defstruct textures vector spritesheet)

(defun make-textures-from-spritesheet (spritesheet)
  (make-textures :spritesheet spritesheet
                 :vector (map 'vector
                              (lambda (f)
                                (sdl2:create-texture-from-surface
                                 *renderer*
                                 (load-bmp (probe-file f))))
                              (spritesheet-texture-files spritesheet))))

(defun destroy-textures (textures)
  (ignore-errors
   (map nil #'destroy-texture (textures-vector textures)))
  (setf (textures-vector textures) nil))

(define-condition inactive-spritesheet (error)
  ((sprite-description :initarg :sprite-description
                       :accessor invalid-textures-sprite-description))
  (:report
   "Cannot instanciate sprite because its spritesheet is inactive."))

(define-condition invalid-textures (error)
  ((instance :initarg :instance :accessor invalid-textures-instance))
  (:report
   (lambda (err stream &aux (textures (invalid-textures-instance err)))
     (if textures
         (format stream
                 "Invalid TEXTURES instance~
                  ~:[ (the spritesheet is inactive)~
                   ~; even though the spritesheet is active (unexpected!)~]."
                 (getf *active-textures* (textures-spritesheet textures)))
         (format stream "Invalid TEXTURES (supplied instance is NIL)")))))

(defun check-textures-active (textures)
  (unless (textures-vector textures)
    (error 'invalid-textures :instance textures)))

(defmacro with-active-spritesheet (spritesheet &body body)
  (once-only (spritesheet)
    (with-gensyms (textures)
      `(let ((,textures (make-textures-from-spritesheet ,spritesheet)))
         (unwind-protect
              (let ((*active-textures* (list* (spritesheet-name ,spritesheet)
                                              ,textures
                                              *active-textures*)))
                ,@body)
           (ignore-errors (destroy-textures ,textures)))))))

(defmacro with-active-spritesheets ((&rest spritesheets) &body body)
  (if spritesheets
      `(with-active-spritesheet ,(first spritesheets)
         (with-active-spritesheets ,(rest spritesheets)
           ,@body))
      `(progn ,@body)))

;; ================================================================
;; ANIMATIONS
;;

(defun parse-animation (val)
  (if (animation-reversed-p val)
      (values (second val) t)
      (values val nil)))

(defun animation-reversed-p (val)
  (and (consp val) (eq (first val) :reverse)))

(defun animation-type (val)
  (if (animation-reversed-p val)
      (second val)
      val))

(defun animation-reducer (old val)
  "A reducer for :ANIMATION in spritesheet tree attributes."
  (case val
    (:reverse
     (if (animation-reversed-p old)
         (second old)
         `(:reverse ,old)))))

(defgeneric make-animator (animation)
  (:method (x) x))

;; ================================================================
;; SPRITE
;; 

(defclass sprite ()
  ((%textures :initarg :%textures-instance)
   (description :initarg :description :accessor sprite-description)
   (animator :initarg :animator :accessor sprite-animator)
   (dest-rect :initarg :dest-rect :initform nil :accessor sprite-dest-rect)
   (texture :initarg :texture :reader sprite-texture)
   (rectangles :initarg :rectangles :reader sprite-rectangles)
   (cursor :initarg :cursor :accessor sprite-cursor :initform 0)))

(defgeneric sprite-step (sprite))
(defgeneric sprite-animation-step (sprite animation))

(defmethod sprite-step ((s sprite))
  (sprite-animation-step s (sprite-animator s))
  (zerop (sprite-cursor s)))

(defmethod sprite-animation-step ((s sprite) animator)
  (setf (sprite-cursor s)
        (mod (1+ (sprite-cursor s)) (length (sprite-rectangles s)))))



(defmethod sprite-textures :before ((s sprite))
  (check-textures-active (slot-value s '%textures)))

(defun %update-sprite-for-description (sprite sd)
  (let ((textures (getf *active-textures*
                        (spritesheet-name
                         (sprite-description-spritesheet sd)))))
    (unless textures
      (error 'inactive-spritesheet :sprite-description sd))
    (check-textures-active textures)
    (multiple-value-bind (animation-type reversedp)
        (parse-animation (getf (sprite-description-env sd) :animation))

      (with-slots (texture rectangles cursor animator) sprite
        (setf animator (make-animator animation-type))
        (setf texture (svref (textures-vector textures)
                             (sprite-description-texture-id sd)))
        (setf rectangles (if reversedp
                             (reverse (sprite-description-rectangles sd))
                             (sprite-description-rectangles sd)))
        (setf cursor 0)))))

(defmethod shared-initialize :after ((s sprite) slot-names &rest rest)
  (declare (ignore slot-names rest))
  (%update-sprite-for-description s (sprite-description s)))

(defmethod (setf sprite-description) :around (sd (s sprite))
  (unless (eq sd (sprite-description s))
    (call-next-method)
    (%update-sprite-for-description s sd)))

(defgeneric render-sprite (sprite &key renderer))

(defclass broken-sprite (sprite) ())
(defmethod render-sprite ((s broken-sprite) &key (renderer *renderer*))
  (when (sprite-dest-rect s)
    (set-render-draw-color renderer 255 0 0 255)
    (render-draw-rect renderer (sprite-dest-rect s))))

(defmethod render-sprite :around ((s sprite) &rest args)
  (handler-case (call-next-method)
    (invalid-textures (e)
      (warn "Sprite ~S is broken: ~A" s e)
      (change-class s 'broken-sprite)
      (apply 'render-sprite s args))))

(defmethod render-sprite ((s sprite)
                          &key
                            (renderer *renderer*)
                            (index (sprite-cursor s)))
  (render-copy-ex
   renderer
   (sprite-texture s)
   :source-rect (svref (sprite-rectangles s) index)
   :dest-rect (sprite-dest-rect s)
   :flip (sprite-description-flip (sprite-description s))))

(defun sprite-reset (s)
  (setf (sprite-animator-cursor s) 0))

(defun sprite-set (s sd &optional (cursor 0))
  (setf (sprite-description s) sd)
  (setf (sprite-cursor s) cursor))

(defmethod update ((s sprite) &key &allow-other-keys)
  (sprite-step s))

(defmethod display ((s sprite) &key &allow-other-keys)
  (render-sprite s))

