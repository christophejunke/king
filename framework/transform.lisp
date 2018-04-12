(in-package #:game.framework.transform)

;;;; TILESET COORDINATES - only scale and translate, no rotation.

(defstruct (transform (:constructor make-transform (sx sy tx ty)))
  sx
  sy
  tx
  ty)

(defun identity-transform ()
  (make-transform 1 1 0 0))

(defmacro with-transform ((&optional sx sy tx ty) transform &body body)
  (let ((sym (gensym)))
    (flet ((maybe (symbol fn)
             (unless (or (not symbol)
                         (string= symbol "_"))
               (list (list symbol `(,fn ,sym))))))
      `(let* ((,sym ,transform)
              ,@(mapcan #'maybe
                        (list sx sy tx ty)
                        '(transform-sx
                          transform-sy
                          transform-tx
                          transform-ty)))
         (declare (ignorable ,sym))
         ,@body))))

(defmacro with-transforms (transforms &body body)
  (etypecase transforms
    (null `(progn ,@body))
    (cons `(with-transform ,@(car transforms)
               (with-transforms ,(cdr transforms) ,@body)))))

(defun combine (t1 t2)
  (with-transforms (((sx1 sy1 tx1 ty1) t1)
                    ((sx2 sy2 tx2 ty2) t2))
    (make-transform (* sx1 sx2)
                    (* sy1 sy2)
                    (+ tx1 (* sx1 tx2))
                    (+ ty1 (* sy1 ty2)))))

(defun scale (sx &optional (sy sx))
  (make-transform sx sy 0 0))

(defun move (tx &optional (ty tx))
  (make-transform 1 1 tx ty))

(defun transform (x y transform)
  (with-transform (sx sy tx ty) transform
    (values (+ tx (* sx x))
            (+ ty (* sy y)))))

(defun pixel-rectangle (x y w h transform)
  (multiple-value-bind (px py) (transform x y transform)
    (with-transform (sx sy) transform
      (let ((unit-x (* w sx))
            (unit-y (* h sy)))
        (sdl2:make-rect

         ;;                                   -W
         ;;          ^                      @----->   
         ;;          | H                    |         
         ;;          |         ====>     -H |         
         ;;    <-----O                      v         
         ;;       W
         ;;
         ;; Make sure width and height are always positive, by shifting if
         ;; required the x and y coordinates and taking the absolute values
         ;; of each dimension (e.g. (0, 0 -10, -10) becomes (-10, -10, 10, 10).
         
         (round (+ px (min 0 unit-x)))
         (round (+ py (min 0 unit-y)))
         (round (abs unit-x))
         (round (abs unit-y)))))))

