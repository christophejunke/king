(in-package :kingdom)

(defun mapply (function args sequence &key (key #'identity) from-end stop-if)
  (let* ((accessor
           (if key
               (compile nil (lambda (u) (funcall key u)))
               #'identity))
         (action
           (if stop-if
               (lambda (u)
                 (let ((test-result
                         (multiple-value-call stop-if
                           (apply function (funcall accessor u) args))))
                   (when test-result
                     (return-from mapply test-result))))
               (lambda (u)
                 (apply function (funcall accessor u) args)))))
    (if from-end
        (etypecase sequence
          (cons (map nil action (reverse sequence)))
          (array (loop for i from (1- (length sequence)) downto 0
                       do (funcall action (aref sequence i)))))
        (map nil action sequence))))

(defun is (value &key (test #'eql) (key #'identity))
  (lambda (u)
    (funcall test value (funcall key u))))

(defclass level ()
  ((layers
    :accessor level-layers
    :initform (make-array 1 :fill-pointer 0 :adjustable t))
   (focus
    :accessor level-focus
    :initform nil)))

(defclass layer ()
  ((objects
    :accessor layer-objects
    :initform (make-array 100 :fill-pointer 0 :adjustable t))))

(defmethod display ((level level) &rest args)
  (mapply #'display args (level-layers level) :from-end t)) 

(defmethod display ((layer layer) &rest args)
  (mapply #'display args (layer-objects layer)))

(defclass grid ()
  ((height :initarg :height :accessor grid-height)
   (width :initarg :width :accessor grid-width)
   (%cells :reader %grid-cells)

   (on-overflow :initarg :on-overflow
                :initform :error
                :accessor on-grid-overflow)
   (%xcoord-fn :initform #'identity)
   (%ycoord-fn :initform #'identity)))

;; (defmethod (setf on-grid-overflow) (policy grid)
  
;;   )

(deftype cell-type () 'T)

(defmethod shared-initialize :after
    ((grid grid) slot-names &key (cell-type 'cell-type) &allow-other-keys)
  (declare (ignore slot-names))
  (with-slots (height width %cells) grid
    (setf %cells
          (make-array (list height width)
                      :element-type cell-type
                      :initial-element 0))))

(make-instance 'grid :width 32 :height 5)

(defgeneric grid (grid x y))
(defgeneric (setf grid) (v grid x y))

