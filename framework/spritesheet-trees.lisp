(in-package #:game.framework.spritesheet-trees)

;;;; TILESET COORDINATES - only scale and translate, no rotation.

(defun combine (&rest matrices)
  (apply #'sb-cga:matrix* matrices))

(defun scale (x y)
  (sb-cga:scale* (float x) (float y) 0.0))

(defun translate (x y)
  (sb-cga:translate* (float x) (float y) 0.0))

;;;; DATATYPES

(defstruct spritesheet
  name          ; symbol
  tree          ; The tree from which we compile the spritesheet
  sprites       ; A vector of SPRITE-DESCRIPTION instances, ordered
                ; according to their ID slot.
  texture-files ; A vector of all unique tileset pathnames declared in
                ; the tree through the :FILE property. Later, we bind
                ; SDL2 textures to each file in the same order (see
                ; TEXTURE-ID below)
  )

(defstruct sprite-description
  spritesheet  ; Parent spritesheet (circular reference)
  id           ; Corresponds to the position of this sprite in the
               ; spritesheet SPRITES vector
  name         ; symbol (name of the sprite); for example, if ROOT,
               ; NODE and LEAF are nodes in the original tree, the
               ; name of the sprite represented by the leaf is
               ; ROOT-NODE-LEAF (nil nodes are ignored for names).
  rectangles   ; Vector of sdl2:rect instances representing, for each
               ; image of an animated sprite, the area to blit from in
               ; the associated texture.
  flip         ; Value associated with the :FLIP property in the tree,
               ; can be either :NONE, :HORIZONTAL or :VERTICAL or a
               ; list like (:HORIZONTAL :VERTICAL).
  env          ; Value of the environtment for that sprite. This is a
               ; way to pass additional information to animation
               ; objects.
  texture-id   ; Index corresponding to the SDL2 texture from which
               ; rectangles should be rendered from. You need to load
               ; textures first and put them in a sequence in the same
               ; order as a corresponding spritesheet's TEXTURE-FILES.
  )

(defmethod print-object ((s sprite-description) out)
  "Ensure SPRITE-DESCRIPTION are printed with circle detection enabled."
  (let ((*print-circle* t))
    (call-next-method)))

(defmethod cleanup ((s sprite-description))
  (map nil #'sdl2:free-rect (sprite-description-rectangles s)))

;;==============================================================================

(defun combine-environments (old-env new-env reducers)
  "Combine property lists w.r.t. reducers.

When no reducer is defined for an attribute, the new value shadows the
previous one. For example:

   (combine-environments '(:a 1 :b 2 :c 3)
                         '(:a 0 :b 20 :c 4)
                         (list :a #'list :b #'+))
   => (:A (1 0) :B 22 :C 4)

An attribute can appear multiple times in the environment: if so, its
consecutive values will be combined in the order of appearance.

   (combine-environments `()
                          `(transform ,(scale 3 1)
                            transform ,(scale 1 2))
                          (list 'transform #'transform-handler))
   => (TRANSFORM
       #<[3.0, 0.0, 0.0, 0.0]
         [0.0, 2.0, 0.0, 0.0]
         [0.0, 0.0, 0.0, 0.0]
         [0.0, 0.0, 0.0, 1.0]>)
"
  (loop with env = (copy-seq old-env)
        for (attribute value) on new-env by #'cddr
        for old-value = (getf env attribute)
        do (setf (getf env attribute)
                 (if old-value
                     (funcall (getf reducers attribute #'shadow-handler)
                              old-value
                              value)
                     value))
        finally (return env)))

(defun map-sprite-tree-leaves (sprites callback &key env reducers)
  "Apply CALLBACK to each leaves of the SPRITES tree.

CALLBACK is a function accepting:

  - PATH, a list of symbols of each node from current leaf to root.
  - ENV, a property list representing bindings available in the scope
    of the leaf node.

The return value of CALLBACK is not used.

ENV is an optional environment that can be used to provide additional
bindings inside CALLBACK. REDUCERS is a plist mapping attributes to
reducing function, which are called to combine old and new attributes
when the environment is extended. See COMBINE-ENVIRONMENTS."
  (labels
      ((recurse (tree path env)
         (etypecase tree
           (null nil)
           (cons
            ;; A tree is either name (possibly NIL or compound),
            ;; additional bindings (either absent, or NIL, or a proper
            ;; list of bindings), as well as zero or more subtrees.
            (destructuring-bind (head &optional bindings &rest children) tree
              (let ((env (combine-environments env bindings reducers)))
                (etypecase head
                  (cons
                   (ecase (first head)
                     ;; Iterate over alternative branches, and
                     ;; for each of them, establish the bindings
                     ;; associated with it.
                     (:each ; (:each (node1 bindings1)
                            ;        (node2 bindings2)
                            ;        ...)
                      (loop for (name bindings) in (rest head)
                            do (recurse `(,name ,bindings ,@children)
                                        path
                                        env)))))
                  (symbol
                   (if head
                       ;; Only add HEAD in front of PATH if it is not
                       ;; NIL.  NIL nodes are useful to introduce
                       ;; properties without cluttering the tree with
                       ;; too many irrelevant.
                       (push head path)
                       (unless children
                         ;; I don't see a good reason to use leaf nodes
                         (warn "NIL leaf node: ~S" path)))
                   (if children
                       (dolist (child children)
                         (recurse child path env))
                       (funcall callback path env)))))))
           (atom (recurse (list tree) path env)))))
    (recurse sprites nil env)))

(defun map-tile-indices-in-order (order env callback)
  "Iterate over the indices of ROWS and COLUMNS in a tileset.

Iteration is done in the order specified by ORDER (e.g. '(:ROW :COL)
for an outer loop over rows and an inner loop for columns), and with
the bounds found inside ENV for :ROW and :COL.

Such bounds can be simple values, or sequences of values, in which
case the CALLBACK function is called for each combination of row and
column.

The CALLBACK function is given 3 key arguments, ROW, COL and INDEX,
but does not need to use them all because it is called
with :ALLOW-OTHER-KEYS T.

ROW and COL are the current row and columns values.  INDEX is a
counter which strictly increases after each call to the callback, and
starts from zero. It represents the current index of a tile in the
sequence of tiles being visited."
  (let ((index -1))
    (labels
        ((recurse (list indices)
           (etypecase list
             (null
              (apply callback
                     :index (incf index)
                     :allow-other-keys t
                     (mapcan #'list order (reverse indices))))
             (cons
              (destructuring-bind (head . tail) list
                (let ((bound (getf env head)))
                  (assert bound (bound))
                  (loop for v in (ensure-list bound)
                        do (check-type v integer "ITS ME")
                           (recurse tail (cons v indices)))))))))
      (recurse order ()))))

(defun pixel-rectangle (x y w h matrix)
  "Apply MATRIX transform to (X Y W H) rectangle, return an SDL2:RECT."
  (let
      ((point (transform-point (vec (float x) (float y) 0.0) matrix))
       (unit-x (transform-direction (vec (float w) 0.0 0.0) matrix))
       (unit-y (transform-direction (vec 0.0 (float h) 0.0) matrix)))
    ;; We don't support rotations, unit-x's y and unit-y's x should be null.
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
     
     (round (+ (aref point 0) (min 0 (aref unit-x 0))))
     (round (+ (aref point 1) (min 0 (aref unit-y 1))))
     (round (abs (aref unit-x 0)))
     (round (abs (aref unit-y 1))))))

(defun make-indexer (&key (key #'identity) (test #'equal))
  "Create a numerical index for a set of values, without duplicates.

Based on the KEY and TEST auxiliary functions (where TEST is a
suitable test for hash-tables), the function builds a closure that
accepts an optional argument:

Each time an argument is given that was not yet already given to this
closure, w.r.t. to KEY and TEST, we increment a counter and affect it
to the value. If however the value was already present, we return the
existing index.

Finally, when no argument is given, the closure returns a vector of
all the given already seen by the closure, stored in the same order as
the index which was associated with the value. Note that those values
are the one being that were passed to the function, not the
keys (which are used only for hashing)."
  (let ((map (make-hash-table :test test))
        (counter -1))
    (lambda (&optional (data nil datap))
      (if datap
          ;; Store data
          (let ((key (funcall key data)))
             (cdr
              (or (gethash key map)
                  (setf (gethash key map)
                        (cons data (incf counter))))))
          ;; Return index
          (let ((array (make-array (hash-table-count map) :adjustable nil)))
            (maphash (lambda (k v)
                       (declare (ignore  k))
                       (setf (aref array (cdr v)) (car v))) map)
            array)))))

;; (defun test-indexer ()
;;   (let ((indexer (make-indexer :key #'namestring)))
;;     (list (funcall indexer #P"/tmp/foo.lisp")
;;           (funcall indexer #P"/tmp/bar.lisp")
;;           (funcall indexer))))

(defparameter *spritesheet-cleanup* nil
  "Set to T if spritesheet recompilation should unintern old symbols silently.")

(defun shadow-handler (old new) (declare (ignore old)) new)
(defun transform-handler (old new) (combine old new))

(defmacro do-tree-leaves ((path env tree &key result reducers) &body body)
  `(block nil
     (map-sprite-tree-leaves ,tree (lambda (,path ,env) ,@body) :reducers ,reducers)
     (return ,result)))

(defun compile-spritesheet-tree
    (&key
       spritesheet-name
       attribute-reducers
       tree
       symbol-format
     &aux (replacep (and (boundp spritesheet-name)
                         (spritesheet-p (symbol-value spritesheet-name)))))
  (let ((collection nil)
        (file-indexer (make-indexer :key #'namestring))
        (counter -1)
        (names (make-hash-table :test #'eq))
        (spritesheet (if replacep
                         (symbol-value spritesheet-name)
                         (make-spritesheet :name spritesheet-name))))
    (do-tree-leaves (path env tree :reducers attribute-reducers)
      (let* ((sequence)
             (name (with-standard-io-syntax
                     (format nil symbol-format (reverse path))))
             (matrix (getf env 'transform (identity-matrix))))

        ;; Intern and export sprite name
        (setf name (intern name *package*))
        (export name *package*)

        ;; Checking for duplicates inside a tree
        (let ((path (gethash name names)))
          (if path
              (error "Duplicate name in tree: ~A in ~S" name (cdr path))
              (setf (gethash name names) (cons t path))))

        ;; For each leaf, we iterate over its tiles and
        ;; compute their region in pixel coordinates
        ;; (relative to the whole tileset).

        (map-tile-indices-in-order (getf env :order)
                                   env
                                   (lambda (&key col row)
                                     (push (pixel-rectangle col
                                                            row
                                                            (getf env :width 1)
                                                            (getf env :height 1)
                                                            matrix)
                                           sequence)))
        (push (make-sprite-description :name name
                                       :id (incf counter)
                                       :spritesheet spritesheet
                                       :env env
                                       :texture-id (funcall file-indexer
                                                            (namestring
                                                             (getf env :file)))
                                       :rectangles (coerce
                                                    (nreverse sequence)
                                                    'vector)
                                       :flip (ensure-list (getf env :flip :none)))
              collection)))

    ;; We collected all sprite descriptions, perform if necessary some cleanup
    ;; with the previous definition of this spritesheet, and return the
    ;; spritesheet object

    (let ((sprites (coerce (nreverse collection) 'vector))
          (texture-files (funcall file-indexer)))

      ;; Everything went fine until here, we can "commit" changes.
      ;; Errors here should be unlikely below (if an error happen, we
      ;; might be in an inconsistent state).

      (when replacep
        (let ((removal))
          (map nil
               (lambda (s)
                 (let ((name (sprite-description-name s)))
                   (unless (gethash name names)
                     (push name removal))))
               (spritesheet-sprites spritesheet))
          (when removal
            (tagbody
               (if *spritesheet-cleanup*
                   (go remove)
                   (restart-case 
                       (error "Those symbols were previously defined ~
                         by the spritesheet: ~& ~S ~& ~% See also ~A."
                              removal
                              '*spritesheet-cleanup*)
                     (ignore ()
                       :report "Leave them."
                       (go end))
                     (unintern ()
                       :report "Unintern them."
                       (go remove))))
             remove
               (mapc #'unintern removal)
             end))))

      ;; Globally bind symbols to their new sprite descriptions.
      ;; Fill and return the spritesheet.

      (map nil #'cleanup (spritesheet-sprites spritesheet))
      
      (map nil
           (lambda (s)
             (proclaim `(special ,(sprite-description-name s)))
             (setf (symbol-value (sprite-description-name s)) s))
           sprites)
      
      (setf (spritesheet-tree spritesheet) tree
            (spritesheet-sprites spritesheet) sprites
            (spritesheet-texture-files spritesheet) texture-files)
      spritesheet)))

(defmacro define-spritesheet
    (name (&key
             (package '*package*)
             (format "*~{~A~^-~}*"))
     &body options-and-tree)
  (let* ((options (butlast options-and-tree))
         (tree (last options-and-tree)))
    `(defparameter ,name 
       (let ((*package* (find-package ,package)))
         (compile-spritesheet-tree
          :spritesheet-name ',name
          :attribute-reducers
          (list ,@(loop for (attribute function) in options
                        collect `(quote ,attribute)
                        collect function)
                'transform #'transform-handler)
          :tree (list nil nil ,@tree)
          :symbol-format ,format)))))
