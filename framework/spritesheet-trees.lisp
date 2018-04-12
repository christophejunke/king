(in-package #:game.framework.spritesheet-trees)

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

;; why
(defclass mixin () ())

(defmethod print-object :around ((m mixin) stream)
  (print-unreadable-object (m stream :type t)
    (call-next-method)))

(defmethod print-object ((m mixin) stream))

;;;; NAME
(defclass name-mixin (mixin)
  ((name :accessor name-of :initarg :name :initform (gensym))))
(defmethod name-of ((s symbol)) (symbol-name s))
(defmethod print-object :before ((o name-mixin) stream)
  (format stream "[NAME ~A]" (name-of o)))

;;;; PARENT
(defclass parent-mixin (mixin)
  ((parent :accessor parent-of :initarg :parent :initform nil)))
(defmethod parent-of ((n null)) nil)
(defmethod print-object :before ((o parent-mixin) stream)
  (format stream "[PARENT ~A]" (name-of (parent-of o))))

;;;; CHILDREN
(defclass children-mixin (mixin)
  ((children :accessor children-of :initarg :children :initform nil)))
(defmethod children-of ((n null)) nil)
(defmethod print-object :before ((o children-mixin) stream)
  (format stream
          "[CHILDREN ~{~A~^, ~}]"
          (map 'list #'name-of (children-of o))))

;;;; SPRITE NODES (internal)
(defclass sprite-node (name-mixin parent-mixin children-mixin)
  ())

;;; SPRITE DESCRIPTIONS (leaves)
(defclass sprite-description (name-mixin parent-mixin)
  ((name :accessor sprite-description-name) ;; alias for name-of
   (spritesheet :initarg :spritesheet
                :accessor sprite-description-spritesheet)
   (id :initarg :id
       :accessor sprite-description-id )
   (rectangles :initarg :rectangles
               :accessor sprite-description-rectangles)
   (flip :initarg :flip
         :accessor sprite-description-flip) ;; TODO: into env?
   (reverse-path :initarg :reverse-path
                 :accessor sprite-description-reverse-path)
   (env :initarg :env
        :accessor sprite-description-env)
   (texture-id :initarg :texture-id
               :accessor sprite-description-texture-id)))

;; uniform interface
(defmethod children-of ((s sprite-description)) nil)

(defmethod cleanup ((s sprite-description))
  (map nil #'sdl2:free-rect (sprite-description-rectangles s)))

;;==============================================================================

(defun find-node-child (parent name)
  (find name
        (children-of parent)
        :key #'name-of))

(defun descend-node (root path)
  (if (endp path) root
      (destructuring-bind (head . tail) path
        (descend-node (or (find-node-child root head) root) tail))))

(defun descend-leaf (root path)
  (let ((down (descend-node root path)))
    (etypecase down
      (sprite-description down)
      (sprite-node
       (let ((first-child (aref (children-of down) 0)))
         (etypecase first-child
           (sprite-description first-child)
           (sprite-node ;; (warn
                        ;;  "Invalid descent from ~A with path:~&  ~A"
                        ;;  root
                        ;;  path)
            nil)))))))

(defun switch-to-sprite (root branch)
  (destructuring-bind (branch . downpath) (ensure-list branch)
    (labels ((up (node stack)
               (if node
                   (let ((child (find-node-child node branch)))
                     (if child
                         (or (descend-leaf child
                                           (remove nil (cons (first stack)
                                                             (append downpath
                                                                     (rest stack)))))
                             root)
                         (up (parent-of node)
                             (cons (name-of node) stack))))
                   (error "Branch ~S not found from ~S."
                          branch
                          root))))
      (up (parent-of root) ()))))

(defmacro spritef (place branch)
  `(setf ,place (switch-to-sprite ,place ,branch)))

(defun make-leaf-collector ()
  (let ((root (make-instance 'sprite-node)))
    (labels
        ((add-child (node child)
           (vector-push-extend child (ensure-children node))
           child)
         (ensure-children (node)
           (or (children-of node)
               (setf (children-of node)
                     (make-array 2 :adjustable t :fill-pointer 0))))
         (make-node (name parent)
           (make-instance
            'sprite-node
            :name name
            :parent parent))
         
         (collect-nodes (path parent leaf)
           (etypecase path
             (null
              (assert (not (find-node-child parent (name-of leaf))))
              (add-child parent leaf)
              (setf (parent-of leaf) parent))
             (cons
              (destructuring-bind (head . tail) path
                (collect-nodes
                 tail
                 (or (find-node-child parent head)
                     (add-child parent (make-node head parent)))
                 leaf))))))
      (lambda (&optional (leaf nil leafp))
        (when leafp
          (collect-nodes (reverse (sprite-description-reverse-path leaf))
                         root
                         leaf))
        root))))

;; ================================================================

(defgeneric attribute-reducer (attribute old-value new-value)
  (:documentation "Generic function for attribute combinators

Define global attribute reducers (most likely with EQL methods). You
can override them by providing a different reducer function in
DEFINE-SPRITESHEET.")
  (:method (_ old new)
    "Shadow the previous value with the new one."
    (declare (ignore old)) new)
  (:method ((_ (eql :transform)) old new)
    (game.framework.transform:combine old new)))

(defun combine-environments (old-env new-env &optional reducers)
  "Combine property lists w.r.t. reducers.

REDUCERS is a plist of property names to functions.  A reducer
function takes the old value, the new value and returns the combined
value. You can install reducer functions globally by defining a method
for ATTRIBUTE-REDUCER (the REDUCERS list however takes precedence over
them). When no reducer is defined for an attribute, the new value
shadows the previous one. For example:

   (combine-environments '(:a 1 :b 2 :c 3)
                         '(:a 0 :b 20 :c 4)
                         (list :a #'list :b #'+))
   => (:A (1 0) :B 22 :C 4)

An attribute can appear multiple times in the environment: if so, its
consecutive values will be combined in the order of appearance.

   (combine-environments `()
                         `(:transform ,(scale 3 3)
                           :transform ,(move 1 1)))

   => (:TRANSFORM 
        #S(GAME.FRAMEWORK.TRANSFORM::TRANSFORM :SX 3 :SY 3 :TX 3 :TY 3))
"
  (loop with env = (copy-seq old-env)
        for (attribute value) on new-env by #'cddr
        for old-value = (getf env attribute)
        do (setf (getf env attribute)
                 (if old-value
                     (let ((reducer (getf reducers attribute)))
                       (if reducer
                           (funcall reducer old-value value)
                           (attribute-reducer attribute old-value value)))
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
           (cons
            ;; A tree is either name (possibly NIL or compound),
            ;; additional bindings (either absent, or NIL, or a proper
            ;; list of bindings), as well as zero or more subtrees.
            (destructuring-bind (head &optional bindings &rest children) tree
              (let ((env (combine-environments env bindings reducers)))
                (typecase head
                  (cons
                   (ecase (first head)
                     ;; Iterate over alternative branches, and
                     ;; for each of them, establish the bindings
                     ;; associated with it.
                     (:each
                      ;; (:each (node1 bindings1)
                      ;;        (node2 bindings2)
                      ;;        ...)
                      (loop for (name bindings) in (rest head)
                            for index from 0
                            do (recurse `(,(case name
                                             (:index index)
                                             (t name))
                                          ,bindings ,@children)
                                        path
                                        env)))))
                  (atom
                   (when (and head (not (and (symbolp head)
                                             (string= :_ head))))
                     ;; Only add HEAD in front of PATH if it is not
                     ;; NIL, or if the NIL node is a leaf.  Internal
                     ;; NIL nodes are useful to introduce properties
                     ;; without cluttering the tree with too many
                     ;; irrelevant. Leaf NIL nodes are useful to
                     ;; provide a dummy sprite (an entry point in
                     ;; the tree).
                     (push head path)
                     (when (symbolp head)
                       ;; Intermediate nodes can be integers, etc.
                       (export head (symbol-package head))))
                   (if children
                       (dolist (child children)
                         (recurse child path env))
                       (funcall callback path env)))))))
           (atom (recurse (list tree) path env)))))
    (recurse sprites nil env)))

(define-condition no-such-bound-in-env (error)
  ((bound :initarg :bound :reader no-such-bound-in-env/bound)
   (env :initarg :env :reader no-such-bound-in-env/env)))

(defun map-tile-indices-in-order (order env callback &aux (index -1))
  "Iterate over the indices of ROWS and COLUMNS in a tileset.

Iteration is done in the order specified by ORDER (e.g. '(:ROW :COL)
for an outer loop over rows and an inner loop for columns), and with
the bounds found inside ENV for :ROW and :COL.

Such bounds can be simple values, a sequences of values, or a
cons-cell (X . Y) which represents the sequence from X to Y (X can be
greater than Y; both bounds are inclusives; if they are equal, the
value is considered only once). CALLBACK function is called for each
combination of row and column.

The CALLBACK function is given 3 key arguments, ROW, COL and INDEX,
but does not need to use them all because it is called
with :ALLOW-OTHER-KEYS T.

ROW and COL are the current row and columns values.  INDEX is a
counter which strictly increases after each call to the callback, and
starts from zero. It represents the current index of a tile in the
sequence of tiles being visited."
  (declare (optimize (debug 3)))
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
                (unless bound
                  (error 'no-such-bound-in-env :bound bound :env env))

                (labels
                    ((process-bound (bound)
                       (typecase bound
                         (atom (recurse tail (cons bound indices)))
                         (cons
                          (destructuring-bind (kind . args) bound
                            (etypecase kind
                              (integer (map nil #'process-bound bound))
                              (symbol
                               (case kind
                                 (:range
                                  (destructuring-bind (from to) args
                                    (if (= to from)
                                        (process-bound to)
                                        (let ((delta (signum (- to from))))
                                          (loop
                                            (process-bound from)
                                            (when (= from to)
                                              (return))
                                            (incf from delta))))))
                                 (t (recurse tail (cons bound indices)))))))))))
                  (process-bound bound))))))))
    (recurse order ())))

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

(defvar *spritesheet-cleanup* t
  "Set to T if spritesheet recompilation should unintern old symbols silently.")

(defmacro do-tree-leaves ((path env tree &key result reducers) &body body)
  `(block nil
     (map-sprite-tree-leaves ,tree (lambda (,path ,env) ,@body) :reducers ,reducers)
     (return ,result)))

(defun sdl-rect-from-env (&key tile-indices env transform)
  (pixel-rectangle (getf tile-indices :col)
                   (getf tile-indices :row)
                   (getf env :width 1)
                   (getf env :height 1)
                   transform))

(defun compile-spritesheet-tree
    (&key
       spritesheet-name
       attribute-reducers
       tree
       symbol-format       
     &aux (replacep (get spritesheet-name 'spritesheet)))
  (let ((collection nil)
        (file-indexer (make-indexer :key #'namestring))
        (counter -1)
        (names (make-hash-table :test #'eq))
        (tree-builder (make-leaf-collector))
        (spritesheet (or (get spritesheet-name 'spritesheet)
                         (make-spritesheet :name spritesheet-name))))
    (do-tree-leaves (path env tree :reducers attribute-reducers)
      (let* ((sequence)
             (name (with-standard-io-syntax
                     (format nil symbol-format (reverse path))))
             (transform (getf env :transform (identity-transform))))

        ;; Intern and export sprite name
        (setf name (intern name *package*))
        (export name *package*)

        ;; Checking for duplicates inside a tree
        (let ((path (gethash name names)))
          (if path
              (error "Duplicate absolute paths in tree: ~A in ~S"
                     name
                     (cdr path))
              (setf (gethash name names) (cons t path))))

        ;; For each leaf, we iterate over its tiles and
        ;; compute their region in pixel coordinates
        ;; (relative to the whole tileset).

        (let ((pixel-function
                (getf env :tile-callback #'sdl-rect-from-env)))
          (map-tile-indices-in-order
           (getf env :order)
           env
           (lambda (&rest tile-indices)
             (push (funcall pixel-function
                            :transform transform
                            :tile-indices tile-indices
                            :env env)
                   sequence))))
        (let ((sd (make-instance (getf env
                                       :sprite-description-class
                                       'sprite-description) 
                                 :name name
                                 :id (incf counter)
                                 :spritesheet spritesheet
                                 :env env
                                 :reverse-path path
                                 :texture-id (funcall file-indexer
                                                      (namestring
                                                       (getf env :file)))
                                 :rectangles (coerce
                                              (nreverse sequence)
                                              'vector)
                                 :flip (ensure-list (getf env :flip :none)))))
          (funcall tree-builder sd)
          (push sd collection))))

    ;; We collected all sprite descriptions, perform if necessary some cleanup
    ;; with the previous definition of this spritesheet, and return the
    ;; spritesheet object

    (let ((sprites (coerce (nreverse collection) 'vector))
          (texture-files (funcall file-indexer)))

      ;; Everything went fine until here, we can "commit"
      ;; changes. Breaking the commit code below might result in an
      ;; inconsistent state.

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
           (lambda (s &aux (sym (sprite-description-name s)))
             (proclaim `(special ,sym))
             (setf (symbol-value sym) s))
           sprites)

      (setf (spritesheet-tree spritesheet) (funcall tree-builder)
            (spritesheet-sprites spritesheet) sprites
            (spritesheet-texture-files spritesheet) texture-files)

      spritesheet)))

(defun find-spritesheet (designator)
  (etypecase designator
    (symbol (or (get designator 'spritesheet)
                (error "Spritesheet not found: ~A" designator)))
    (spritesheet designator)))

(defun (setf find-spritesheet) (new name)
  (check-type name symbol)
  (check-type new spritesheet)
  (setf (get name 'spritesheet) new))

(defmacro define-spritesheet
    (name (&key
             (package '*package*)
             (format "*~{~A~^-~}*"))
     &body options-and-tree)
  (let* ((options (butlast options-and-tree))
         (tree (last options-and-tree)))
    `(setf (find-spritesheet ',name) 
       (let ((*package* (find-package ,package)))
         (compile-spritesheet-tree
          :spritesheet-name ',name
          :attribute-reducers
          (list ,@(loop for (attribute function) in options
                        collect `(quote ,attribute)
                        collect function))
          :tree (list nil nil ,@tree)
          :symbol-format ,format)))))
