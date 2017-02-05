(in-package #:game.framework.signals)

(define-condition game-condition (condition) ())

(define-condition main-loop-restart-condition (game-condition)
  ((main-loop :initarg :main-loop :accessor main-loop-of :initform nil))
  (:report "Requesting a restart of the main loop."))

(defgeneric invoke-main-loop-restart (e)
  (:method ((e main-loop-restart-condition))
    (invoke-restart 'restart-main-loop (main-loop-of e))))

(defgeneric restart-main-loop (&optional new-main-loop)
  (:method (&optional new-main-loop)
    (signal 'main-loop-restart-condition :main-loop new-main-loop)))
