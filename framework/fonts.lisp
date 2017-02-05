(in-package #:game.framework.fonts)

(defun load-font (point-size pathname)
  ;; SDL2-TTF assumes 72 Dots per inches in OPEN-FONT
  (open-font (or (probe-file pathname)
                 (error "No such font: ~S" pathname))
             (round (* (dpi-v (window-dpi))
                       point-size
                       1/72))))

(defmacro with-font ((visible-var pt &rest name) &body body)
  (with-gensyms (hidden-var)
    `(let* ((,hidden-var (load-font ,pt ,(apply #'font name)))
            (,visible-var ,hidden-var))
       (unwind-protect (progn ,@body)
         (close-font ,hidden-var)))))

(defmacro with-fonts (bindings &body body)
  (etypecase bindings
    (null `(progn ,@body))
    (cons `(with-font ,(first bindings)
             (with-fonts ,(rest bindings)
               ,@body)))))


