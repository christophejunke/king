(in-package #:game.framework.dpi)

(defstruct dpi d h v)

(defun display-dpi (display-index)
  (with-foreign-objects ((ddpi :float)
                         (hdpi :float)
                         (vdpi :float))
    (sdl-get-display-dpi display-index ddpi hdpi vdpi)
    (make-dpi :d (mem-ref ddpi :float)
              :h (mem-ref hdpi :float)
              :v (mem-ref vdpi :float))))

(defun window-dpi (&optional (window *window*))
  (display-dpi
   (sdl-get-window-display-index window)))

