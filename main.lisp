(in-package :kingdom)

;;
;; COMMUNICATION BETWEEN THREADS
;;

(defparameter *channel* (trivial-channels:make-channel))

(defun send (function)
  (trivial-channels:sendmsg *channel* function))

(defmacro eval-in-game (&body body)
  `(send (lambda () ,@body)))

(defun execute-external-functions ()
  (loop for function = (trivial-channels:getmsg *channel*)
        while function
        do (restart-case (funcall function)
             (continue () :report "Ignore error from external function"))))

(defun discard-external-functions ()
  (loop for function = (trivial-channels:getmsg *channel*)
        while function))


(defmacro define-command (name args &body body)
  ;;
  ;; This macro should have been defined before the "kingdom"
  ;; package. also, it should be possible to return values from
  ;; commands (another channel).
  ;;
  (with-gensyms (command)
    `(defun ,name ,args
       (flet ((,command () ,@body))
         (if (and (boundp '*game*) *game*)
             (,command)
             (send (function ,command)))))))

;;
;; RUN A MAIN-LOOP WITH SOME HANDLERS
;;

(define-command lone-king ()
  (setf (game-active-object *game*) (list (find-king))))

(defun run (main-loop)
  (with-everything (:window (*window* :flags '(:hidden :opengl)) :gl *gl*)
    (with-renderer (*renderer* *window*)
      (handler-bind
          ((main-loop-restart-condition #'invoke-main-loop-restart))
        (tagbody
         restart
           (restart-case
               (funcall main-loop)
             (restart-main-loop (&optional new-main-loop)
               (when new-main-loop
                 (setf main-loop new-main-loop))
               (go restart))))))))

(defun populate-game (game)
  (setf (game-static-object game)
        (list
         (make-background :r 190
                          :g 185
                          :b 160)
         (make-ground :level (round (@ 9.8))
                      :r 200
                      :g 200
                      :b 170)
         (make-ground :level (round (@ 4.3))
                      :r 200
                      :g 198
                      :b 150)
         (make-ground :level (round (@ 4.1))
                      :r 200
                      :g 170
                      :b 90)
         (make-ground :level (+ (@ 4) 2)
                      :r 90
                      :g 50
                      :b 10)
         (make-ground :level (round (@ 3.9))
                      :r 100
                      :g 90
                      :b 70)))
  (setf (game-active-object game)
        (list (make-instance 'king
                             :x (@ 5)
                             :y (@ 5)))))

(defun followers ()
  (discard-external-functions)
  (set-window-title *window* "Followers")
  (with-active-spritesheets (*forest*)
    (render-clear *renderer*)
    (let ((*game* (make-instance 'game)))
      (resize-game-window)
      (populate-game *game*)
      (add-golden-thing)
      (display *game*)
      (render-present *renderer*)
      (show-window *window*)
      (with-event-loop ()
        (:keydown (:keysym keysym)
                  (on-key-down *game*
                               :scancode (sdl2:scancode-value keysym)
                               :sym (sdl2:sym-value keysym)
                               :mod (sdl2:mod-value keysym)
                               :allow-other-keys t))
        (:keyup (:keysym keysym)
                (on-key-up *game*
                           :scancode (sdl2:scancode-value keysym)
                           :sym (sdl2:sym-value keysym)
                           :mod (sdl2:mod-value keysym)
                           :allow-other-keys t))
        (:quit () t)
        (:idle ()
               (execute-external-functions)
               (set-render-draw-color *renderer* 0 0 0 255)
	       (render-clear *renderer*)
               (update *game*)
               (display *game*)
               (render-present *renderer*)
               (delay 10)
               (sb-ext:gc))))))

;; USEFUL ENTRY POINTS

(defun cl-user::test ()
  (run 'followers))

(defun cl-user::redo ()
  (ql:quickload :game)
  (cl-user::test))


