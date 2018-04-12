(in-package :game.framework.messages)

(defparameter *in-channel* (trivial-channels:make-channel))
(defparameter *out-channel* (trivial-channels:make-channel))

;; SDL2 thread = Game

;; TODO: CHANGE! SIMPLY USE *GAME*

(defun game-thread-p (&optional (thread sb-thread:*current-thread*))
  (search "SDL2" (sb-thread:thread-name thread)))

(defun game-started-p ()
  (some #'game-thread-p (sb-thread:list-all-threads)))

;; Send/receive functions

(defun send (function)
  (trivial-channels:sendmsg *in-channel* function))

(defun %send-result (&rest values)
  (trivial-channels:sendmsg *out-channel* values))

(defun receive ()
  (values-list (trivial-channels:recvmsg *out-channel*)))

(defun flush (channel)
  (loop while (trivial-channels:getmsg channel)))

(defun flush-channels ()
  (flush *in-channel*)
  (flush *out-channel*))

(defun funcall-in-game (function &optional (timeout 3))
  (unless (find-if (lambda (s) (search "SDL2" s))
                   (sb-thread:list-all-threads)
                   :key #'sb-thread:thread-name)
    (cerror "CONTINUE" "No thread named SDL2."))
  (handler-case 
      (trivial-timeout:with-timeout (timeout) 
        (send (lambda () (multiple-value-call #'%send-result (funcall function))))        
        (receive))
    (trivial-timeout:timeout-error (e)
      (flush-channels)
      (signal e))))

(defmacro eval-in-game (&body body)
  `(funcall-in-game (lambda () ,@body)))

(defmacro define-command (name args &body body)
  (alexandria:with-gensyms (command)
    `(defun ,name ,args
       (flet ((,command () ,@body))
         (if (game-thread-p)
             (,command)
             (funcall-in-game (function ,command)))))))

;; Called from inside game

(defun execute-external-functions ()
  (loop for function = (trivial-channels:getmsg *in-channel*)
        while function
        do (restart-case (funcall function)
             (continue () :report "Ignore error from external function"))))

(defun discard-external-functions ()
  (flush-channels))

