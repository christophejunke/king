(in-package #:game.framework.resources)

(defun setup-translations (&optional (root *default-pathname-defaults*))
  (setf (logical-pathname-translations "RES")
        `(("RES:**;*.*.*" ,(merge-pathnames
                            (make-pathname :directory
                                           '(:relative "resources" :wild-inferiors)
                                           :name :wild
                                           :type :wild)
                            root)))))

(defun setup-translations/development ()
  "Setup translations during development so that directories are
relative to the system's source directory."
  ;; Ignore errors if the component is not found, which happens when
  ;; the executable is deployed.
  (ignore-errors
   (setup-translations 
    (asdf:system-source-directory :game))))


(defun setup-translation/executable ()
  "Call this when starting the executable to setup directories."
  (setup-translations sb-ext:*core-pathname*))

;; (sb-ext:save-lisp-and-die
;;  #P"/tmp/game"
;;  :executable t
;;  :toplevel (lambda (&rest args)
;;              (print args)
;;              (print sb-ext:*core-pathname*)
;;              (print
;;               (setup-translation/executable))
;;              (sb-impl::toplevel-init)))

;; Then we can even move the executable elsewhere, and the directory
;; changes accordingly when running it.

(defun resource (components directory default-type)
  (merge-pathnames
   ;; Make a relative logical pathname from path components
   (format nil "~{;~a~}" components)
   ;; Define root path and default type
   (make-pathname :host "RES"
                  :directory directory
                  :type default-type)))

(defun font-path (&rest c) (resource c "fonts" "ttf"))
(defun sprite-path (&rest c) (resource c "sprites" "png"))

(setup-translations/development)

