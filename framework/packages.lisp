(in-package :cl)

(defpackage :game.framework.signals
  (:use :cl)
  (:export :main-loop-restart-condition
	   #:invoke-main-loop-restart
	   #:restart-main-loop))

(defpackage :game.framework.messages
  (:use :cl)
  (:import-from :trivial-channels
                #:sendmsg
                #:getmsg
                #:recvmsg)
  (:export #:execute-external-functions
           #:discard-external-functions
           #:eval-in-game
           #:funcall-in-game
           #:define-command))

(defpackage :game.framework.globals
  (:use :cl)
  (:export #:*window*
	   #:*gl*
	   #:*renderer*

           #:cleanup
           
           #:display
           #:update

           #:on-key-up
           #:on-key-down))

(defpackage :game.framework.dpi
  (:use :cl)
  (:import-from :alexandria
                #:mean)
  (:import-from :game.framework.globals
                #:*window*)
  (:import-from :cffi
                #:with-foreign-objects
                #:mem-ref)
  (:import-from :sdl2-ffi.functions
                #:sdl-get-display-dpi
                #:sdl-get-window-display-index)
  (:export #:window-dpi
           #:display-dpi
           #:dpi-d
           #:dpi-h
           #:dpi-v))

(defpackage :game.framework.resources
  (:use :cl)
  (:export :setup-translations/development
           #:setup-translation/executable
           #:font-path
           #:sprite-path))

(defpackage :game.framework.fonts
  (:use :cl :sdl2-ttf)
  (:import-from :alexandria
                #:with-gensyms)
  (:import-from :game.framework.resources
                #:font-path)
  (:import-from :game.framework.dpi
                #:window-dpi
                #:dpi-v)
  (:export :load-font
           #:with-font
           #:with-fonts))

(defpackage :game.framework.transform
  (:use :cl)
  (:export #:scale
           #:move
           #:combine
           #:pixel-rectangle
           #:identity-transform
           #:transform-handler))

(defpackage :game.framework.spritesheet-trees
  (:use :cl)
  (:import-from :alexandria
                #:once-only
                #:ensure-list)
  (:import-from :game.framework.globals
                #:cleanup)
  (:import-from :game.framework.transform
                #:scale
                #:move
                #:combine
                #:identity-transform
                #:pixel-rectangle
                #:transform-handler)
  (:export :compile-spritesheet-tree
           #:*spritesheet-cleanup*
           
           #:spritesheet-name
           #:spritesheet-tree
           #:spritesheet-sprites
           #:spritesheet-texture-files
           
           #:sprite-description 
           #:sprite-description-spritesheet 
           #:sprite-description-id          
           #:sprite-description-name        
           #:sprite-description-rectangles  
           #:sprite-description-flip        
           #:sprite-description-env   
           #:sprite-description-texture-id

           #:find-spritesheet
           #:define-spritesheet
           #:spritesheet
           
           #:switch-to-sprite
           #:spritef))

(defpackage :game.framework.spritesheet-textures
  (:use :cl
        :sdl2
        :game.framework.globals
        :game.framework.spritesheet-trees
        :alexandria)
  (:export #:with-active-spritesheets
           #:call-with-active-spritesheets
           
           #:animation-reducer
           
           #:sprite
           #:sprite-dest-rect
           #:sprite-cursor
           #:sprite-description

           #:sprite-reset
           #:sprite-set

           #:render-sprite

           #:sprite-step
           #:sprite-animation-step))

(defpackage :game.framework.actors
  (:use :cl
        :game.framework.spritesheet-textures
        :game.framework.globals
        :alexandria)
  (:import-from :sdl2 #:make-rect)
  (:export #:actor))
