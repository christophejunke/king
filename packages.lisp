(in-package :cl-user)

(defpackage #:kingdom
  (:use #:cl
        #:sdl2
        #:alexandria
        #:game.framework.messages
        #:game.framework.signals
        #:game.framework.globals
        #:game.framework.transform
        #:game.framework.spritesheet-trees
        #:game.framework.spritesheet-textures
        #:game.framework.actors
        #:game.framework.resources))


