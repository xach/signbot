;;;; signbot.asd
;;;; $Id: signbot.asd,v 1.1.1.1 2006/11/20 02:28:51 xach Exp $

(defpackage :signbot-system
  (:use :cl :asdf))

(in-package :signbot-system)

(defsystem #:signbot
  :depends-on (#:skippy-legacy #:zpb-ttf)
  :components ((:file "package")
               (:file "image"
                      :depends-on ("package"))
               (:file "glyphmaps"
                      :depends-on ("package"))
               (:file "signbot"
                      :depends-on ("glyphmaps"
                                   "image"
                                   "package"))))

