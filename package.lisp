;;;; $Id: package.lisp,v 1.3 2006/11/29 13:46:59 xach Exp $

(defpackage #:signbot
  (:use #:cl #:zpb-ttf)
  (:export #:make-sign #:*palette* #:*blue-palette* #:*green-palette*
           #:*amber-palette*)
  (:shadow #:fill))
