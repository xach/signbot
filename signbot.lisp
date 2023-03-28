;; signbot.lisp

(in-package :signbot)

(defparameter *palette* #(#x00 #x00 #x00 ; +black+
                          #xFF #xFF #xFF ; +white+, +transparent+
                          #xFF #x00 #x00 ; +red+
                          #x88 #x00 #x00 ; +red1+
                          #x66 #x00 #x00 ; +red2+
                          #x22 #x22 #x22 ; +darkgray+
                          #x66 #x66 #x66 ; +lightgray+
                          #xFF #x66 #x66 ; +brightred+
                          ))

(defparameter *blue-palette*
  #(#x00 #x00 #x00                      ; +black+
    #xFF #xFF #xFF                      ; +white+, +transparent+
    #x00 #x00 #xFF                      ; +red+
    #x00 #x00 #x66                      ; +red1+
    #x00 #x00 #x33                      ; +red2+
    #x22 #x22 #x22                      ; +darkgray+
    #x66 #x66 #x66                      ; +lightgray+
    #x77 #x77 #xFF                      ; +brightred+
    ))

(defparameter *green-palette*
  #(#x00 #x00 #x00                      ; +black+
    #xFF #xFF #xFF                      ; +white+, +transparent+
    #x00 #xCC #x00                      ; +red+
    #x00 #x66 #x00                      ; +red1+
    #x00 #x44 #x00                      ; +red2+
    #x22 #x22 #x22                      ; +darkgray+
    #x66 #x66 #x66                      ; +lightgray+
    #x99 #xFF #x99                      ; +brightred+
    ))

(defparameter *amber-palette*
  #(#x00 #x00 #x00                      ; +black+
    #xFF #xFF #xFF                      ; +white+, +transparent+
    #xDD #x99 #x00                      ; +red+
    #x66 #x33 #x00                      ; +red1+
    #x44 #x22 #x00                      ; +red2+
    #x22 #x22 #x22                      ; +darkgray+
    #x66 #x66 #x66                      ; +lightgray+
    #xDD #x99 #x33                      ; +brightred+
    ))

(defparameter *jmc-palette*
  #(#x00 #x00 #x00                      ; +black+
    #xFF #xFF #xFF                      ; +white+, +transparent+
    #xDD #x99 #x00                      ; +red+
    #x17 #x00 #x5B                      ; +red1+
    #x00 #x00 #x22                      ; +red2+
    #x22 #x22 #x22                      ; +darkgray+
    #x66 #x66 #x66                      ; +lightgray+
    #xFF #xBD #x05                      ; +brightred+
    ))
  

(defconstant +black+ 0)
(defconstant +white+ 1)
(defconstant +transparent+ 1)
(defconstant +red+ 2)
(defconstant +red1+ 3)
(defconstant +red2+ 4)
(defconstant +darkgray+ 5)
(defconstant +lightgray+ 6)
(defconstant +brightred+ 7)



;;; convert a bitmap into a 'sploded image.
;;;
;;; Sploded output has a NxN grid for each input pixel, and there is M
;;; pixels of padding around and between grids. So something like
;;;
;;; Input:
;;; 010
;;; 111
;;;
;;; Output: 
;;;
;;; |<-- 13 --->| aka (padding * (width + 1)) + (pixel-size * width)
;;; 3333333333333
;;; 3000311130003
;;; 3000311130003
;;; 3000311130003
;;; 3333333333333
;;; 3111311131113
;;; 3111311131113
;;; 3111311131113
;;; 3333333333333

(defun splode-canvas (&key height width pixel-size padding)
  (let ((width (+ (* padding (1+ width))
                  (* pixel-size width)))
        (height (+ (* padding (1+ height))
                   (* pixel-size height))))
    (make-instance 'image :height height :width width)))

(defparameter *off-image* nil)
(defparameter *on-image* nil)

(defun off-image ()
  (or *off-image*
      (setf *off-image*
            (let ((image (make-instance 'image :height 2 :width 2)))
              (fill image +red2+)
              (setf (pixel-ref image 0 0) +red1+)
              image))))

(defun on-image ()
  (or *on-image*
      (setf *on-image*
            (let ((image (make-instance 'image :height 2 :width 2)))
              (fill image +red+)
              (setf (pixel-ref image 0 0) +brightred+)
              image))))
  
(defun splode (image &key (step 3))
  (let ((sploded (splode-canvas :height (height image)
                                :width (+ (width image) step)
                                :padding 1
                                :pixel-size 2))
        (on (on-image))
        (off (off-image)))
    (dotimes (x (width image) sploded)
      (dotimes (y (height image))
        (let ((dx (+ 1
                     (* 1 x)
                     (* 2 x)))
              (dy (+ 1
                     (* 1 y)
                     (* 2 y))))
          (when (plusp (pixel-ref image x y))
            (copy on sploded :dx dx :dy dy)
            (copy off sploded :dx (+ dx step) :dy dy)))))))
                  

;;; Background image



(defun make-corners ()
  (let ((tl (make-instance 'image :height 4 :width 4))
        (black 0)
        (transparent 1))
    (fill tl black)
    (horizontal-line tl 0 0 4 transparent)
    (vertical-line tl 0 0 4 transparent)
    (setf (pixel-ref tl 1 1) transparent)
    (let* ((bl (reflect (clone tl)))
           (br (mirror (clone bl)))
           (tr (reflect (clone br))))
      (values tl bl tr br))))




(defun make-background (width height)
  (let ((image (make-instance 'image :width width :height height)))
    (fill image +black+)
    (fill-area image 5 5 (- width 10) (- height 10) +darkgray+)
    (multiple-value-bind (tl bl tr br)
        (make-corners)
      (let ((trx (- width (width tr)))
            (bry (- height (height br))))
        (copy tl image)
        (copy tr image :dx trx)
        (copy br image :dx trx :dy bry)
        (copy bl image :dy bry)))
    (horizontal-line image 6 (- height 6) (- width 12) +lightgray+)
    (vertical-line image (- width 6) 6 (- height 12) +lightgray+)
    (loop for dy from 7 below (- height 8) by 3 do
          (loop for dx from 7 below (- width 7) by 3
                do (copy (off-image) image :dx dx :dy dy)))
    image))


;;; The actual scroller creator
(defparameter *default-max-width* 200)
(defparameter *default-step* 6)
(defparameter *default-delay* 5)
(defparameter *font* (merge-pathnames #p"volter-goldfish.ttf"
                                      #.(or *load-truename*
                                            *compile-file-truename*)))
(defvar *frame-width-padding* 12)
(defvar *frame-height-padding* 12)

(defun make-sign (string file &key
                  (step *default-step*)
                  (width *default-max-width*)
                  (delay *default-delay*))
  (declare (optimize (debug 3)))
 (with-font-loader (font *font*)
    (let* ((frame-width (- width *frame-width-padding*))
           (string-image (stringimage string font))
           (source (splode string-image :step step))
           (dest (make-instance 'image
                                :height (height source)
                                :width (- frame-width (mod frame-width 3))))
           (bg (make-background (+ (width dest) *frame-width-padding*)
                                (+ (height dest) *frame-height-padding*)))
           (gif (make-instance 'gif:image
                               :height (height bg)
                               :width (width bg)
                               :color-table *palette*
                               :loopingp t
                               :comment "http://wigflip.com/signbot/")))
      (let ((frame (image-frame bg)))
        (setf (gif:transparency-index frame) +transparent+)
        (setf (gif:image frame) gif)
        (gif:add-delay delay gif))
      (loop with step = step
            for i below (+ (width source) frame-width) by step
            for dx = (+ step (width dest)) then (- dx step)
            do
            (fill dest 0)
            (copy source dest :dx dx)
            (let ((frame (image-frame dest)))
              (setf (gif:image frame) gif
                    (gif::offset-top frame) (truncate *frame-height-padding* 2)
                    (gif::offset-left frame) (truncate *frame-width-padding* 2)
                    (gif:transparency-index frame) 0
                    (gif:disposal-method frame) :combine))
            (gif:add-delay delay gif))
      (gif:add-delay 100 gif)
      (let ((file (gif:output-image gif file)))
        (values file (width bg) (height bg))))))

(defun make-icon (string file &key
                  (step 1)
                  (width 16)
                  (delay 10))
  (with-font-loader (font *font*)
    (let* ((source (stringimage string font))
           (dest (make-instance 'image
                                :height 16
                                :width 16))
           (bg (clone dest))
           (gif (make-instance 'gif:image
                               :height (height bg)
                               :width (width bg)
                               :color-table *palette*
                               :loopingp t
                               :comment "http://wigflip.com/signbot/")))
      (let ((frame (image-frame bg)))
        (setf (gif:transparency-index frame) +transparent+)
        (setf (gif:image frame) gif)
        (gif:add-delay delay gif))
      (loop with step = step
            for i below (+ (width source) 16) by step
            for dx = (+ step (width dest)) then (- dx step)
            do
            (fill dest 0)
            (copy source dest :dx dx)
            (let ((frame (image-frame dest)))
              (setf (gif:image frame) gif))
            (gif:add-delay delay gif))
      (gif:add-delay 100 gif)
      (let ((file (gif:output-image gif file)))
        (values file (width bg) (height bg))))))
