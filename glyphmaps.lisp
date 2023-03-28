;;;; $Id: glyphmaps.lisp,v 1.2 2006/11/25 19:23:28 xach Exp $

(in-package #:signbot)

(defun round* (i)
  (round i 228))

(defun vertical-segments (glyph)
  (let ((verticals '()))
    (do-contours (contour glyph (sort verticals #'< :key #'first))
      (do-contour-segments (p0 p1 p2)
          contour
        (when (and (not p1)
                   (= (round* (x p0))
                      (round* (x p2))))
          (push (list (round* (x p0))
                      (round* (y p0))
                      (round* (y p2)))
                verticals))))))

(defun pixel-height (glyph)
  (values (round* (- (zpb-ttf:ymax glyph) (zpb-ttf:ymin glyph)))))

(defun pixel-width (glyph)
  (values (round* (- (zpb-ttf:xmax glyph) (zpb-ttf:xmin glyph)))))

(defun spans (glyph)
  (let ((spans '())
        (span-vector (make-array (pixel-height glyph) :initial-element nil)))
    (dolist (segment (vertical-segments glyph) spans)
      (destructuring-bind (x y0 y1)
          segment
        (when (< y1 y0)
          (rotatef y1 y0))
        (loop for raw-y from y0 below y1
              for y = (- raw-y (round* (ymin glyph)))
              for left-edge = (aref span-vector y)
              if left-edge do
              (push (list y left-edge x) spans)
              (setf (aref span-vector y) nil)
              else do
              (setf (aref span-vector y) x))))))
              

(defvar *bitmap-cache* (make-hash-table))


(defun draw-bitmap (glyph)
  (let ((spans (spans glyph))
        (image (make-instance 'image
                              :height (pixel-height glyph)
                              :width (pixel-width glyph))))
    (let ((glyph-xmin (round* (xmin glyph))))
      (dolist (span spans image)
        (destructuring-bind (y xmin xmax) span
          (loop for x from xmin below xmax
                for yi = (- (pixel-height glyph) y 1)
                do (setf (pixel-ref image (- x glyph-xmin) yi) 1)))))))

(defun bitmap (glyph)
  (let ((cached (gethash (zpb-ttf:font-index glyph) *bitmap-cache*)))
    (or cached
        (setf (gethash (zpb-ttf:font-index glyph) *bitmap-cache*)
              (draw-bitmap glyph)))))

(defun stringimage (string font)
  (let* ((x 1)
         (y 1)
         (bbox (string-bounding-box string font))
         (max (round* (ymax bbox)))
         (image (make-instance 'image
                               :height (+ 2 (- (round* (ymax bbox))
                                               (round* (ymin bbox))))
                               :width (+ 2 (- (round* (xmax bbox))
                                              (round* (xmin bbox)))))))
    (dotimes (i (length string))
      (let* ((char (char string i))
             (glyph (find-glyph char font))
             (bitmap (bitmap glyph)))
        (copy bitmap image
              :dx (+ x (round* (xmin glyph)))
              :dy (+ y (- max (round* (ymax glyph)))))
        (incf x (round* (advance-width glyph)))))
    image))
