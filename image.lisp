;;;; $Id: image.lisp,v 1.6 2006/11/26 17:16:29 xach Exp $

(in-package #:signbot)

(deftype pixel ()
  '(unsigned-byte 8))

(defclass image ()
  ((height
    :initarg :height
    :initform (error "~S required" :height)
    :reader height)
   (width
    :initarg :width
    :initform (error "~S required" :width)
    :reader width)
   (data
    :reader data
    :writer (setf %data))
   (stride
    :reader stride
    :writer (setf %stride))))

(defmethod print-object ((image image) stream)
  (print-unreadable-object (image stream :type t :identity t)
    (format stream "~Dx~D" (width image) (height image))))

(deftype palette-index ()
  '(unsigned-byte 8))

(deftype image-data ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype image-index ()
  `(mod ,most-positive-fixnum))

(defmethod initialize-instance :after ((image image) &key height width)
  (setf (%data image) (make-array (* height width)
                                  :initial-element 0
                                  :element-type 'palette-index)
        (%stride image) width))

(defun clip (xmin0 ymin0 xmax0 ymax0
             xmin1 ymin1 xmax1 ymax1)
  (flet ((clamp (min val max)
           (cond ((< val min) min)
                 ((> val max) max)
                 (t val))))
    (values (clamp xmin0 xmin1 xmax0)
            (clamp ymin0 ymin1 ymax0)
            (clamp xmin0 xmax1 xmax0)
            (clamp ymin0 ymax1 ymax0))))

(defun clip-image (source dest &key (sx 0) (sy 0) (dx 0) (dy 0)
                   (width (width source)) (height (height source)))
  (let* ( ;; destination
         (xmin0 0)
         (ymin0 0)
         (xmax0 (width dest))
         (ymax0 (height dest))
         ;; source
         (xmin1 (- dx sx))
         (ymin1 (- dy sy))
         (xmax1 (+ xmin1 (width source)))
         (ymax1 (+ ymin1 (height source)))
         ;; source offset
         (xmin2 dx)
         (ymin2 dy)
         (xmax2 (+ xmin2 width))
         (ymax2 (+ ymin2 height)))
    ;; clip source offset to source
    (multiple-value-bind (xmin3 ymin3 xmax3 ymax3)
        (clip xmin1 ymin1 xmax1 ymax1
              xmin2 ymin2 xmax2 ymax2)
      ;; clip that against dest
      (multiple-value-bind (xmin4 ymin4 xmax4 ymax4)
          (clip xmin0 ymin0 xmax0 ymax0
                xmin3 ymin3 xmax3 ymax3)
        (values xmin4 ymin4
                (- xmin4 xmin1)
                (- ymin4 ymin1)
                (- xmax4 xmin4)
                (- ymax4 ymin4))))))

         
(defun copy (source dest
             &key (sx 0) (sy 0)
             (dx 0) (dy 0)
             (width (width source)) (height (height source)))
  (multiple-value-bind (dx* dy* sx* sy* width* height*)
      (clip-image source dest
                  :sx sx :sy sy
                  :dx dx :dy dy
                  :width width :height height)
    (when (or (zerop width*)
              (zerop height*))
      (return-from copy))
    (let ((source-data (data source))
          (source-stride (stride source))
          (dest-data (data dest))
          (dest-stride (stride dest)))
      (declare (type image-data source-data dest-data)
               (type image-index source-stride dest-stride))
      (loop repeat height*
            for source-start from (+ (* source-stride sy*) sx*) by source-stride
            for dest-start   from (+ (* dest-stride   dy*) dx*) by dest-stride
            for source-end   from (+ source-start width*) by source-stride
            do (replace dest-data source-data :start1 dest-start
                        :start2 source-start :end2 source-end))
      dest)))





;;; Save and load images

(defvar *image-magic*
  (make-array 3 :element-type '(unsigned-byte 8)
              :initial-contents (list #x89 #xAD #x17)))

(defvar *file-format-version* 1)

(defun write-u32 (i stream)
  (write-byte (logand #xFF (ash i -24)) stream)
  (write-byte (logand #xFF (ash i -16)) stream)
  (write-byte (logand #xFF (ash i  -8)) stream)
  (write-byte (logand #xFF (ash i   0)) stream))

(defun read-u32 (stream)
  (logand #xFFFFFFFF
          (+ (ash (read-byte stream) 24)
             (ash (read-byte stream) 16)
             (ash (read-byte stream)  8)
             (ash (read-byte stream)  0))))

(defun write-image (image stream)
  (write-sequence *image-magic* stream)
  (write-byte *file-format-version* stream)
  (write-u32 (width image) stream)
  (write-u32 (height image) stream)
  (write-u32 (stride image) stream)
  (write-sequence (data image) stream)
  t)

(defun read-image (stream)
  (dotimes (i (length *image-magic*))
    (let ((byte (read-byte stream)))
      (when (/= byte (aref *image-magic* i))
        (error "Bad magic in stream"))))
  (let ((version (read-byte stream)))
    (when (/= version *file-format-version*)
      (error "Unsupported version in stream -- expected ~D, read ~D"
             *file-format-version* version)))
  (let ((width (read-u32 stream))
        (height (read-u32 stream))
        (stride (read-u32 stream)))
    (declare (ignore stride))
    (when (>= (* width height) array-total-size-limit)
      (error "Image dimensions (~Dx~D) too large to load"
             width height))
    (let ((image (make-instance 'image :height height :width width)))
      (read-sequence (data image) stream)
      image)))

(defun save-image (image file &key (if-exists :supersede))
  (with-open-file (stream file :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists if-exists)
    (write-image image stream))
  (probe-file file))

(defun load-image (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8)
                          :direction :input)
    (read-image stream)))


;;; Useful operations

(defun fill (image palette-index)
  (declare (type palette-index palette-index)
           (optimize (speed 3)))
  (let ((data (data image)))
    (declare (type image-data data))
    (cl:fill data palette-index)
    (values)))

(defun clone (image)
  (let ((new-image (make-instance 'image
                                  :height (height image)
                                  :width (width image))))
    (setf (%data new-image)
          (copy-seq (data image)))
    new-image))

(defun mirror (image)
  (loop repeat (height image)
        with data = (data image)
        with stride = (stride image)
        for i = 0 then (+ i stride)
        for j = (1- stride) then (+ j stride)
        do (loop for m from i
                 for n downfrom j
                 while (< m n) do
                 (rotatef (aref data m) (aref data n))))
  image)

(defun flip (image)
  (setf (%data image) (nreverse (data image)))
  image)

(defun reflect (image)
  (flip image)
  (mirror image))

(defun scale (image factor)
  (let* ((width (* (width image) factor))
         (height (* (height image) factor))
         (new (make-instance 'image :width width :height height)))
    (dotimes (y (height image) new)
      (dotimes (x (width image))
        (let ((p (pixel-ref image x y))
              (xf (* x factor))
              (yf (* y factor)))
          (dotimes (i factor)
            (dotimes (j factor)
              (setf (pixel-ref new (+ xf i) (+ yf j)) p))))))))


(defun horizontal-line (image x y width color)
  (let* ((start (+ x (* (stride image) y)))
         (end (+ start width)))
    (cl:fill (data image) color :start start :end end)))

(defun vertical-line (image x y height color)
  (let* ((stride (stride image))
         (i (+ x (* y stride)))
         (data (data image)))
    (dotimes (j height)
      (setf (aref data i) color
            i (+ i stride)))))

(defun fill-area (image x y width height color)
  (loop with stride = (stride image)
        with data = (data image)
        with start = (+ x (* stride y))
        repeat height
        for i = start then (+ i stride)
        for j = (+ start width) then (+ j stride)
        do (cl:fill data color :start i :end j)))

(defun image-frame (image)
  (make-instance 'gif:frame
                 :data (copy-seq (the image-data (data image)))
                 :height (height image)
                 :width (width image)))

(defun save-image-gif (image file)
  (let* ((gif (make-instance 'gif:image
                             :height (height image)
                             :width (width image)
                             :color-table *palette*))
         (frame (make-instance 'gif:frame
                               :image gif
                               :data (data image))))
    (gif:output-image gif file)))


(defun pixel-ref (image x y)
  (aref (data image) (+ (* y (stride image)) x)))

(defun (setf pixel-ref) (new-value image x y)
  (setf (aref (data image) (+ (* y (stride image)) x)) new-value))

                             

