;;;; CENTER-SELECTION -- crop an image to center the current selection
;;;; By Scott Pakin <scott+csel@pakin.org>
;;;; Version 1.0

;;; Define the function that does all the work of cropping an image.
(define (script-fu-crop-to-center-selection image
                                            drawable
                                            center-x?
                                            center-y?
                                            preserve-aspect-ratio?)
  (gimp-image-undo-group-start image)
  (let ((selection-bbox (gimp-selection-bounds image)))
    (if (= (car selection-bbox) 0)
        (gimp-message "There needs to be be an active selection for this script to work")
        (let* ((original-width (car (gimp-image-width image)))
               (original-height (car (gimp-image-height image)))
               (new-width original-width)
               (new-height original-height)
               (offset-x 0)
               (offset-y 0))

          ;; Define a helper function that maps a total distance
          ;; (original-width or original-height), the starting point
          ;; of the selection (x0 or y0), and the ending point of the
          ;; selection (x1 or y1) to a crop offset and crop length.
          (let ((compute-offset-and-length
                 (lambda (original-distance
                          selection-start
                          selection-end)
                   (let* ((selection-center (trunc (/ (+ selection-start
                                                         selection-end
                                                         -1)
                                                      2)))
                          (before-selection-center selection-center)
                          (after-selection-center (- original-distance
                                                     selection-center
                                                     1)))
                     (cond ((< before-selection-center after-selection-center)
                            ;; Crop the part of the image preceding
                            ;; the selection.
                            (list 0 (+ (* 2 selection-center) 1)))
                           ((> before-selection-center after-selection-center)
                            ;; Crop the part of the image following
                            ;; the selection.
                            (let ((offset (- selection-start
                                             (- original-distance selection-end))))
                              (list offset (- original-distance offset))))
                           ;; Don't crop anything.
                           (t (list 0 original-distance)))))))

            ;; Determine the new image width and the offset from the
            ;; original image.
            (if (eq? center-x? TRUE)
                (let ((x-crop-values (compute-offset-and-length
                                      original-width
                                      (nth 1 selection-bbox)
                                      (nth 3 selection-bbox))))
                  (set! offset-x (nth 0 x-crop-values))
                  (set! new-width (nth 1 x-crop-values))))

            ;; Determine the new image height and the offset from the
            ;; original image.
            (if (eq? center-y? TRUE)
                (let ((y-crop-values (compute-offset-and-length
                                      original-height
                                      (nth 2 selection-bbox)
                                      (nth 4 selection-bbox))))
                  (set! offset-y (nth 0 y-crop-values))
                  (set! new-height (nth 1 y-crop-values))))

            ;; Crop the image.
            (gimp-image-crop image new-width new-height offset-x offset-y))

          ;; Optionally crop the image a second time to preserve the
          ;; original image's aspect ratio.
          (if (eq? preserve-aspect-ratio? TRUE)
              (let ((original-ratio (/ original-width original-height))
                    (new-ratio (/ new-width new-height)))
                (cond ((> original-ratio new-ratio)
                       ;; The new image is too tall -- crop it vertically.
                       (let* ((final-height (trunc (/ (* new-width original-height)
                                                      original-width)))
                              (offset-y (trunc (/ (- new-height final-height) 2))))
                         (gimp-image-crop image new-width final-height 0 offset-y)))
                      ((< original-ratio new-ratio)
                       ;; The new image is too wide -- crop it horizontally.
                       (let* ((final-width (trunc (/ (* new-height original-width)
                                                      original-height)))
                              (offset-x (trunc (/ (- new-width final-width) 2))))
                         (gimp-image-crop image final-width new-height offset-x 0)))))))))

  ;; Flush the resulting image.
  (gimp-image-undo-group-end image)
  (gimp-displays-flush))


;;; Register the function.
(script-fu-register
 "script-fu-crop-to-center-selection"
 _"_Center Selection..."
 "Crops the image so that the center of the current selection becomes the center of the entire image."
 "Scott Pakin <scott+csel@pakin.org>"
 "Copyright (C) 2008, Scott Pakin"
 "21 September 2008"
 "*"
 SF-IMAGE    "Image"     0
 SF-DRAWABLE "Drawable"  0
 SF-TOGGLE   _"Center _horizontally"  TRUE
 SF-TOGGLE   _"Center _vertically"  TRUE
 SF-TOGGLE   _"Preserve _aspect ratio" FALSE)

;;; Create a menu item to invoke the script.
(script-fu-menu-register "script-fu-crop-to-center-selection"
                         "<Image>/Script-Fu/Selection")
