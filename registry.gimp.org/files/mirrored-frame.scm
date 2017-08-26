;;; mirrored-frame.scm

;;; Creates a frame outside the image, which mirrors the original
;;; image. A typical usage is when sending an image to a printer which
;;; only prints e.g. 30x20 cm images, while the frame that you want
;;; the image in is 22x16 cm. Using this script, you can print a 30x20
;;; cm image, grab a pair of scissors (preferably a shear/guillotine)
;;; and then cut off the mirrored frame, without fear of getting a
;;; "white border" if your measurements were slightly off. Also useful
;;; with e.g. printing to canvases if your print shop charges extra
;;; for creating a mirror frame.

;;; TODO: test using Resynthesize instead of plain mirroring.

;;; TODO: crop after first resize, to make the second mirror faster,
;;;       crop after second resize, to make the file smaller

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(script-fu-register
 "mirrored-frame"
 "<Image>/Filters/Decor/_Mirror frame"
 "Create a frame outside the visible image, that mirrors the inside content"
 "(c) Kevin Brubeck Unhammer <unhammer(at)fsfe.org>"
 "Published under GPL version 2 or later"
 "2011-10-11"
 "*"

 SF-IMAGE "Image" 0
 SF-DRAWABLE "Drawable" 0

 ;; SF-ADJUSTMENT values: '(10	; start-value
 ;; 			   0	; min-value
 ;; 			   100 	; max-value
 ;; 			   0.1 	; small-step
 ;; 			   10	; large-step
 ;; 			   1	; int=0, float=1
 ;; 			   1)	; slider=0, roll-box=1

 SF-ADJUSTMENT "Inside frame _width (relative to print width)"      '(22.5 0 4096 0.1 10 1 1)
 SF-ADJUSTMENT "Inside frame _height (relative to print width)"     '(16.7 0 4096 0.1 10 1 1) 
 SF-ADJUSTMENT "Print w_idth (should be larger than frame width)"   '(30   0 4096 0.1 10 1 1) 
 SF-ADJUSTMENT "Print h_eight (should be larger than frame height)" '(20   0 4096 0.1 10 1 1)
 ;; Would have loved to have some help text here, but I guess we can't with script-fu 
 )

(define (mirrored-frame image
			drawable
			in-width-rel
			in-height-rel
			out-width-rel
			out-height-rel)
  (gimp-image-undo-group-start image)
  
  (let* ((new-width (/ (* out-width-rel
			  (car (gimp-image-width image)))
		       in-width-rel))
	 (new-height (/ (* out-height-rel
			   (car (gimp-image-height image)))
			in-height-rel))
	 ;; for centering the resized image:
	 (x-offset (floor (* 0.5
			     (- new-width (car (gimp-image-width image))))))
	 (y-offset (floor (* 0.5
			     (- new-height (car (gimp-image-height image)))))))
    (let ((left (car (gimp-layer-new-from-visible image image "left mirror"))))
      (gimp-image-add-layer image left -1)
      (gimp-drawable-transform-flip-simple left
					   ORIENTATION-HORIZONTAL
					   FALSE
					   0.0
					   FALSE)
      ;; drop unneccessary image data:
      (gimp-layer-resize left
      			 x-offset
			 (car (gimp-drawable-height left))
       			 (- x-offset (car (gimp-drawable-width left)))
			 0)
      (let ((right (car (gimp-layer-new-from-visible image image "right mirror"))))
	(gimp-image-add-layer image right -1)
	(gimp-drawable-transform-flip-simple right
					   ORIENTATION-HORIZONTAL
					   FALSE
					   0.0
					   FALSE)
	(gimp-layer-set-offsets right
				(car (gimp-image-width image))
				0)
      ;; drop unneccessary image data:
      (gimp-layer-resize right
      			 x-offset
			 (car (gimp-drawable-height right))
       			 (- x-offset (car (gimp-drawable-width left))) ; TODO left???
			 0)
      (gimp-image-resize image
			 new-width (car (gimp-image-height image)) 
			 x-offset  0)))
    ;; again we use new-from-visible; since we resized at the end of
    ;; the previous step we get a mirror of the mirror in the outside
    ;; corners:
    (let ((top (car (gimp-layer-new-from-visible image image "top mirror"))))
      (gimp-image-add-layer image top -1)
      (gimp-drawable-transform-flip-simple top
					   ORIENTATION-VERTICAL
					   FALSE
					   0.0
					   FALSE)
      ;; drop unneccessary image data:
      (gimp-layer-resize top
			 (car (gimp-drawable-width top))
			 y-offset
			 0
       			 (- y-offset (car (gimp-drawable-height top))))
      (let ((bottom (car (gimp-layer-new-from-visible image image "bottom mirror"))))
	(gimp-image-add-layer image bottom -1)
	(gimp-drawable-transform-flip-simple bottom
					     ORIENTATION-VERTICAL
					     FALSE
					     0.0
					     FALSE)
	(gimp-layer-set-offsets bottom
				0
				(car (gimp-image-height image)))
	;; drop unneccessary image data:
	(gimp-layer-resize bottom
			 (car (gimp-drawable-width bottom))
			 y-offset
			 0
       			 (- y-offset (car (gimp-drawable-height top))))
	(gimp-image-resize image
			   (car (gimp-image-width image)) new-height 
			   0                              y-offset))))
  
  (gimp-image-undo-group-end image)
  (gimp-displays-flush))

