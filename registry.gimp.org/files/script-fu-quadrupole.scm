; Quadrupole, V1.0
;
; http://flickr.com/photos/theilr, (c) 2009
;
; This script was tested with GIMP 2.6.7
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License Version 3 as 
; published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
; GNU General Public License at  http://www.gnu.org/licenses for
; more details.
;
; DESCRIPTION: Takes an image, makes four 1/4 scale copies, and
; reflects them about vertical and horizontal axes to achieve a
; four-fold symmetry.
; 
; BUGS: As well as being atrociously inefficient (eg, by scaling the
; same image four times, instead of making copies after the first
; scaling), this code does substantially less than the "Small Tiles"
; dialog (which comes standard in Gimp in <Image>/Filters/Map) 
;
; I don't know what it will do with an image which has an odd number
; of pixels as either the height or width
;
; The script is located in menu "<Image> / Filters / theilr / Quadrupole"
;
; 
; Version 1.0 (Oct 2009)
; =============================================================================

(define (script-fu-quadrupole inImage inLayer inOrient)
  (let* (
	 (theWidth (car (gimp-image-width inImage)))
	 (theHeight (car (gimp-image-height inImage)))
	 (halfWidth (/ theWidth 2))
	 (halfHeight (/ theHeight 2))
	 (newLayer1 (car (gimp-layer-copy inLayer TRUE)))
	 (newLayer2 (car (gimp-layer-copy inLayer TRUE)))
	 (newLayer3 (car (gimp-layer-copy inLayer TRUE)))
	 (newLayer4 (car (gimp-layer-copy inLayer TRUE)))
	 )
    (gimp-image-undo-group-start inImage) 

    ;; Add four scaled layers (nb, must be a way to avoid all this dup'd effort)
    (gimp-image-add-layer inImage newLayer1 -1)
    (gimp-drawable-set-name newLayer1 "Quadrupole")
    (gimp-layer-scale-full newLayer1 halfWidth halfHeight FALSE INTERPOLATION-CUBIC)

    (gimp-image-add-layer inImage newLayer2 -1)
    (gimp-layer-scale-full newLayer2 halfWidth halfHeight FALSE INTERPOLATION-CUBIC)

    (gimp-image-add-layer inImage newLayer3 -1)
    (gimp-layer-scale-full newLayer3 halfWidth halfHeight FALSE INTERPOLATION-CUBIC)

    (gimp-image-add-layer inImage newLayer4 -1)
    (gimp-layer-scale-full newLayer4 halfWidth halfHeight FALSE INTERPOLATION-CUBIC)


    (gimp-drawable-transform-flip-simple newLayer2 1 1 halfWidth TRUE)
    (gimp-drawable-transform-flip-simple newLayer3 1 1 halfWidth TRUE)
    (gimp-drawable-transform-flip-simple newLayer3 0 1 halfHeight TRUE)
    (gimp-drawable-transform-flip-simple newLayer4 0 1 halfHeight TRUE)

    (if (= inOrient 0) ;; UL
	(begin
	  (gimp-layer-set-offsets newLayer2 0 halfHeight)
	  (gimp-layer-set-offsets newLayer3 halfWidth halfHeight)
	  (gimp-layer-set-offsets newLayer4 halfWidth 0)
	  )
	)
    
    (if (= inOrient 1) ;; UR
	(begin
	  (gimp-layer-set-offsets newLayer3 0 halfHeight)
	  (gimp-layer-set-offsets newLayer2 halfWidth halfHeight)
	  (gimp-layer-set-offsets newLayer1 halfWidth 0)
	  )
	)
    
    (if (= inOrient 2) ;; LR
	(begin
	  (gimp-layer-set-offsets newLayer4 0 halfHeight)
	  (gimp-layer-set-offsets newLayer1 halfWidth halfHeight)
	  (gimp-layer-set-offsets newLayer2 halfWidth 0)
	  )
	)

    (if (= inOrient 3) ;; LL
	(begin
	  (gimp-layer-set-offsets newLayer1 0 halfHeight)
	  (gimp-layer-set-offsets newLayer4 halfWidth halfHeight)
	  (gimp-layer-set-offsets newLayer3 halfWidth 0)
	  )
	)


    
    (gimp-image-merge-down 
     inImage 
     (car (gimp-image-merge-down 
	   inImage 
	   (car (gimp-image-merge-down 
		 inImage 
		 newLayer4 
		 0) 
		)
	   0)
	  )
     0)

    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-quadrupole"
		    "_Quadrupole"
		    "Makes four copies of image and symmetrizes them"
		    "theilr"
		    "(c) theilr"
		    "24 Oct 2009"
		    "RGB*"
		    SF-IMAGE "Image"  0
		    SF-DRAWABLE "Drawable" 0 
		    SF-OPTION "Orientation" '("Image to UpperLeft" 
					      "Image to UpperRight"
					      "Image to LowerRight"
					      "Image to LowerLeft")
		    )
(script-fu-menu-register "script-fu-quadrupole" "<Image>/Filters/theilr")
