; Multi-Burst
;    transforms Sony multi-burst images (4x4 small images)
;    into an animated gif

; Copyright (C) 2005-2008 Eicke Godehardt <eicke@godehardt.org>

(define (script-fu-multi-burst inImage
                               drawable
                               tiles-x
                               tiles-y
                               rotation)
  (let* ((tile-width (/ (car (gimp-image-width inImage)) tiles-x))
	 (tile-height (/ (car (gimp-image-height inImage)) tiles-y))
	 (number (* tiles-x tiles-y))
	 (counter number)
	 (theImage (car (gimp-image-new tile-width tile-height RGB)))
	 (layer)
	 (x)
	 (y)
	 (xpos)
	 (ypos))

    (gimp-context-push)

;    (gimp-image-set-active-layer inImage drawable)
;    (gimp-image-undo-group-start inImage)

    (gimp-display-new theImage)
    (set! y tiles-y)
    (while (> y 0)
      (set! y (- y 1))
      (set! x tiles-x)
      (set! ypos (* y tile-height))
      (while (> x 0)
	(set! x (- x 1))
	(set! counter (- counter 1))
	(set! xpos (* x tile-width))

	(set! layer (car (gimp-layer-new theImage tile-width tile-height RGB-IMAGE
					 (string-append "frame" (number->string counter) "(300ms)(replace)") 100 NORMAL-MODE)))
	(gimp-image-add-layer theImage layer number)

	(gimp-rect-select inImage
			  xpos
			  ypos
			  tile-width
			  tile-height
			  CHANNEL-OP-REPLACE
			  FALSE
			  0)

	(gimp-edit-copy drawable)
	(gimp-floating-sel-anchor (car (gimp-edit-paste layer FALSE)))))

    (gimp-selection-none inImage)
;    (gimp-image-undo-group-end inImage)
    (gimp-image-convert-indexed theImage FS-DITHER MAKE-PALETTE 256
				         FALSE FALSE "dummy")
    (cond ((= rotation 1) (gimp-image-rotate theImage ROTATE-90))
	  ((= rotation 2) (gimp-image-rotate theImage ROTATE-180))
	  ((= rotation 3) (gimp-image-rotate theImage ROTATE-270)))

;    (file-gif-save 0 theImage layer "" "test" FALSE TRUE 300 2)
    ))






(script-fu-register "script-fu-multi-burst"
		    _"_Multi Burst..."
		    "Transforms multi-burst images (e.g. Sony 4x4) into multiple layers for an animated gif"
		    "Eicke Godehardt <eicke@godehardt.org>" ; author
		    "Eicke Godehardt"                       ; copyright holder
		    "Januar 2008"                           ; date
		    "RGB*"                                  ; image types
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-ADJUSTMENT _"Tiles X" '(4 1 32 1 10 0 1)
		    SF-ADJUSTMENT _"Tiles Y" '(4 1 32 1 10 0 1)
		    SF-OPTION _"Rotate (CW)" '("No" "90 degrees"
					       "180 degrees" "270 degrees"))

(script-fu-menu-register "script-fu-multi-burst"
                         _"<Image>/Filters/Animation")

