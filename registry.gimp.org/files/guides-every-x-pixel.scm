; -*-scheme-*-
; Nicholas DeClario 2009.  No copyright.  Public Domain.
; Script based on rule-of-thirds.scm by Guillermo Maldonado

(define (script-fu-guide-rot image
			     drawable
			     direction
			     size)
    (let* (
		(width (car (gimp-image-width image)))
		(height (car (gimp-image-height image)))
		(position 0)
		(imgsize width)
          )
    (if (= direction 0) (set! imgsize height))
    (gimp-image-undo-group-start image)

    (while (< (+ position size) imgsize)
	    (set! position (+ size position))
	    (if (= direction 0)
		(gimp-image-add-hguide image position))
	    (if (= direction 1)
		    (gimp-image-add-vguide image position))
    )

    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-guide-rot"
  _"New Guides Every X Pixels"
  "Adds vertical or horizontal guides every X pixels"
  "Nicholas DeClario"
  "Nicholas DeClario, 2009"
  "January 2009"
  ""
  SF-IMAGE      "Input Image"      0
  SF-DRAWABLE   "Input Drawable"   0
  SF-OPTION     _"Direction"       '(_"Horizontal" _"Vertical")
  SF-ADJUSTMENT _"Size (in pixels)" '(50 0 10000 1 10 0 1)
)

(script-fu-menu-register "script-fu-guide-rot"
                         "<Image>/Image/Guides")
