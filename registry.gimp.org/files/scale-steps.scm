(define (script-fu-scale-steps image drawable mwidth mheight stepsize flatten interpolation recursion)
	(gimp-image-undo-group-start image)
	
	(if (= flatten TRUE) 
		(set! drawable (car(gimp-image-flatten image)))
	)
	
	(let*(
		(sel_width 0)
		(sel_height 0)
		(new_width 0)
		(new_height 0)
		(new_x1 0)
		(new_x2 0)
		(new_y1 0)
		(new_y2 0)
	)
	
	(while 
		(or 
			(> (car (gimp-drawable-height drawable)) mheight)
			(> (car (gimp-drawable-width drawable)) mwidth)
		)
		(set! sel_width (car (gimp-drawable-width drawable)))
		(set! sel_height (car (gimp-drawable-height drawable)))
		(set! new_width (* sel_width (/ (- 100 stepsize) 100)))   
		(set! new_height (* sel_height (/ (- 100 stepsize) 100)))
		(if (< new_width mwidth)
		  (set! new_width mwidth)
		)
		(if (< new_height mheight)
		  (set! new_height mheight)
		)

		(set! new_x1 0)
		(set! new_y1 0)
		(set! new_x2 new_width)
		(set! new_y2 new_height)

        	(gimp-drawable-transform-scale
                                          drawable
					  new_x1
					  new_y1
					  new_x2
					  new_y2
                                          0 interpolation 1 recursion TRANSFORM-RESIZE-ADJUST
        	)
	)
	(if (= flatten TRUE) (plug-in-autocrop 0 image drawable))
	(gimp-image-undo-group-end image)
	(gimp-displays-flush)
)
)

(script-fu-register 
		"script-fu-scale-steps"
		"Scale in steps"
		"Scale in steps until width and height are smaller then the supplied values"
		"K.-M. Hansche"
		"(c) 2008 K.-M. Hansche"
		"2008-09-03"
		"RGB* GRAY*"
		SF-IMAGE "image" 0
		SF-DRAWABLE "drawable" 0
		SF-ADJUSTMENT "Max Width" '(900 10 9999 1 50 0 1)
		SF-ADJUSTMENT "Max Height" '(675 10 9999 1 50 0 1)
		SF-ADJUSTMENT "Stepsize in percent" '(10 1 20 1 2 0 1)
		SF-TOGGLE "Flatten Image" TRUE
		SF-ENUM "Interpolation (Scaling)" '("InterpolationType" "lanczos")
		SF-ADJUSTMENT "Recursion level (Scaling)" '(3 1 10 1 1 0 1)
)

(script-fu-menu-register "script-fu-scale-steps" "<Image>/Script-Fu")

