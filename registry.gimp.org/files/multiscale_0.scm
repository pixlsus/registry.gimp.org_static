(define (script-fu-multiscale image drawable size doheight stepsize flatten interpolation recursion)
	(gimp-image-undo-group-start image)
	
	(if (= flatten TRUE) 
		(set! drawable (car(gimp-image-flatten image)))
  )
	
	(let*(
    (pri_size 0)
    (sec_size 0)
		(new_x1 0)
		(new_x2 0)
		(new_y1 0)
		(new_y2 0)
    (aspect 0)
	)

  (set! pri_size (if (= doheight 0) (car (gimp-drawable-width drawable)) (car (gimp-drawable-height drawable))))
  (set! sec_size (if (= doheight 0) (car (gimp-drawable-height drawable)) (car (gimp-drawable-width drawable))))
  (set! aspect (/ pri_size sec_size))
	
	(while (> pri_size size)

    (set! pri_size (- pri_size stepsize))
    (if (> size pri_size) (set! pri_size size))
    (set! sec_size (/ pri_size aspect))

		(set! new_x1 0)
		(set! new_y1 0)
		(set! new_x2 (if (= doheight 0) pri_size sec_size))
		(set! new_y2 (if (= doheight 0) sec_size pri_size))

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
		"script-fu-multiscale"
		"Multiscale"
		"Scale an image using multiple intermediate steps \
     until a certain width or height is reached \
     while retaining original aspect ratio."
		"Jan Dohl <polygon@wh2.tu-dresden.de>"
		"Jan Dohl"
		"January 2011"
		"RGB* GRAY*"
		SF-IMAGE "image" 0
		SF-DRAWABLE "drawable" 0
		SF-ADJUSTMENT "Size" '(1200 10 9999 10 100 0 1)
    SF-OPTION "Dimension" '("Width" "Height")
		SF-ADJUSTMENT "Stepsize" '(500 10 1000 10 100 0 1)
		SF-TOGGLE "Flatten Image" TRUE
		SF-ENUM "Interpolation (Scaling)" '("InterpolationType" "lanczos")
    SF-ADJUSTMENT "Recursion level (Scaling)" '(3 1 10 1 1 0 1)
)

(script-fu-menu-register "script-fu-multiscale" "<Image>/Script-Fu")

