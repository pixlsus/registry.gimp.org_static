;
; Generate diagonal grids, a Gimp script by Roy Johnson
;
; Create a new layer, generate a grid on it, and rotate it by 45 degrees
; This provides guide lines for cropping. Pick a point of interest and move
; the grid so that a diagonal goes through it. Then when you crop, make sure
; that diagonal goes through a corner of the crop window. Then you can move
; the grid around to find another point and crop the opposite corner (don't
; change the corner you already cropped to).

(define	(script-fu-diagonal-grid	theImage
					theDrawable
	)
    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    (let* ( (max-image-dim (max (car (gimp-image-width theImage))
	   			(car (gimp-image-height theImage))))
	    (width (* 1.42 max-image-dim))
	    (grid-layer	(car (gimp-layer-new theImage width width
			  (car (gimp-drawable-type theDrawable))
			  "Diagonal Grid" 100 DIFFERENCE-MODE)))
	    (line_w 2)
	    (spacing (/ width 5))
	    (offset 0)
	    (line_color '(180 180 180))
	  )
	  (gimp-image-add-layer theImage grid-layer -1)
	  (gimp-layer-set-offsets grid-layer
		(/ (- max-image-dim width) 2)
		(/ (- max-image-dim width) 2) )
	  (plug-in-grid 1 theImage grid-layer
		line_w (/ width 5) offset line_color 255
		line_w (/ width 5) offset line_color 255
		line_w spacing offset line_color 255)
	  (gimp-drawable-transform-rotate-default grid-layer (/ 3.14159 4)
	   TRUE (/ width 2) (/ width 2)  FALSE 0)
    )
    (gimp-displays-flush)

    ;End the undo group
    (gimp-image-undo-group-end theImage)
)

(script-fu-register "script-fu-diagonal-grid"
            _"Diagonal Grid"
            "Grid at 45 degree angles to help with cropping"
            "Roy Johnson"
            "2009, Roy Johnson"
            "July 2009"
            ""
            SF-IMAGE		"Image"		0
	    SF-DRAWABLE		"Drawable"	0
)

(script-fu-menu-register "script-fu-diagonal-grid"
             "<Image>/FX-Foundry/Toolbox/Grids/")
