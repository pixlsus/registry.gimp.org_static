;===============================================================
;== GIMP Add Guides Vertically and Horizontally @ 50%         ==
;== Copyright (c) 2008 Gregory M. Ross                        ==
;===============================================================

(define (script-fu-crosshairs img drawable)
  
    (let* ((ImageW (car (gimp-drawable-width drawable))) 
	    (ImageH (car (gimp-drawable-height drawable))) 
	    (Vert_Offset  (car (gimp-drawable-offsets drawable)))
	    (Horz_Offset  (cadr (gimp-drawable-offsets drawable)))

	 
	    (Vert_50  (+ (* ImageW 0.5)Vert_Offset))
	    (Horz_50  (+ (* ImageH 0.5)Horz_Offset))
    )
   
    (gimp-image-undo-group-start img)
    (gimp-image-add-vguide img Vert_50)
    (gimp-image-add-hguide img Horz_50)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))


(script-fu-register "script-fu-crosshairs"
		    "<Image>/View/Crosshair Guides"
		    "Add Guides at 50% Horizontal and Vertical"
		    "Gregory M. Ross"
		    "Gregory M. Ross"
		    "March 2008"
		    ""
		    SF-IMAGE    "Image" 0
		    SF-DRAWABLE "Drawable" 0)
		    