;===============================================================
;== GIMP Add Guides Vertically and Horizontally @ 10% and 90% ==
;== Copyright (c) 2006 Gregory M. Ross                        ==
;===============================================================

(define (script-fu-10%-edge-guides img drawable)
  
    (let* ((ImageW (car (gimp-drawable-width drawable))) 
	    (ImageH (car (gimp-drawable-height drawable))) 
	    (Vert_Offset  (car (gimp-drawable-offsets drawable)))
	    (Horz_Offset  (cadr (gimp-drawable-offsets drawable)))

	 
	    (Vert_10  (+ (* ImageW 0.1)Vert_Offset))
	    (Horz_10  (+ (* ImageH 0.1)Horz_Offset))
	    (Vert_90  (+ (* ImageW 0.9)Vert_Offset))
	    (Horz_90  (+ (* ImageH 0.9)Horz_Offset))
    )
   
    (gimp-image-undo-group-start img)
    (gimp-image-add-vguide img Vert_10)
    (gimp-image-add-hguide img Horz_10)
    (gimp-image-add-vguide img Vert_90)
    (gimp-image-add-hguide img Horz_90)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))


(script-fu-register "script-fu-10%-edge-guides"
		    "<Image>/View/10% Edge Guides"
		    "Add Guides at 10% in on all sides"
		    "Gregory M. Ross"
		    "Gregory M. Ross"
		    "September 2006"
		    ""
		    SF-IMAGE    "Image" 0
		    SF-DRAWABLE "Drawable" 0)
		    