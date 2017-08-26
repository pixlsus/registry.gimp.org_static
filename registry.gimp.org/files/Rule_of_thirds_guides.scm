;===============================================================
;== GIMP Add Guides for Rule of Thirds                        ==
;== Copyright (c) 2006 Gregory M. Ross                        ==
;===============================================================

(define (script-fu-Rule-of-Thirds-guides img drawable)
  
    (let* ((ImageW (car (gimp-drawable-width drawable))) 
	    (ImageH (car (gimp-drawable-height drawable))) 
	    (Vert_Offset  (car (gimp-drawable-offsets drawable)))
	    (Horz_Offset  (cadr (gimp-drawable-offsets drawable)))

	 
	    (Vert_1  (+ (/ ImageW 3)Vert_Offset))
	    (Horz_1  (+ (/ ImageH 3)Horz_Offset))
	    (Vert_2  (+ (* (/ ImageW 3) 2)Vert_Offset))
	    (Horz_2  (+ (* (/ ImageH 3) 2)Horz_Offset))
    )
   
    (gimp-image-undo-group-start img)
    (gimp-image-add-vguide img Vert_1)
    (gimp-image-add-hguide img Horz_1)
    (gimp-image-add-vguide img Vert_2)
    (gimp-image-add-hguide img Horz_2)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))


(script-fu-register "script-fu-Rule-of-Thirds-guides"
		    "<Image>/View/Rule of Thirds Guides"
		    "Add Guides following the Rule of Thirds"
		    "Gregory M. Ross"
		    "Gregory M. Ross"
		    "September 2006"
		    ""
		    SF-IMAGE    "Image" 0
		    SF-DRAWABLE "Drawable" 0)
		    