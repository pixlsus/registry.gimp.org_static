;===================================================================
;== GIMP Add Guides Vertically and Horizontally in 10% increments ==
;==              Copyright (c) 2006 Gregory M. Ross               ==
;===================================================================

(define (script-fu-10%-grid-guides img drawable)
  
    (let* ((ImageW (car (gimp-drawable-width drawable)))
	    (ImageH (car (gimp-drawable-height drawable)))
	    (Vert_Offset  (car (gimp-drawable-offsets drawable)))
	    (Horz_Offset  (cadr (gimp-drawable-offsets drawable)))

	    (Vert_0  (+ (* ImageW 0.0)Vert_Offset))
	    (Horz_0  (+ (* ImageH 0.0)Horz_Offset))
	    (Vert_1  (+ (* ImageW 0.1)Vert_Offset))
	    (Horz_1  (+ (* ImageH 0.1)Horz_Offset))	 
	    (Vert_2  (+ (* ImageW 0.2)Vert_Offset))
	    (Horz_2  (+ (* ImageH 0.2)Horz_Offset))	 
	    (Vert_3  (+ (* ImageW 0.3)Vert_Offset))
	    (Horz_3  (+ (* ImageH 0.3)Horz_Offset))	 
	    (Vert_4  (+ (* ImageW 0.4)Vert_Offset))
	    (Horz_4  (+ (* ImageH 0.4)Horz_Offset))	 
	    (Vert_5  (+ (* ImageW 0.5)Vert_Offset))
	    (Horz_5  (+ (* ImageH 0.5)Horz_Offset))	 
	    (Vert_6  (+ (* ImageW 0.6)Vert_Offset))
	    (Horz_6  (+ (* ImageH 0.6)Horz_Offset))	 
	    (Vert_7  (+ (* ImageW 0.7)Vert_Offset))
	    (Horz_7  (+ (* ImageH 0.7)Horz_Offset))	 
	    (Vert_8  (+ (* ImageW 0.8)Vert_Offset))
	    (Horz_8  (+ (* ImageH 0.8)Horz_Offset))	 
	    (Vert_9  (+ (* ImageW 0.9)Vert_Offset))
	    (Horz_9  (+ (* ImageH 0.9)Horz_Offset))
	    (Vert_A  (+ (* ImageW 1.0)Vert_Offset))
	    (Horz_A  (+ (* ImageH 1.0)Horz_Offset))
    )
   
    (gimp-image-undo-group-start img)
    (gimp-image-add-vguide img Vert_0)
    (gimp-image-add-hguide img Horz_0)
    (gimp-image-add-vguide img Vert_1)
    (gimp-image-add-hguide img Horz_1)
    (gimp-image-add-vguide img Vert_2)
    (gimp-image-add-hguide img Horz_2)
    (gimp-image-add-vguide img Vert_3)
    (gimp-image-add-hguide img Horz_3)
    (gimp-image-add-vguide img Vert_4)
    (gimp-image-add-hguide img Horz_4)
    (gimp-image-add-vguide img Vert_5)
    (gimp-image-add-hguide img Horz_5)
    (gimp-image-add-vguide img Vert_6)
    (gimp-image-add-hguide img Horz_6)
    (gimp-image-add-vguide img Vert_7)
    (gimp-image-add-hguide img Horz_7)
    (gimp-image-add-vguide img Vert_8)
    (gimp-image-add-hguide img Horz_8)
    (gimp-image-add-vguide img Vert_9)
    (gimp-image-add-hguide img Horz_9)
    (gimp-image-add-vguide img Vert_A)
    (gimp-image-add-hguide img Horz_A)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))


(script-fu-register "script-fu-10%-grid-guides"
		    "<Image>/View/10% Grid Guides"
		    "Add Guides at 10% increments Vertically and Horizontally"
		    "Gregory M. Ross"
		    "Gregory M. Ross"
		    "September 2006"
		    ""
		    SF-IMAGE    "Image" 0
		    SF-DRAWABLE "Drawable" 0)
		    