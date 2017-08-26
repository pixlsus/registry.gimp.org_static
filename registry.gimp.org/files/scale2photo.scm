(define 
  (scale2photo image layer YLENGTH XLENGTH)

  ;; make faster
  (gimp-image-undo-disable image)


  (let* 
      (
       ;; constants
       (XOFFSET 0)
       (YOFFSET 0)
       (WHITE '(255 255 255))

       ;; get image height and width
       (height (car (gimp-drawable-height layer)))
       (width (car (gimp-drawable-width layer)))

  
       ;; calculate both pixels/inch, use the max
       (PPILONG (/ (max height width) (max XLENGTH YLENGTH) ))
       (PPISHORT (/ (min height width) (min XLENGTH YLENGTH) ))
       (RESOLUTION (max PPILONG PPISHORT))
       
       ;; calculate canvas sizes
       (LONGPADDEDSIDE (* (max XLENGTH YLENGTH) RESOLUTION))	; pixels - short side
       (SHORTPADDEDSIDE (* (min XLENGTH YLENGTH) RESOLUTION))	; pixels - short side
       

       (newWidth 0)
       (newHeight 0)

       )
    
    ;; need to pad both sides (based on 8"X11") (is this 8"X10" or 10"X8"?)
    (cond ( (> width height) (begin
			       ;; set canvas size vars
			       (set! newWidth LONGPADDEDSIDE)
			       (set! newHeight SHORTPADDEDSIDE) 
			       ))
	  ( (> height width) (begin
			       ;; set canvas size vars
			       (set! newWidth SHORTPADDEDSIDE)
			       (set! newHeight LONGPADDEDSIDE) 
			       ))
	  )

    ;; change canvas size
    (gimp-image-resize image
		       newWidth
		       newHeight
		       XOFFSET
		       YOFFSET)
    
    
    ;; make sure background is white
    (gimp-palette-set-background WHITE)

    ;; change layer size
    (gimp-layer-resize layer
		       newWidth
		       newHeight
		       XOFFSET
		       YOFFSET)

    ;; change print size to 8"X10" 
    (gimp-image-set-resolution image RESOLUTION RESOLUTION)

    )

  ;; cleanup
  (gimp-image-undo-enable image)
  (gimp-displays-flush)

)


; register function w/gimp
(script-fu-register "scale2photo"
		    _"<Image>/Filters/Mine/scale2photo"
		    "Scales image for photo printing, such as a 5X7\", 8X10\", etc."
		    "Elliot Nathanson"
		    "2008"
		    "20080205"
		    ""
		    SF-IMAGE "The Image" 0
		    SF-DRAWABLE "The Layer" 0
		    SF-VALUE   _"Height"        "8"
		    SF-VALUE   _"Width"        "10"
		    )
