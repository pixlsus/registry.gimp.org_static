;script creates comic template. 
(define (Comic width height horiz vert border )
	;set image size 
	
	(let* (
			(page-width (+ (* (+ width border) horiz) border))
			(page-height (+ (* (+ height border) vert) border))
			(img (car (gimp-image-new page-width	page-height 0 )))
			(layer (car (gimp-layer-new img page-width page-height 0 "Comic" 100 0)))
			(flag 0)
			(flagv 0)
			)			
			
						
 (gimp-image-add-layer img layer 0)
 ;add alpha channel to layer
 (gimp-layer-add-alpha layer)
 ;set resolution to 300dpi for printing 
 
 (gimp-image-set-resolution img 300 300)

 (gimp-edit-clear layer)
  ;set forground color to white and fill background
 (gimp-context-set-foreground '(255 255 255))

 (gimp-drawable-fill layer  0)

;select rectangles  for panels
 (while (< flag horiz)
	(set! flagv 0)
  (while (< flagv vert)
  
 (gimp-rect-select img (+ border (* width flag) (* border flag)) (+ border (* height flagv) (* border flagv)) width height 0 0 0 )
		(set! flagv (+ flagv 1))
	)
; Increment the flag
		(set! flag (+ flag 1))
	)
 ;(gimp-rect-select img border border width height 0 0 0 )
 ;(gimp-rect-select img (+ border width border) border width height 0 0 0 )
 ;(gimp-rect-select img border (+ border height border) width height 0 0 0 )
 ;(gimp-rect-select img (+ border width border) (+ border height border) width height 0 0 0 )

 
 ;set forground colour to black and stroke the selection to outline the panels
 (gimp-context-set-foreground '(000 000 000))

  (gimp-edit-stroke layer)
 
 
  (gimp-edit-clear layer)

 (gimp-selection-none img)
 
 ;display image
 (gimp-display-new img)
 ))

(script-fu-register
    "Comic"
    "Comic"
    "Creates a comic template."
    "Geoff Griffiths"
    "2010, Geoff Griffiths"
    "August 4,2010"
    ""
	SF-VALUE "Panel width" "300" 
    SF-VALUE "Panel height" "300" 
	SF-VALUE "horizontal-panels" "2"
	SF-VALUE "vertical-panels" "2"
	SF-VALUE "border" "50"
	       
)
(script-fu-menu-register "Comic"
                         "<Image>/Filters/")