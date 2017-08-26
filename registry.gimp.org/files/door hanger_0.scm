;script creates a door hanger template. 
(define (door-hanger)
	;set image size to 100 pixels more than required hanger size.
	(let* ((img (car (gimp-image-new 1000 3100 0 )))
			(layer (car (gimp-layer-new img 1000 3100 0 "hanger" 100 0))))

 (gimp-image-add-layer img layer 0)
 ;add alpha channel to layer
 (gimp-layer-add-alpha layer)
 ;set resolution to 300dpi for printing 
 (gimp-image-set-resolution img 300 300)

 (gimp-edit-clear layer)
  ;set forground color to white and fill background
 (gimp-context-set-foreground '(255 255 255))

 (gimp-drawable-fill layer  0)

;select rounded rectangle for hanger 
 (gimp-round-rect-select img 50 50 900 3000 25 25 0 TRUE FALSE 0 0 )

 (gimp-edit-bucket-fill layer 0 0 100 0 FALSE 0 0)
 ;set forground colour to black and stroke the selection to outline the hanger
 (gimp-context-set-foreground '(000 000 000))

  (gimp-edit-stroke layer)
;set elipse for cutout  
 (gimp-ellipse-select img 140 140 720 720 2 TRUE FALSE 0 )
 
 (gimp-edit-stroke layer)
  (gimp-edit-clear layer)

 (gimp-selection-none img)
 
 ;autocrop image to hanger size.
 (plug-in-autocrop RUN-NONINTERACTIVE img layer)
 ;display image
 (gimp-display-new img)
 ))

(script-fu-register
    "door-hanger"
    "Door Hanger"
    "Creates a simple door hanger template."
    "Geoff Griffiths"
    "2010, Geoff Griffiths"
    "March 19,2010"
    ""
    SF-VALUE "width" "900" 
    SF-VALUE "Height" "3000" 
        
)
(script-fu-menu-register "door-hanger"
                         "<Image>/Filters/")