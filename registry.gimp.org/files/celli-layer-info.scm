;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Display active layer info  for GIMP 2.6
; Created by Salvatore Celli
;
; Tags: tool, layer, width, height, x, y, info
;

(define (script-fu-layer-informations image drawable)
	(let* 
		(
			(layerName (car (gimp-drawable-get-name drawable)))
			(layerWidth (car (gimp-drawable-width drawable))) 
			(layerHeight (car (gimp-drawable-height drawable)))
			(layerOffsets (gimp-drawable-offsets drawable))
			(layerOffsetsX (car layerOffsets))
			(layerOffsetsY (cadr layerOffsets))
		)	
			 
		(begin
			(gimp-message-set-handler 0)
			(gimp-message 
				(string-append 
					"Name: " layerName "\n" 
					"X: " (number->string layerOffsetsX) "\n" 
					"Y: " (number->string layerOffsetsY) "\n" 
					"Width: " (number->string layerWidth) "\n" 
					"Height: " (number->string layerHeight)
				) 
			)
		)
	)
)

(script-fu-register "script-fu-layer-informations" 
	"<Image>/Layer/Layer In_formation"
    "Displays bounding box (left,top,width,height) of current layer."
	"Salvatore Celli <sal.celli@yahoo.com>"
	"(c) 2009, Salvatore Celli"
	"August, 28 2009"
	"*"
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
 )
