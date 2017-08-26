;; -*-scheme-*- 

(define (script-fu-half-tile-offset inImage
					 inLayer) 
					 
	(set! theImage inImage)
	(set! theLayer inLayer)
	(set! theHeight (car (gimp-drawable-height theLayer)))
	(set! theWidth (car (gimp-drawable-width theLayer)))
	
	(set! AllLayers (gimp-image-get-layers theImage))
	(set! num-layers (car AllLayers))
	(set! layer-array (cadr AllLayers))
	
	
	(set! theHeight (/ theHeight 2))
	(set! theWidth (/ theWidth 2))
	
	(gimp-image-undo-group-start theImage)
	
	
	(set! layer-count 0)
	(while (< layer-count num-layers)
      (set! layer (aref layer-array layer-count))
      ;(gimp-layer-set-offsets layer theWidth theHeight)
	  (gimp-drawable-offset layer 1 1 theWidth theHeight)
      (set! layer-count (+ layer-count 1))
	 )

	
	
  
  
	(gimp-image-undo-group-end theImage)
	(gimp-displays-flush)
  
  )

(script-fu-register "script-fu-half-tile-offset" 
		    _"Half Tile _Offset"
		    "Offsets all layers by half, when making seamless textures. Expected cases are: all layers have the same size, i.e 1024x1024"
		    "Ilian 'Ultrano' Dinev"
		    "2010, Ilian Dinev"
		    "24th February 2010"
		    "RGB*"
			
		    SF-IMAGE    "Image"    0 
		    SF-DRAWABLE "Drawable" 0)

(script-fu-menu-register "script-fu-half-tile-offset" 
			 "<Image>/Image")
