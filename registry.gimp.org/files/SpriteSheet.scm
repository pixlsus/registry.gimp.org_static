(define (create-sprite-sheet img drawable)
	(let * 
		(
			(anImage 0)
			(numlayers 0)
			(layers 0)
			(imgw 0)
			(idx 0)
			(layr 0)
			(xoff 0)
		)

		(set! anImage   (car (gimp-image-duplicate img)))
		(set! numlayers (car (gimp-image-get-layers anImage)))
		(set! layers    (cadr(gimp-image-get-layers anImage)))
		(set! imgw      (car (gimp-image-width anImage)))
		
		(while (< idx numlayers)
			(set! layr (aref layers (- (- numlayers 1) idx)))
			(gimp-layer-translate layr xoff 0)
			(set! xoff (+ imgw xoff))
			(set! idx (+ idx 1))
		)
		
		(gimp-image-resize-to-layers anImage)
		(gimp-image-merge-visible-layers anImage EXPAND-AS-NECESSARY)
		(gimp-display-new anImage)
	)
	
)		
		
(script-fu-register "create-sprite-sheet"
	_"<Toolbox>/Xtns/Sprite-Sheet/Create From Layers..."
    "Creates a new image from current image, then offsets each layer of new image, and finally merges all visible layers to create a spritesheet"
    "Brian.Schultheiss@airegear.com"
    "Brian.Schultheiss@airegear.com"
    "2009"
    "INDEXED* RGB* GRAY*"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
)