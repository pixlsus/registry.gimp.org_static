;; orignaly based on brian.schultheiss
;; modified by Sean Chapel (seoushi@gmail.com)

(define (create-sprite-sheet img drawable)
	(let * 
		(
			(anImage 0)
			(numlayers 0)
			(layers 0)
			(imgw 0)
			(imgh 0)
			(idx 0)
			(layr 0)
			(xoff 0)
			(yoff 0)
			(childIdx 0)
			(numChildern 0)
			(childern 0)
			(child 0)
		)

		(set! anImage   (car (gimp-image-duplicate img)))
		(set! numlayers (car (gimp-image-get-layers anImage)))
		(set! layers    (cadr(gimp-image-get-layers anImage)))
		(set! imgw      (car (gimp-image-width anImage)))
		(set! imgh      (car (gimp-image-height anImage)))
		
		(while (< idx numlayers)
			(set! layr (aref layers (- (- numlayers 1) idx)))

			(when (gimp-item-is-group layr)
			        (set! childIdx 0)
			        (set! numChildern (car (gimp-item-get-children layr)))
			        (set! childern (cadr (gimp-item-get-children layr)))

			        (while (< childIdx numChildern)
			        	(set! child (aref childern (- (- numChildern 1) childIdx)))
						(gimp-layer-translate child xoff yoff)
			          	(set! xoff (+ imgw xoff))
			          	(set! childIdx (+ 1 childIdx)))


					)

			(when (not (gimp-item-is-group layr))
				(gimp-layer-translate layr xoff yoff)
				(set! xoff (+ imgw xoff)))

			(set! idx (+ idx 1))
			(set! xoff 0)
			(set! yoff (+ imgh yoff)))
		
		(gimp-image-resize-to-layers anImage)
		(gimp-image-merge-visible-layers anImage EXPAND-AS-NECESSARY)
		(gimp-display-new anImage)
	)
	
)		
		
(script-fu-register "create-sprite-sheet"
	_"<Toolbox>/Xtns/Sprite-Sheet/Create From Layer Groups..."
    "Creates a new image from current image, then offsets each layer of new image, and finally merges all visible layers to create a spritesheet"
    "seoushi@gmail.com"
    "seoushi@gmail.com"
    "2012"
    "INDEXED* RGB* GRAY*"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
)