(define (load-sprite-sheet img drawable filename w)
	(let * 
		(
			(anImage 0)
			(numlayers 0)
			(layers 0)
			(imgh 0)
			(imgw 0)
			(idx 0)
			(baseLayer 0)
			(layr 0)
			(framew 0)
			(xoff 0)
		)

		(set! anImage   (car (file-png-load 1 filename filename)))

		(set! imgw      (car (gimp-image-width anImage)))
		(set! imgh      (car (gimp-image-height anImage)))
		(set! framew    imgh)
		(if (> w 0)                
			(set! framew w)
        )
		
		(set! numlayers (/ imgw framew))

		(set! layers    (cadr(gimp-image-get-layers anImage)))
		(set! baseLayer (aref layers 0))

		(while (< idx numlayers)
			(set! layr (car (gimp-layer-copy baseLayer TRUE)) )
            (gimp-image-insert-layer anImage layr 0 0 )
			(gimp-layer-translate layr xoff 0)
	        (set! xoff (- xoff framew))
			(set! idx (+ idx 1))
		)
        (gimp-image-crop anImage framew imgh 0 0)
        (gimp-context-set-foreground '(255 0 192) )
        (gimp-drawable-fill baseLayer 0)

        (gimp-display-new anImage)
	)
)		
		
(script-fu-register "load-sprite-sheet"
	_"<Toolbox>/Xtns/Sprite-Sheet/Load Spritesheet..."
    "Loads a spritesheet .png, cuts them into multiple frame layers, adds a background layer"
    "jaroslav.meloun@gmail.com"
    "jaroslav.meloun@gmail.com"
    "2011"
    "INDEXED* RGB* GRAY*"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
    SF-FILENAME     "Filename"              ""
	SF-VALUE        "FrameWidth"            "0"
)

(define (save-sprite-sheet img drawable filename)
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
            (if (> idx 0)                
                (begin
    			    (gimp-item-set-visible layr TRUE)
			        (set! xoff (+ imgw xoff))
                )
	    		(gimp-item-set-visible layr FALSE)
            )
			(set! idx (+ idx 1))
		)
		
		(gimp-image-resize-to-layers anImage)
		(gimp-image-merge-visible-layers anImage CLIP-TO-IMAGE)
        (file-png-save 0 anImage drawable filename filename 0 9 0 0 0 1 1)
	)
)		
		
(script-fu-register "save-sprite-sheet"
	_"<Toolbox>/Xtns/Sprite-Sheet/Save Spritesheet..."
    "Aligns frame layers in line and save the result as a single image"
    "jaroslav.meloun@gmail.com"
    "jaroslav.meloun@gmail.com"
    "2011"
    "INDEXED* RGB* GRAY*"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
    SF-FILENAME     "Filename"              ""
)

(define (show-step-frame anImage drawable step)
	(let * 
		(
			(numlayers 0)
			(layers 0)
			(imgw 0)
			(idx 0)
			(baseLayer 0)
			(activeLayer 0)
			(activeLayerIndex 0)
			(layr 0)
			(xoff 0)
		)

		(set! numlayers (car (gimp-image-get-layers anImage)))
		(set! layers    (cadr(gimp-image-get-layers anImage)))
		(set! baseLayer (aref layers (- numlayers 1)))
		(set! activeLayer (car (gimp-image-get-active-layer anImage)))

		(while (< idx numlayers)
			(set! layr (aref layers idx))
            (gimp-item-set-visible layr FALSE)
            (if (= layr activeLayer)
                (set! activeLayerIndex idx)
            )
			(set! idx (+ idx 1))
		)
        (set! activeLayerIndex (+ activeLayerIndex step))
        (if (< activeLayerIndex 0)
            (set! activeLayerIndex 0)
        ) 
        (if (> activeLayerIndex (- numlayers 1))
            (set! activeLayerIndex (- numlayers 1))
        ) 
        (gimp-item-set-visible (aref layers activeLayerIndex) TRUE)
        (gimp-item-set-visible baseLayer TRUE)
        (gimp-image-set-active-layer anImage (aref layers activeLayerIndex))
        (gimp-displays-flush)
	)
)

(define (show-next-frame anImage drawable)
	(let * 
        ()
	    (show-step-frame anImage drawable -1)	
	)
	
)		
(script-fu-register "show-next-frame"
	_"<Toolbox>/Xtns/Sprite-Sheet/Next frame"
    "Shows next frame layer and background, hides other layers"
    "jaroslav.meloun@gmail.com"
    "jaroslav.meloun@gmail.com"
    "2011"
    "INDEXED* RGB* GRAY*"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
)


(define (show-prev-frame anImage drawable)
	(let * 
        ()
	    (show-step-frame anImage drawable 1)	
	)
	
)		
(script-fu-register "show-prev-frame"
	_"<Toolbox>/Xtns/Sprite-Sheet/Prev frame"
    "Shows previous frame layer and background, hides other layers"
    "jaroslav.meloun@gmail.com"
    "jaroslav.meloun@gmail.com"
    "2011"
    "INDEXED* RGB* GRAY*"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
)

