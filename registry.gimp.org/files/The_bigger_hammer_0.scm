(script-fu-register
    "bigger-hammer"																					;func name
    "The bigger hammer"																								;menu label
    "Implements Dan Margulis concept, The Bigger Hammer, from Modern Photoshop Color Workflow"	;description
    "Stig Junge"																							;author
    "copyright 2014, Stig Junge"										;copyright notice
    "February 16, 2014"																						;date created
    ""																										;image type that the script works on
    SF-IMAGE        "Input Image"       0
	SF-OPTION		"Channel to use" '("Red channel" "Green channel" "Blue channel" "RGB")
	SF-OPTION		"Mode" '("Overlay" "Soft light")
	SF-OPTION		"Protection" '("Dark areas" "Light areas")
	SF-ADJUSTMENT	"Radius" '(25 0 300 0 0 0 0)
)
 
 (script-fu-menu-register "bigger-hammer" "<Image>/Filters/Enhance")


(define (bigger-hammer inImage overlay mode protection radius)

   ;get specific layer in image
   (define (get-specified-layer theImage theLayerNumber)
        (let*
            (
                (theRequestedLayer 0)
            )
            (set! theRequestedLayer 
                (vector-ref
                    (cadr
                        (gimp-image-get-layers theImage)
                    )
                    theLayerNumber
                )
            )
            (list theRequestedLayer)
        )
    )
    
    ;make a copy of layer number X and get the reference
    (define (copy-specified-layer theImage theLayerNumber)
        (let*
            (
                (theRequestedLayer 0)
            )
            (set! theRequestedLayer 
                (car
                    ( gimp-layer-copy 
                        (car (get-specified-layer theImage theLayerNumber))
                        FALSE
                    )
                )
            )
            (list theRequestedLayer)
        )
    )

      
    (let*
        (
            (theImage inImage)		;The input image
            (theLayer 0)			;
            (RGB_Image 0)
            (theCurrentLayer 0)		;The layer currently in question	
			(theOriginalLayer 0)	;The original layer to copy from
            (theCopy 0)
            (theFloat 0)			;Floating selection after copy-paste
		)
		
		(gimp-context-push)
		(gimp-image-undo-group-start theImage)
		
		;Insert a greyscale copy of the red, green or blue channel - or the RGB-composite
		(if (= overlay 3)
			;Insert the RGB-composite as a new layer
			(begin
				;Get af copy of the original layer
				(set! theOriginalLayer (car (get-specified-layer theImage 0)))
				(set! theCurrentLayer (car (get-specified-layer theImage 0)))
				(gimp-edit-copy theOriginalLayer)
				(define theFloat 
					(car 
						(gimp-edit-paste theCurrentLayer 1)
					)
				)
				(gimp-floating-sel-to-layer theFloat)
				;Insert as a new layer
				(set! theCurrentLayer (car (get-specified-layer theImage 0)))
				(gimp-desaturate-full theCurrentLayer DESATURATE-LUMINOSITY)
			)
			;ELSE
			(begin
				;Get the original layer
				(set! theLayer (car (get-specified-layer theImage 0)))
				;Decompose to RGB
				(define RGB_Image 
					(car 
						(plug-in-decompose RUN-NONINTERACTIVE theImage theLayer "RGB" TRUE)
					)
				)
				;Copy red channel from RGB-image
				(if (= overlay 0)
					(begin
						(set! theCopy 
							(car 
								(gimp-edit-copy 
									(car
										(gimp-image-get-layer-by-name RGB_Image "red")
									)
								)
							)
						)
					)
					()
				)
				;Copy green channel from RGB-image
				(if (= overlay 1)
					(begin
						(set! theCopy 
							(car 
								(gimp-edit-copy 
									(car
										(gimp-image-get-layer-by-name RGB_Image "green")
									)
								)
							)
						)
					)
					()
				)
				;Copy blue channel from RGB-image
				(if (= overlay 2)
					(begin
						(set! theCopy 
							(car 
								(gimp-edit-copy 
									(car
										(gimp-image-get-layer-by-name RGB_Image "blue")
									)
								)
							)
						)
					)
					()
				)
				;Insert the copy
				(set! theFloat 
					(car 
						(gimp-edit-paste theLayer 1)
					)
				)
				;Convert floating layer to layer
				(gimp-floating-sel-to-layer theFloat)
			)
		)
		;Invert and blur the inserted layer
		(set! theCurrentLayer (car (get-specified-layer theImage 0)))
		(gimp-invert theCurrentLayer)
		(plug-in-gauss 1 theImage theCurrentLayer radius radius 1)
		(gimp-layer-set-opacity theCurrentLayer 50)
		
		(if (= mode 0)
			(begin
				;Copy the original to a new layer...
				;Get reference to original layer
				(set! theOriginalLayer (car (get-specified-layer theImage 1)))
				;Make a copy of the original layer
				(gimp-edit-copy theOriginalLayer)
				(set! theCurrentLayer (car (get-specified-layer theImage 0)))
				;Insert the copy
				(set! theFloat 
					(car 
						(gimp-edit-paste theCurrentLayer 1)
					)
				)
				;Convert floating layer to layer (hardlight mode layer for merge down with grayscale layer)
				(gimp-floating-sel-to-layer theFloat)
				;Get reference to topmost layer
				(set! theCurrentLayer (car (get-specified-layer theImage 0)))
				;Set layer mode to hard light
				(gimp-layer-set-mode theCurrentLayer HARDLIGHT-MODE)
				;Merge down
				(gimp-image-merge-down inImage theCurrentLayer 0)
			)
			(gimp-layer-set-mode theCurrentLayer SOFTLIGHT-MODE)
		)
		
		;Rename the topmost layer
		(set! theCurrentLayer (car (get-specified-layer theImage 0)))
		(gimp-item-set-name theCurrentLayer "Overlay")
		
		;Copy the original to a new layer (darken-only layer)
		(set! theLayer (car (get-specified-layer theImage 0)))
		(set! theCurrentLayer (car (get-specified-layer theImage 1)))
		(gimp-edit-copy theCurrentLayer)
		(define theFloat 
			(car 
				(gimp-edit-paste theLayer 1)
			)
		)
		(gimp-floating-sel-to-layer theFloat)

		(set! theCurrentLayer (car (get-specified-layer theImage 0)))
		(if (= protection 0)
				(begin
					(gimp-layer-set-mode theCurrentLayer DARKEN-ONLY-MODE)
					(gimp-item-set-name theCurrentLayer "Darken only")
				)
				(begin
					(gimp-layer-set-mode theCurrentLayer LIGHTEN-ONLY-MODE)
					(gimp-item-set-name theCurrentLayer "Lighten only")
				)
		)
		;Set opacity to 50%
		(gimp-layer-set-opacity theCurrentLayer 50)
			
		;Copy the original layer to the "Unblurred" layer...
		(set! theLayer (car (get-specified-layer theImage 0)))
		(set! theCurrentLayer (car (get-specified-layer theImage 2)))
		(gimp-edit-copy theCurrentLayer)
		(define theFloat 
			(car 
				(gimp-edit-paste theLayer 1)
			)
		)
		(gimp-floating-sel-to-layer theFloat)
		(set! theCurrentLayer (car (get-specified-layer theImage 0)))
		(gimp-item-set-name theCurrentLayer "Unblurred")
		(gimp-item-set-visible theCurrentLayer 0)

		
		(gimp-image-undo-group-end theImage)
		(gimp-context-pop)
	    (gimp-displays-flush)
    )
)