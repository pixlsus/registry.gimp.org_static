;	Script to copy and image and translate all layers down to form a vertical filmstrip.
;	This version is for GIMP 2.8+
;	Copyright 2013, Jonathan Thomas
;	Despite the copyright, you are free to alter and distribute this as much as you want, as long as I am credited.

(define (scirpt-fu-stack-layers inImage flowingLiquid)
	(if (= flowingLiquid FALSE)
	(let*
		(
			;define local variables
			(index 1)	;utility variable
		
			;get information about the image to be converted
			(theNumberOfLayers (car (gimp-image-get-layers inImage)))
			(theOldImageHeight (car (gimp-image-height inImage)))

			;set some values for use later
			(theImageHeight (* theNumberOfLayers (car (gimp-image-height inImage))))	;value for the new image height
																						;old image height * number of layers
			(theImageWidth 	(car (gimp-image-width inImage)))	;value for the new image width
			(theImage (car (gimp-image-duplicate inImage)))		;duplicate the old image
			(layer )	;the layer we want to work with at any given time
			(listOfLayers '())	;a list of all the layers in the new image (we'll fill it later)


		
			
		);end of local variables			


		
		;resize the image to the right dimensions
		(gimp-image-resize theImage theImageWidth theImageHeight 0 0)
		
		;move the first layer to the bottom. trust me, it's necessary
		(gimp-image-lower-item-to-bottom theImage (car (vector->list (cadr (gimp-image-get-layers theImage)))))
		
		;translate each layer a certain distance based on its position in the original image
		(while (< index theNumberOfLayers)	;while the utility var is less than number of layers
			
			(set! listOfLayers (vector->list (cadr (gimp-image-get-layers theImage))))	;set layers to a list of all the layers in theImage
			
			(set! layer (car listOfLayers))
			
			(gimp-item-set-visible layer TRUE)
	
			(gimp-layer-translate layer 0 (* index theOldImageHeight))
	
			(gimp-image-lower-item-to-bottom theImage layer)	;lowers the layer to the bottom. we're done with it

			(set! index (+ index 1))	;increment the variable (VITAL! as I have learned <_< )

		)
	
		;package up the image and finish
		(gimp-image-merge-visible-layers theImage EXPAND-AS-NECESSARY)
		(gimp-display-new theImage)
		(gimp-image-clean-all theImage)	
	)
	)
	
	(if (= flowingLiquid TRUE)
	(let*
		(
			;define local variables
			(index 1)	;utility variable
			;(index2 0)	;another utility variable (don't know how to use for loops in this language)
		
			;get information about the image to be converted
			(theNumberOfLayers (car (gimp-image-get-layers inImage)))
			(theOldImageHeight (car (gimp-image-height inImage)))

			;set some values for use later
			(theImageHeight (* 2 (* theNumberOfLayers (car (gimp-image-height inImage)))))	;value for the new image height
																						;old image height * number of layers
			(theImageWidth 	(* 2 (car (gimp-image-width inImage))))	;value for the new image width
			(theImage (car (gimp-image-duplicate inImage)))		;duplicate the old image
			(layer )	;the layer we want to work with at any given time
			(listOfLayers '())	;a list of all the layers in the new image (we'll fill it later)


		
			
		);end of local variables			


		
		;resize the image to the right dimensions
		(gimp-image-resize theImage theImageWidth theImageHeight 0 0)
		
		(set! layer (car (vector->list (cadr (gimp-image-get-layers theImage)))))
		
		(plug-in-tile RUN-NONINTERACTIVE theImage layer theImageWidth theImageWidth FALSE)
		
		;move the first layer to the bottom. trust me, it's necessary
		(gimp-image-lower-item-to-bottom theImage (car (vector->list (cadr (gimp-image-get-layers theImage)))))
		
		;translate each layer a certain distance based on its position in the original image
		(while (< index theNumberOfLayers)	;while the utility var is less than number of layers
			
			(set! listOfLayers (vector->list (cadr (gimp-image-get-layers theImage))))	;set layers to a list of all the layers in theImage
			
			(set! layer (car listOfLayers))
			
			(gimp-item-set-visible layer TRUE)
			
			(plug-in-tile RUN-NONINTERACTIVE theImage layer theImageWidth theImageWidth FALSE)
	
			(gimp-layer-translate layer 0 (* index theOldImageHeight 2))
			
			(gimp-image-lower-item-to-bottom theImage layer)	;lowers the layer to the bottom. we're done with it

			(set! index (+ index 1))	;increment the variable (VITAL! as I have learned <_< )

		)
	
		;package up the image and finish
		(gimp-image-resize-to-layers theImage)
		(gimp-image-merge-visible-layers theImage EXPAND-AS-NECESSARY)
		(gimp-display-new theImage)
		(gimp-image-clean-all theImage)	
	)
	)
)



(script-fu-register
    "scirpt-fu-stack-layers"              		;func name
    "Minecraft Animation"        				;menu label
    "Takes all visible layers and converts them to a Minecraft animation"      ;description
    "Jonathan Thomas"							;author
    "copyright 2013, Jonathan Thomas;"  		;copyright notice
    "April 05, 2013"           					;date created
    ""                     						;image type that the script works on
	SF-IMAGE		"Image"		0				;working image
	SF-TOGGLE		"Flowing Liquid"	FALSE	;is it a flowing liquid
  )
  (script-fu-menu-register "scirpt-fu-stack-layers" "<Image>/File/Create")