;Define the main function
(define (script-fu-film-like-grain img drawable strength sharpness)
         (gimp-image-undo-group-start img) ;Start the undo group (allow all actions to be undone as one)
         (gimp-context-set-foreground '(128 128 128)) ;Set the foreground color to middle gray.
	 (let* (
		;Declare the film grain layer and it's mask.
		(film-grain-layer (car (gimp-layer-new img (car (gimp-image-width img)) (car (gimp-image-height img)) 0 "Film Grain" 100 0 )))
		(film-grain-layer-mask (car (gimp-layer-create-mask film-grain-layer 0)))
	       )
	   (gimp-drawable-fill film-grain-layer 0) ;Fill the film grain layer with the foreground layer.
	   (gimp-image-add-layer img film-grain-layer 0) ;Add the film grain layer to the image.
	   (plug-in-scatter-hsv 1 img film-grain-layer 2 0 0 strength) ;Create the noise using the Scatter HSV method, using the user's strength.
	   (gimp-layer-set-mode film-grain-layer 5) ;Set the mode of the film grain layer to Overlay.
	   (plug-in-gauss-iir 1 img film-grain-layer sharpness sharpness sharpness) ;Blur the noise a tad to give a realistic effect.
	   (gimp-layer-add-mask film-grain-layer film-grain-layer-mask) ;Add the mask to the layer with full opacity.
	   
	   
	 )
	 (gimp-context-set-foreground '(0 0 0)) ;Set the foreground color to black.
	 (gimp-image-undo-group-end img) ;End the undo group.
)


;Register the script w/ GIMP.
(script-fu-register
          "script-fu-film-like-grain"                        ;func name
          "Film Grain"                                       ;menu label
          "Adds film grain to an image. Use a greater strength for larger images." ;description
          "Conor Dube"                                ;author
          "Copyright 2008, Conor Dube"                ;copyright notice
          "Oct. 2008"                                 ;date created
          ""                                          ;image type that the script works on

          SF-IMAGE    "Image"         0
          SF-DRAWABLE "Layer to convert" 0
	  SF-ADJUSTMENT "Grain Strength" '(100 0 255 5 10 1 0)
	  SF-ADJUSTMENT "Grain Sharpness" '(1 0 10 .1 .5 2 0)
)
        
(script-fu-menu-register "script-fu-film-like-grain" "<Image>/Script-Fu")