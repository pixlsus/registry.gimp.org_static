; Muted Color Effect v1
; Created by Patrick David <patdavid@gmail.com>
;
; Mutes the colors of an image using 1 of 3 methods:
; 1. Desaturated Layer over color with Opacity
; 2. Luminosity Layers with different blending modes (Multiply & Screen)
; 3. Solid-fill layers (black and white) with Layer masks applied (from image Luminosity)
;

(define (script-fu-patdavid-mute Image Drawable Method)

	(if (= Method 0)
		(let* 
			( 	; define variables
				(DarkLayer (car (gimp-layer-new-from-drawable Drawable Image)) )
				(LightLayer (car (gimp-layer-new-from-drawable Drawable Image)) )
			)
				(gimp-image-undo-group-start Image)
	
				(gimp-drawable-set-name LightLayer "Screen Layer")
				(gimp-image-add-layer Image LightLayer -1)
				(gimp-desaturate-full LightLayer 1)
				(gimp-layer-set-mode LightLayer 4)
	
				(gimp-drawable-set-name DarkLayer "Multiply Layer")
				(gimp-image-add-layer Image DarkLayer -1)
				(gimp-desaturate-full DarkLayer 1)
				(gimp-layer-set-mode DarkLayer 3)
				
				(gimp-displays-flush)
		
				(gimp-image-undo-group-end Image)
		)
	
		(if (= Method 1)
	
			(let*
				( ; define variables
					(DarkLayer (car (gimp-layer-new-from-drawable Drawable Image)) )
					(LightLayer (car (gimp-layer-new-from-drawable Drawable Image)) )
					(LumMask (car (gimp-layer-new-from-drawable Drawable Image)) )
				)
					(gimp-image-undo-group-start Image)
					
					(gimp-image-add-layer Image LumMask -1) ; add layer copy
					(gimp-desaturate-full LumMask 1) ; desaturate it
					(gimp-edit-copy LumMask) ; copy into buffer
					(gimp-image-remove-layer Image LumMask) ; remove layer from image
		
					(gimp-drawable-set-name LightLayer "Light Layer")
					(gimp-drawable-fill LightLayer 2) ; fill with white
					(gimp-image-add-layer Image LightLayer -1) ; add to image
					(gimp-layer-add-mask LightLayer (car(gimp-layer-create-mask LightLayer 0)) ) ; add a mask
					; below - paste the copied luminosity layer and anchor to lightlayer mask
					(gimp-floating-sel-anchor (car(gimp-edit-paste (car(gimp-layer-get-mask LightLayer)) FALSE)) )
		
					(gimp-context-set-foreground '(0 0 0) )
					(gimp-drawable-set-name DarkLayer "Dark Layer")
					(gimp-drawable-fill DarkLayer 0)
					(gimp-image-add-layer Image DarkLayer -1)
					(gimp-layer-add-mask DarkLayer (car(gimp-layer-create-mask DarkLayer 0)) )
					(gimp-floating-sel-anchor ( car( gimp-edit-paste ( car(gimp-layer-get-mask DarkLayer)) FALSE)))
					(gimp-invert (car(gimp-layer-get-mask DarkLayer)) )
					
		
					(gimp-displays-flush)
					(gimp-image-undo-group-end Image)
			)

			(let*
				( ; define variables
					(LumLayer (car (gimp-layer-new-from-drawable Drawable Image)))
				)
					(gimp-image-undo-group-start Image)
					
					(gimp-drawable-set-name LumLayer "Luminosity")
					(gimp-image-add-layer Image LumLayer 0)
					(gimp-desaturate-full LumLayer 1)
					(gimp-layer-set-opacity LumLayer 70)

					(gimp-displays-flush)
					(gimp-image-undo-group-end Image)
			)
		)
	)
)

; Finally register our script with script-fu.
(script-fu-register "script-fu-patdavid-mute"
                    "Mute Colors..."
                    "Mutes Colors using 1 of 2 methods (luminosity blend layers or solid fill layers with luminosity masks)."
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2011-06-09"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
			SF-OPTION "Mute Method" '("Luminosity Layers" "Solid Layers with Masks" "Desaturated Overlay" )
)

(script-fu-menu-register "script-fu-patdavid-mute" "<Image>/Colors")
