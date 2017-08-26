;=========================================================================================================================
;Script Name:  Double HSV Film Grain
;Version Number: v.0.9.6
;Author: VangaurdVagabond
;Copyright: 2010 VangaurdVagabond
;=========================================================================================================================
;
;Version Notes:
;
;v.0.9.6: Bug fix, now works with multiple layered images
;
;v.0.9.5: Beta Version: Can include or remove Leveled Layer.  Still does not work with Grayscale, but will be included
;	  later if possible.
;
;v.0.9: Beta Version: Boolean operation can save layers or combine all, still needs some stablization and possible
;       features.  Alpha vestigial code removed.
;
;v.0.4: Alpha test: Code reworked
;
;v.0.3.1: Alpha test: Code stablized
;
;v.0.3: Alpha test: Drawable adjustment removed, most code in place, highly unstable
;
;v.0.2: Alpha test: Drawable adjustment added
;
;v.0.1: Alpha test to set up registration and input values
;
;=========================================================================================================================

(define (gs-layer-transfer
		ltlay1
		ltlay2
	)
	
	(let* 	(
			(ltver (car (gimp-edit-copy ltlay1) ) )
			(ltfloat nil)
		)
		(set! ltfloat (car (gimp-edit-paste ltlay2 TRUE) ) )
		(gimp-floating-sel-anchor floater)
		(set! ltfloat nil)
	)
)

(define (script-fu-film-grain 
		;paramereters
		image						;Image input value
		inGrain						;Grain input in x/255 format
		inLowOut					;Low Output input
		inHighOut					;High Output Input
		inPreserve					;Boolean level preserver
		inLeveled					;Boolean leveled create
	
	)

	
	(let*  	(						;Declare Variables
			(grainValue (* inGrain 12.75) )
			(srclayer (car (gimp-image-get-active-drawable image) ) )
			(imgx (car (gimp-image-width image) ) )
			(imgy (car (gimp-image-height image) ) )
			(leveled (car (gimp-layer-new image imgx imgy 1 "leveled" 100 0) ) )
			(pattern (car (gimp-layer-new image imgx imgy 1 "pattern" 100 5) ) )
			(grain (car (gimp-layer-new image imgx imgy 1 "grain" 100 5) ) )
			(srcpos (car (gimp-image-get-layer-position image srclayer) ) )				
			(origFore (car (gimp-context-get-foreground ) ) )
			(copyver (car (gimp-edit-copy srclayer) ) )
			(floater nil)
		)
	
	(gimp-context-push)
	(gimp-image-undo-group-start image)			;Begin Undo Group
	(gimp-progress-set-text "Film Grain")
	
	
								;Add layers
	(gimp-image-add-layer image leveled -1)
	(gimp-image-add-layer image pattern -2)
	(gimp-image-add-layer image grain -3)
	
								;copy affected
	;(set! copyver (car (gimp-edit-copy srclayer) ) )
	(set! floater (car (gimp-edit-paste leveled TRUE) ) )
		(gimp-floating-sel-anchor floater)
	(set! floater (car (gimp-edit-paste pattern TRUE) ) )
		(gimp-floating-sel-anchor floater)
	(set! floater (car (gimp-edit-paste grain TRUE) ) )
		(gimp-floating-sel-anchor floater)
		
	;adjust Leveled Layer with levels
	(if (= inLeveled TRUE)
		(gimp-levels leveled 0 0 255 1 inLowOut inHighOut)
	)
	
	;Pattern layer color, noise, and mode
	
	(gimp-context-set-foreground '(128 128 128) )		;50% grey
	(gimp-layer-set-lock-alpha pattern TRUE)
	(gimp-edit-fill pattern 0)
	(plug-in-hsv-noise 1 image pattern 1 0 0 (/ grainValue 2) )
	
	;Grain Layer color, noise, and mode
	
	(gimp-layer-set-lock-alpha grain TRUE)
	(gimp-edit-fill grain 0)
	(plug-in-hsv-noise 1 image grain 1 0 0 grainValue)
	
	;Close it up
	
	(if (= inLeveled FALSE)
		(gimp-edit-clear leveled)
	)
		
	(if (and (= inPreserve FALSE) (= inLeveled TRUE) )
		(begin
			(set! leveled (car (gimp-image-merge-down image pattern 2) ) )
			(set! leveled (car (gimp-image-merge-down image grain 2) ) )
			(set! srclayer (car (gimp-image-merge-down image leveled 2) ) )
		)
	)
	
	(if (and (= inPreserve FALSE) (= inLeveled FALSE) )
		(begin
			(gimp-image-remove-layer image leveled)
			(set! srclayer (car (gimp-image-merge-down image pattern 2) ) )
			(set! srclayer (car (gimp-image-merge-down image grain 2) ) )
		)
	)
	
	(if (and (= inPreserve TRUE) (= inLeveled FALSE) )
		(gimp-image-remove-layer image leveled)
	)

	(gimp-image-set-active-layer image srclayer)
	(gimp-context-set-foreground origFore)
	(gimp-progress-end)
	(gimp-image-undo-group-end image)
	(gimp-displays-flush)
	(gimp-context-pop)
	)
	
)


(script-fu-register
	"script-fu-film-grain"					;func name
	"<Image>/Filters/Noise/_Double HSV Film Grain t2"	;menu label and location
	"Creates a grain pattern\
	over selected area and preserves\
	created layers."					;description
	"VangaurdVagabond"					;author
	"Copyright 2010 VanguardVagabond"			;copyright notice
	"January 3, 2010"					;date created
	"RGB RGBA"					;image type the script works on

	;input parameters
	
	SF-IMAGE	"Input Image"	0			;Image being worked on
	SF-ADJUSTMENT	"Grain"		'(8 1 20 1 4 0 0)	;Grain selector
	SF-ADJUSTMENT	"Low Output"	'(51 1 255 1 13 0 0)	;Low Level Output
	SF-ADJUSTMENT	"High Output"	'(204 1 255 1 13 0 0)	;High Level Output
	SF-TOGGLE	"Preserve Procedural Layers"	FALSE	;Preserve layers Pattern, Grain, and Leveled
	SF-TOGGLE	"Include Leveled Layer"		TRUE	;Creates a leveled layer

)