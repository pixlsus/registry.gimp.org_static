; Advanced Tone Mapping is a script for The GIMP
;
; Reduce global contrast while increasing local contrast and shadow/highlight
; detail.
;
; Advanced Tone Mapping is a fork/continuation of Tonemapping script
; originally written by David Meiklejohn and Harry Phillips.
; The 2.0 version was created by Vit 'tasuki' Brunner.
;
; The script is located in menu "<Image> / Filters / Enhance"
; Last changed: 13 June 2008
;
; --------------------------------------------------------------------
;  
; Changelog:
;  Version 2.0 codename 'tasuki' (Friday the 13th, June 2008)
;    - Moved the script to Filters/Enhance where it IMHO belongs
;    - Removed result flattening
;    - Changed the amount of Gauss-blur to be in % of image size
;    - Added option to set the original Gauss-blurred layer's opacity
;      (default staying 75)
;    - Added option to set the number of times the merged layer is copied
;    - The original layer is preserved. The script adds one non-transparent
;      layer with all the changes in it, which is named according to
;      the parameters the script was called with (looks messy but is handy)
;
;  Version 1.4 (8th August 2007)
;    - Added option to flatten the result
;
;  Version 1.3 (5th August 2007)
;    - Added GPL3 licence 
;    - Menu location at the top of the script
;    - Removed the "script-fu-menu-register" section
; 
;  Version 1.2
;    - Made the script compatible with GIMP 2.3
;
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.


(define (my-duplicate-layer image layer)
	(let* ((dup-layer (car (gimp-layer-copy layer 1))))
		(gimp-image-add-layer image dup-layer 0)
		dup-layer))

(define (script-fu-advanced-tone-mapping
	theImage
	theLayer
	blurAmount
	opacityBlurredAmount
	opacityMergedAmount
	copiesAmount)

	;Start an undo group so the process can be undone with one undo
	(gimp-image-undo-group-start theImage)

	(let
		(
			;Create temporary layers
			(copy1 (my-duplicate-layer theImage theLayer))
			(copy2 (my-duplicate-layer theImage theLayer))
			(copy3 (my-duplicate-layer theImage theLayer))
			;Round the input values (displays nicely in layer name)
			(blurAmount (round blurAmount))
			(opacityBlurredAmount (round opacityBlurredAmount))
			(opacityMergedAmount (round opacityMergedAmount))
			(copiesAmount (round copiesAmount))
		)

		;Apply the desaturate and invert to the top layer
		(gimp-desaturate copy3)
		(gimp-invert copy3)

		;Apply the blur with the supplied blur amount (in percents!)
		(let ((blur (* blurAmount (/ (+ (car (gimp-drawable-width theLayer))
				(car (gimp-drawable-height theLayer))) 200))))
			(plug-in-gauss 1 theImage copy3 blur blur 0))

		;Set the layer's opacity
		(gimp-layer-set-opacity copy3 opacityBlurredAmount)

		;Merge the top layer down and keep track of the newly merged layer
		(let
			(
				(merged (car (gimp-image-merge-down theImage copy3 0)))
				(i 0)
			)

			;Change the merged layers mode to SOFT LIGHT (19)
			(gimp-layer-set-mode merged 19)

			;Change the merged layers opacity
			(gimp-layer-set-opacity merged opacityMergedAmount)

			;Copy and merge the layer the necessary number of times
			(set! i 1)
			(while (< i copiesAmount)
				(let ((tmpLayer (my-duplicate-layer theImage merged)))
					(gimp-image-lower-layer theImage tmpLayer)
					(gimp-image-merge-down theImage tmpLayer 0)
					(set! i (+ i 1))))

			;Merge the last layer down
			(let ((final (car (gimp-image-merge-down theImage merged 0))))
				;Name the layer so that we can see the parameters in its name
				(gimp-drawable-set-name final
					(string-append
						"tmapd: g "
						(number->string blurAmount)
						", ob "
						(number->string opacityBlurredAmount)
						", om "
						(number->string opacityMergedAmount)
						", c "
						(number->string copiesAmount)))))

		;Finish the undo group for the process
		(gimp-image-undo-group-end theImage)

		;Ensure the updated image is displayed now
		(gimp-displays-flush)
	)
)

(script-fu-register "script-fu-advanced-tone-mapping"
	_"<Image>/Filters/Enhance/Advanced Tone Mapping..."
	"Performs a tone mapping operation with a specified blur on the open image"
	"David Meiklejohn, Harry Phillips, Vit 'tasuki' Brunner"
	"2006-2008, David Meiklejohn, Harry Phillips, Vit 'tasuki' Brunner"
	"2008-06-13"
	"*"
	SF-IMAGE		"Image"     0
	SF-DRAWABLE		"Drawable"  0
	SF-ADJUSTMENT	_"Gauss. Blur (% of img size)"   '(10 1 100 1 10 0 0)
	SF-ADJUSTMENT	_"Opacity of blurred layer"   '(75 0 100 1 10 0 0)
	SF-ADJUSTMENT	_"Opacity of merged layer" '(90 0 100 1 10 0 0)
	SF-ADJUSTMENT	_"Copies of merged layer" '(1 1 10 1 10 0 0)
)

