; $Log: $
;
; photo-manny-librodo.scm
; by $Author: $
; $Revision: $
; Description
;
; A script-fu script that 
;
; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html
;
; "$Id: $";
;
(define (my-duplicate-layer image layer)
	(let* (
		(dup-layer (car (gimp-layer-copy layer 1))))
		(gimp-image-add-layer image dup-layer 0)
		dup-layer))

(define (photo-manny-librodo
		inImage
		theLayer
		radius1
		amount1
		radius2
		amount2
		thresh
		Opacity
		inCopy
		inFlatten)

	; Initiate some variables
	(let*
	 	(
			(theImage 0)
			(drawable 0)
			(layerRGB 0)
			(myLayer 0)
			(width 0)
			(height 0)
			(old-fg 0)
			(old-bg 0)
		)
		; Return the Image ID
		(set! theImage (if (= inCopy TRUE)
			(car (gimp-image-duplicate inImage))
			inImage
			) 
		)
		(if (= inCopy FALSE)
			(begin
			; Start an undo group so the process can be undone with one undo
			(gimp-image-undo-group-start theImage)
			)
		)
		; Init
		(set! drawable (car (gimp-image-get-active-drawable theImage)))
		; Detect if it is RGB. Change the image RGB if it isn't already
		(set! layerRGB (car (gimp-drawable-is-rgb drawable)))
		(if (= layerRGB 0) (gimp-image-convert-rgb theImage))
		; Read the image height and width so that we can create a new layer of the same
		; dimensions of the current image
		(set! old-fg (car (gimp-palette-get-foreground)))
		(set! old-bg (car (gimp-palette-get-background)))
		(set! width  (car (gimp-image-width  theImage)))
		(set! height (car (gimp-image-height theImage)))
		; ----------------- Begin ------------------------------------------
		(set! myLayer (my-duplicate-layer theImage drawable))
		(plug-in-unsharp-mask RUN-NONINTERACTIVE theImage myLayer radius1 amount2 thresh)
		(plug-in-unsharp-mask RUN-NONINTERACTIVE theImage myLayer radius2 amount2 thresh)
		(set! drawable (my-duplicate-layer theImage myLayer))
		(gimp-layer-set-mode drawable DARKEN-ONLY-MODE)
        (set! myLayer (car (gimp-image-merge-down theImage drawable 0)))

		(plug-in-unsharp-mask RUN-NONINTERACTIVE theImage myLayer radius2 amount2 thresh)
		(set! drawable (my-duplicate-layer theImage myLayer))
		(gimp-layer-set-mode drawable LIGHTEN-ONLY-MODE)
		(gimp-layer-set-opacity drawable 50)
        (set! myLayer (car (gimp-image-merge-down theImage drawable 0)))
		(gimp-layer-set-opacity myLayer Opacity)
		; ----------------- End   ------------------------------------------
		(if (= inFlatten TRUE)
			(begin
			(gimp-image-flatten theImage)
			)
		)
		(if (= inCopy TRUE)
			(begin
			(gimp-image-clean-all theImage)
			(gimp-display-new theImage)
			) ; else
			(begin
			; Finish the undo group for the process
			(gimp-image-undo-group-end theImage)
			)
		)

		(gimp-palette-set-foreground old-fg)
		(gimp-palette-set-background old-bg)
		; Ensure the updated image is displayed now
		(gimp-displays-flush)
	)
)

(script-fu-register "photo-manny-librodo" 
    "<Image>/Filters/Enhance/Sharp (Manny Librodo)"
	"Add xxx efect to image"
	"$Author: Cprogrammer $"
	"$Author: Cprogrammer $"
	"$Date: 2008-11-21 14:05:01+05:30 $"
	"*"
	SF-IMAGE      "Image"          0
	SF-DRAWABLE   "Drawable"       0
	SF-ADJUSTMENT "USM: Radius1"   '(40.0 0.0 50.0 1 0 2 0)
	SF-ADJUSTMENT "USM: Amount1"   '(0.18 0.0 5.0 0.5 0 2 0)
	SF-ADJUSTMENT "USM: Radius2"   '(0.3 0.0 50.0 1 0 2 0)
	SF-ADJUSTMENT "USM: Amount2"   '(1.5 0.0 5.0 0.5 0 2 0)
	SF-ADJUSTMENT "USM: Threshold" '(0.0 0.0 50.0 1.0 0 2 0)
	SF-ADJUSTMENT "Opacity"        '(50 0 100 5 10 1 0)
	SF-TOGGLE     "Work on copy"   FALSE
	SF-TOGGLE     "Flatten image"  FALSE
)
