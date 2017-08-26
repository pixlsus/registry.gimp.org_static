; $Log: midnightSepia.scm,v $
; Revision 1.2  2008-04-07 14:05:01+05:30  Cprogrammer
; added option to change blur amount
;
; Revision 1.1  2008-04-07 11:40:20+05:30  Cprogrammer
; Initial revision
;
;
; midnightSepia.scm
; by $Author: Cprogrammer $
; $Revision: 1.2 $
; Description
;
; A Gimp script-fu that gives a Midnight Sepia effect as described
; at http://www.escrappers.com/midnightsepia.html
; 1.  Create a duplicate of the first Layer
; 2.  Apply a Gaussian Blur on the top Layer
;     Radius=20
; 3.  Use Hue/Saturation
;     Check the Colorize Option
;     Hue=45
;     Saturation=25
;     Lightness=0 
; 4.  Change the Blending Mode to Multiply
; 5.  Flatten the Image
; 6.  Duplicate the Layer
; 7.  Change the Blending Mode to Screen
; 8.  Flatten the Image
; 9.  Duplicate the Layer
; 10. Change the Blending Mode to Screen
; 11. Adjust the Opacity to taste
;     The script sets it to zero
;
; Adapted the script from script written by kward1979uk
; http://kward1979uk.deviantart.com/art/Midnight-Sepia-Script-fu-42108641
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
; "$Id: midnightSepia.scm,v 1.2 2008-04-07 14:05:01+05:30 Cprogrammer Exp root $";
;
;
; RGB-TO-HSV taken from http://pages.interlog.com/~kcozens/software/gimp/2.0/neon-sign.scm
; Convert RGB ([0-1],[0-1],[0-1]) to HSV ([0-360],[0-1],[0-1])

(define (photo-midnightSepia
		inImage
		theLayer
		inCopy
		inFlatten
		deSaturate
		internalHsv
		Hue
		lightNess
		Saturation
		gblur
		Opacity
		myColour)

	; Initiate some variables
	(let*
	 	(
			(theImage 0)
			(base 0)
			(layerRGB 0)
			(width 0)
			(height 0)
			(drawable 0)
			(myLayer 0)
			(old-fg 0)
			(old-bg 0)
			(hsv 0)
			(h 0)
			(s 0)
			(v 0)
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

		(set! myLayer (my-duplicate-layer theImage drawable))

		(if (= internalHsv TRUE)
			(begin
			(set! hsv (gimp-rgb-to-hsv myColour))
			(set! h (car hsv))
			(set! s (cadr hsv))
			(set! v (caddr hsv))
			(gimp-colorize myLayer h s v)
			) ; else
			(begin
			(gimp-hue-saturation myLayer ALL-HUES Hue lightNess Saturation)
			)
		)
    	(gimp-brightness-contrast myLayer 0 20)
		(if (= deSaturate TRUE)
			(begin
			(gimp-desaturate-full myLayer DESATURATE-LIGHTNESS)
			)
		)
		(plug-in-gauss RUN-NONINTERACTIVE theImage myLayer gblur gblur 0)
		(gimp-layer-set-mode myLayer MULTIPLY-MODE)
		(set! drawable (car (gimp-image-flatten theImage)))

		(set! myLayer (my-duplicate-layer theImage drawable))
		; Change the Layer's opacity
		(gimp-layer-set-opacity myLayer Opacity)
		(gimp-layer-set-mode myLayer SCREEN-MODE)
		(set! drawable (car (gimp-image-flatten theImage)))

		(set! myLayer (my-duplicate-layer theImage drawable))
		(gimp-drawable-set-name myLayer "TopLayer")
		(gimp-layer-set-mode myLayer SCREEN-MODE)
		; Change the Layer's opacity
		(gimp-layer-set-opacity myLayer 0)

		; ----------------- End ------------------------------------------
		(if (= inFlatten TRUE)
			(begin
			(gimp-image-flatten theImage)
			)
		)
		(if (= inCopy TRUE)
			(begin
			(gimp-image-clean-all theImage)
			(gimp-display-new theImage)
			)
			(begin ; else
			; Finish the undo group for the process
			(gimp-image-undo-group-end theImage)
			)
		)
		; Ensure the updated image is displayed now
		(gimp-displays-flush)
		; Restore the original foreground & background palette
		(gimp-palette-set-foreground old-fg)
		(gimp-palette-set-background old-bg)
	)
)

(script-fu-register "photo-midnightSepia" 
	"<Image>/Filters/Artistic/Midnight Sepia"
	"Add Midnight Sepia Effect to an image"
	"$Author: Cprogrammer $"
	"$Author: Cprogrammer $"
	"$Date: 2008-04-07 14:05:01+05:30 $"
	"RGB*"
	SF-IMAGE      "Image"            0
	SF-DRAWABLE   "Drawable"         0
	SF-TOGGLE     "Work on copy"     FALSE
	SF-TOGGLE     "Flatten image"    FALSE
	SF-TOGGLE     "Desaturate"       FALSE
	SF-TOGGLE     "Internal RGB-HSV" TRUE
	SF-ADJUSTMENT "Hue"              '(45 -180 180 5 10 1 0)
	SF-ADJUSTMENT "Lightness"        '(0  -100 100 5 10 1 0)
	SF-ADJUSTMENT "Saturation"       '(35 -100 100 5 10 1 0)
	SF-ADJUSTMENT "Blur"             '(20 1 100 1 10 0 0)
	SF-ADJUSTMENT "Opacity"          '(80 0 100 5 10 1 0)
	SF-COLOR      "Colour"           '(162 138 101)
)
