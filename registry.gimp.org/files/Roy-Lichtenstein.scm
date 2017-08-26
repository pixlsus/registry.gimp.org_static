; $Log: Roy-Lichtenstein.scm,v $
; Revision 1.2  2008-04-07 14:05:16+05:30  Cprogrammer
; combined to if statements into one if-else
;
; Revision 1.1  2008-04-06 15:31:32+05:30  Cprogrammer
; Initial revision
;
;
; photo-Roy-Lichtenstein.scm
; by $Author: Cprogrammer $
; $Revision: 1.2 $
; Description
;
; A script-fu script that adds the "Roy Lichtenstein" effect to an image
; Adapted from tutorial by Funadium at http://www.flickr.com/photos/funadium/2354849007/
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
; "$Id: Roy-Lichtenstein.scm,v 1.2 2008-04-07 14:05:16+05:30 Cprogrammer Exp root $";
;
(define (photo-RoyLichtenstein
		inImage
		inCopy
		inFlatten
		baseOpacity
		BackGroundColour
		contrast
		edgeMethod
		edgeAmount
		erodeImage
		newsPrint
		pixelSize
		spotFunc
		blackAng
		cyanAng
		magentaAng
		yellowAng
		posterizeLevel
		NewsPrintOpacity
		DeSpeckle)

	; Initiate some variables
	(let*
	 	(
			(theImage 0)
			(base 0)
			(NewsPrintLayer 0)
			(BorderLayer 0)
			(layerRGB 0)
			(width 0)
			(height 0)
			(bottomlayer 0)
			(drawable 0)
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

		; Add a coloured layer to bottom. This I felt gives some punch to the image
		; You can play with different colours to get different effects.
		(set! bottomlayer (car (gimp-layer-new theImage width height RGB-IMAGE "Bottom" 100 NORMAL-MODE)))
		(gimp-image-add-layer theImage bottomlayer -1)
		(gimp-palette-set-foreground BackGroundColour)
		(gimp-bucket-fill bottomlayer FG-BUCKET-FILL NORMAL-MODE 100 255 0 1 1)
		(gimp-image-lower-layer-to-bottom theImage bottomlayer)

		; Add the NewsPrint layer to the image
		(if (= newsPrint TRUE)
			(begin
			(set! NewsPrintLayer (my-duplicate-layer theImage drawable))
			; Rename the layer to NewsPrint
			(gimp-drawable-set-name NewsPrintLayer "NewsPrint")
			(if (= DeSpeckle TRUE)
				(begin
				(gimp-posterize NewsPrintLayer posterizeLevel)
				(plug-in-gauss     RUN-NONINTERACTIVE theImage NewsPrintLayer 6 6 0)
				(plug-in-despeckle RUN-NONINTERACTIVE theImage NewsPrintLayer 5 2 2 254)
				)
			)
			(plug-in-newsprint RUN-NONINTERACTIVE theImage NewsPrintLayer pixelSize 
				 1 100 blackAng spotFunc cyanAng spotFunc magentaAng spotFunc yellowAng spotFunc 15)
			; Change the NewsPrint Layer's opacity
			(gimp-layer-set-opacity NewsPrintLayer NewsPrintOpacity)
			)
		)

		; Add Black Edge Border layer to the image
		(set! BorderLayer (my-duplicate-layer theImage drawable))
		(gimp-drawable-set-name BorderLayer "BorderLayer")

		(plug-in-gauss RUN-NONINTERACTIVE theImage BorderLayer 3 3 0)
		(plug-in-edge  RUN-NONINTERACTIVE theImage BorderLayer edgeAmount edgeMethod 0)
		(gimp-invert BorderLayer)
		(gimp-desaturate-full BorderLayer DESATURATE-LUMINOSITY)
		(gimp-brightness-contrast BorderLayer 0 contrast)

		(if (= erodeImage TRUE)
			(begin
			(plug-in-erode RUN-NONINTERACTIVE theImage BorderLayer 1 0 1 0 0 254)
			(plug-in-gauss RUN-NONINTERACTIVE theImage BorderLayer 3 3 0)
			)
		)
		; This makes only the edge visible and rest of the image becomes transparent
		(plug-in-colortoalpha RUN-NONINTERACTIVE theImage BorderLayer '(255 255 255))

		(gimp-layer-set-opacity drawable baseOpacity)

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

		; Ensure the updated image is displayed now
		(gimp-displays-flush)
		(gimp-palette-set-foreground old-fg)
		(gimp-palette-set-background old-bg)
	)
)

(script-fu-register "photo-RoyLichtenstein" 
	"<Image>/Filters/Artistic/Sketch/Roy Lichtenstein"
	"Add Roy Lichtenstein effect to an image"
	"$Author: Cprogrammer $"
	"$Author: Cprogrammer $"
	"$Date: 2008-04-07 14:05:16+05:30 $"
	"RGB*"
	SF-IMAGE        "Image"                   0
	SF-TOGGLE       "Work on copy"            FALSE
	SF-TOGGLE       "Flatten image"           FALSE
	SF-ADJUSTMENT   "Base Layer Opacity"      '(80 0 100 5 10 1 0)
	SF-COLOR        "Background Colour"       '(255 255 255)
	SF-ADJUSTMENT   "Contrast"                '(55 -127 127 1 5 0 0)
	SF-OPTION       "Edge Detect Algorithm"   '("Sobel" "Prewitt Compass" "Gradient" "Roberts" "Differntial" "Laplace")
	SF-ADJUSTMENT   "Edge Amount"             '(4 1 10 1 5 0 0)
	SF-TOGGLE       "Erode image"             FALSE
	SF-TOGGLE       "News Print Effect"       TRUE
	SF-ADJUSTMENT   "Newsprint Pixel Size"    '(3 1 20 1 10 1 1)
	SF-OPTION       "Spot Function"           '("Round" "Line" "Diamond" "PS Square" "PS Diamond")
	SF-ADJUSTMENT   "Black Angle"             '(45 -90 90 1 10 1 1)
	SF-ADJUSTMENT   "Cyan Angle"              '(15 -90 90 1 10 1 1)
	SF-ADJUSTMENT   "Magenta  Angle"          '(75 -90 90 1 10 1 1)
	SF-ADJUSTMENT   "Yellow Angle"            '(0 -90 90 1 10 1 1)
	SF-ADJUSTMENT   "Posterize Level"         '(7 1 255 1 10 1 1)
	SF-ADJUSTMENT   "Newsprint Layer Opacity" '(50 0 100 5 10 1 1)
	SF-TOGGLE       "Despeckle"               TRUE
)
