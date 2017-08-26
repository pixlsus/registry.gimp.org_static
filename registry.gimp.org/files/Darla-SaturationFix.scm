;
; Saturation Fix, V2.0
;
; AUTHOR: Darla McKay (Darla@FarcryDesign.com), (C) 2007,2008
;
; This plugin was tested with GIMP 2.4
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License Version 3 as 
; published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
; GNU General Public License at  http://www.gnu.org/licenses for
; more details.
;
; DESCRIPTION:
; Saturation Fix for restoring clipped layers.
; The script is located in menu "<Image> / Script-Fu / Darla / Saturation Fix"
;
; USAGE NOTES:
; Creates a new layer to be used for saturation adjustments.
;     See http://www.FarcryDesign.com/GIMP/ for more information.
; =============================================================================
;
;
; SCRIPT SUMMARY:
; Method: http://www.luminous-landscape.com/tutorials/restore-clipped.shtml
; add a copy of the original layer and two 50% grey layers 
; set top layer (copy) as colour mode and merge down onto one of the greys
; set this new layer to difference mode and merge down onto other grey layer
; desaturate final layer, set levels to 0 128
; 
; Version 1.0 (2007) - Initial version
; Version 2.0 (Jan 2008)
; - updated for GIMP 2.4
; =============================================================================

(define (script-fu-Darla-SaturationFix InImage InLayer)
	(gimp-image-undo-group-start InImage)

	(let*	(
		(New1Layer (car (gimp-layer-copy InLayer TRUE)))
		(New2Layer (car (gimp-layer-copy InLayer TRUE)))
		(New3Layer (car (gimp-layer-copy InLayer TRUE)))
		(merged-layer New3Layer)
		(Old-FG-Color (car (gimp-palette-get-foreground)))
		(TheOpacity 20)
		)

		; colour layers 2 and 3 grey - set colour and fill
		(gimp-palette-set-foreground '(127 127 127))
		(gimp-drawable-fill New1Layer FOREGROUND-FILL)
		(gimp-drawable-fill New2Layer FOREGROUND-FILL)

		; Add the layers to the image
		(gimp-image-add-layer InImage New1Layer -1)
		(gimp-image-add-layer InImage New2Layer -1)
		(gimp-image-add-layer InImage New3Layer -1)

		; process layers and merge down
		(gimp-layer-set-mode New3Layer COLOR-MODE)
		(set! merged-layer (car (gimp-image-merge-down InImage New3Layer CLIP-TO-IMAGE)))

		(gimp-layer-set-mode merged-layer DIFFERENCE-MODE)
		(set! merged-layer (car (gimp-image-merge-down InImage merged-layer CLIP-TO-IMAGE)))

		(gimp-desaturate merged-layer)
		(gimp-levels merged-layer 0 0 128 1 0 255)
		(gimp-layer-set-opacity merged-layer TheOpacity)
		(gimp-drawable-set-name merged-layer "Saturation Adjustment")
	)

	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
)

(script-fu-register 
	"script-fu-Darla-SaturationFix"
	"<Image>/Script-F_u/_Darla/_Saturation Fix"
	"Saturation Fix  \n\
For adjusting saturation to restore clipped layers. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
      "RGB*"
	SF-IMAGE		"The Image"	0
	SF-DRAWABLE		"The Layer"	0
)
