;
; Blue Sky Gradient, V2.0
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
; To fix burned out or white skies by adding a blue sky gradient.
; The script is located in menu "<Image> / Script-Fu / Darla / Blue Sky Gradient"
;
; USAGE NOTES:
; Two colours are selectable, specifying the upper (darker) and lower 
; (lighter) sky.  The gradient is applied based on a level horizon.
; The resulting layer can easily be edited to "white-out" any areas that 
; were accidentally picked up and coloured.  The amount of colour can be 
; fine-tuned by adjusting the transparency of the new sky layer. If no sky 
; gets picked up, retry the script with a lower threshold number.
;     See http://www.FarcryDesign.com/GIMP/ for more information.
; =============================================================================
;
;
; SCRIPT SUMMARY:
; copy layer, use threshold mask & invert, select black areas, grow & feather selection, then fill with selected colour gradient
; 
; Version 1.0 (2007) - Initial version
; Version 2.0 (Jan 2008)
; - updated for GIMP 2.4
; =============================================================================

(define (script-fu-Darla-BlueSkyGradient InImage InLayer InThreshold InSkyTop InSkyBottom InGrow InFeather InFlatten)
	(gimp-image-undo-group-start InImage)

	(let*	(
		(SkyLayer (car (gimp-layer-copy InLayer TRUE)))
		(TheHeight (car (gimp-image-height InImage)))
		(Old-FG-Color (car (gimp-context-get-foreground)))
		(Old-BG-Color (car (gimp-context-get-background)))
		)

		(gimp-image-add-layer InImage SkyLayer -1)
		(gimp-threshold SkyLayer InThreshold 255)
		(gimp-invert SkyLayer)
		(gimp-context-set-foreground InSkyTop)
		(gimp-context-set-background InSkyBottom)
		(gimp-by-color-select SkyLayer '(0 0 0) 0 2 FALSE 0 0 0)
		(gimp-selection-grow InImage InGrow)
		(gimp-selection-feather InImage InFeather)
		(gimp-edit-blend SkyLayer FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 0 0 TRUE 0 0 0 (- TheHeight 1))
		(gimp-selection-none InImage)
		(gimp-layer-set-mode SkyLayer DARKEN-ONLY-MODE)

		; return original color palette, flatten image if needed
		(gimp-context-set-foreground Old-FG-Color)
		(gimp-context-set-background Old-BG-Color)
		(cond
			((= InFlatten TRUE) (gimp-image-merge-down InImage SkyLayer CLIP-TO-IMAGE))
			((= InFlatten FALSE) (gimp-drawable-set-name SkyLayer "Blue Sky Gradient"))
		)
	)

	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
)

(script-fu-register 
	"script-fu-Darla-BlueSkyGradient"
	"<Image>/Script-F_u/_Darla/Blue Sky _Gradient"
	"Blue Sky Gradient \n\
To fix burned out or white skies by adding a blue sky \
gradient with a threshold mask. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
	"RGB* GRAY*"
	SF-IMAGE		"The Image"			0
	SF-DRAWABLE		"The Layer"			0
	SF-ADJUSTMENT	_"Threshold"			'(248 0 254 1 0 0 0)
	SF-COLOR		_"Sky Top Color"			'(187 219 255)
	SF-COLOR		_"Sky Bottom Color"		'(221 234 255)
	SF-ADJUSTMENT	_"Edges: Grow Amount"		'(3.0 1.0 10.0 1.0 0 1 0)
	SF-ADJUSTMENT	_"Edges: Feather Amount"	'(5.0 1.0 10.0 1.0 0 1 0)
	SF-TOGGLE		_"Flatten Image"			FALSE
)
