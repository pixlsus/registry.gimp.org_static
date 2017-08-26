;
; Purple Fringe, V2.0
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
; To reduce the the effects of purple fringing or chromatic aberration 
; in high contrast areas of an affected photograph.
; The script is located in menu "<Image> / Script-Fu / Darla / Purple Fringe"
;
; USAGE NOTES:
;     See http://www.FarcryDesign.com/GIMP/ for more information.
; =============================================================================
;
;
; SCRIPT SUMMARY:
; use mask to affect edges only, adjust saturation of magenta, red, blue, cyan
; 
; Version 1.0 (2007) - Initial version
; Version 2.0 (Jan 2008)
; - updated for GIMP 2.4
; =============================================================================

(define (script-fu-Darla-PurpleFringe InImage InLayer InEdge InBlur InDeMag InDeRed InDeBlue InDeCyan InFlatten)
	(gimp-image-undo-group-start InImage)

	(let*	(
		(FringeLayer (car (gimp-layer-copy InLayer TRUE)))
		(MaskImage (car (gimp-image-duplicate InImage)))
		(MaskLayer (cadr (gimp-image-get-layers MaskImage)))
		(HSVImage  (car (plug-in-decompose TRUE InImage InLayer "Value" TRUE)))
   		(HSVLayer  (cadr (gimp-image-get-layers HSVImage)))
		)

		; desats
		(gimp-image-add-layer InImage FringeLayer -1)
		(gimp-hue-saturation FringeLayer 6 0 0 InDeMag)
		(gimp-hue-saturation FringeLayer 1 0 0 InDeRed)
		(gimp-hue-saturation FringeLayer 5 0 0 InDeBlue)
		(gimp-hue-saturation FringeLayer 4 0 0 InDeCyan)

		; Find edges, Warpmode = Smear (1), Edgemode = Sobel (0)
		(plug-in-edge TRUE MaskImage (aref MaskLayer 0) InEdge 1 0)
		(gimp-levels (aref MaskLayer 0) 0 75 255 0.8 0 255)
		(gimp-convert-grayscale MaskImage)
		(plug-in-gauss TRUE MaskImage (aref MaskLayer 0) InBlur InBlur 0)

		; next change FringeLayer to new layer
		(let*	(
			(FringeMask (car (gimp-layer-create-mask FringeLayer ADD-WHITE-MASK)))
			)
			(gimp-layer-add-mask FringeLayer FringeMask)
			(gimp-selection-all MaskImage)
			(gimp-edit-copy (aref MaskLayer 0))
			(gimp-floating-sel-anchor (car (gimp-edit-paste FringeMask FALSE)))
			(gimp-image-delete MaskImage)
		)

		; flatten image if needed
		(cond
			((= InFlatten TRUE) (gimp-image-merge-down InImage FringeLayer CLIP-TO-IMAGE))
			((= InFlatten FALSE) (gimp-drawable-set-name FringeLayer "Purple Fringe Adjustment"))
		)
	)

	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
)

(script-fu-register 
	"script-fu-Darla-PurpleFringe"
	"<Image>/Script-F_u/_Darla/_Purple Fringe"
	"Purple Fringe \n\
To reduce the effects of Purple Fringing or \
Chromatic Aberration in high contrast areas of \
an affected photograph. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
	"RGB*"
	SF-IMAGE		"The Image"		0
	SF-DRAWABLE		"The Layer"		0
	SF-ADJUSTMENT	_"Edges: Detect Amount"	'(7.0 1.0 10.0 1.0 0 1 0)
	SF-ADJUSTMENT	_"Edges: Blur Pixels"	'(5.0 1.0 10.0 1.0 0 1 0)
	SF-ADJUSTMENT	_"Desat level: Magenta"	'(-80 -100 100 1 0 0 0)
	SF-ADJUSTMENT	_"Desat level: Red"	'(-60 -100 100 1 0 0 0)
	SF-ADJUSTMENT	_"Desat level: Blue"	'(-40 -100 100 1 0 0 0)
	SF-ADJUSTMENT	_"Desat level: Cyan"	'(0 -100 100 1 0 0 0)
	SF-TOGGLE		_"Flatten Image"		FALSE
)
