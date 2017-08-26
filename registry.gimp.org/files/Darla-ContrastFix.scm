;
; Contrast Fix, V2.0
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
; Adds a Contrast Adjustment layer to your image to help fix contrast problems.
; The script is located in menu "<Image> / Script-Fu / Darla / Contrast Fix"
;
; USAGE NOTES:
; Creates a new layer to be used for contrast adjustments (mode & %)
;     See http://www.FarcryDesign.com/GIMP/ for more information.
; =============================================================================
;
;
; SCRIPT SUMMARY:
; Method: http://www.luminous-landscape.com/tutorials/contrast_masking.shtml and
; http://www.luminous-landscape.com/tutorials/understanding-series/u-contrast-masking.shtml
; copy layer, desaturate, invert, set mode to overlay and blur 
; setting threshold to a number affects how much is lightened.  0 means whole image.
; 
; Version 1.0 (2007) - Initial version
; Version 2.0 (Jan 2008)
; - updated for GIMP 2.4
; =============================================================================

(define (script-fu-Darla-ContrastFix InImage InLayer InOrig InBlur InThreshold InTint InCustomTint InFlatten)
	(gimp-image-undo-group-start InImage)

	(let*	(
		(Old-FG-Color (car (gimp-context-get-foreground)))
		(NewLayer (car (gimp-layer-copy InLayer TRUE)))
		(NewMask (car (gimp-layer-create-mask NewLayer ADD-WHITE-MASK)))
		(MaskImage (car (gimp-image-duplicate InImage)))
		(MaskLayer (cadr (gimp-image-get-layers MaskImage)))
		(TheWidth (car (gimp-image-width InImage)))
		(TheHeight (car (gimp-image-height InImage)))
		)

		(gimp-image-add-layer InImage NewLayer -1)
		(gimp-desaturate NewLayer)
		(gimp-invert NewLayer)
		(plug-in-gauss TRUE InImage NewLayer InBlur InBlur 0)
            (gimp-drawable-set-name NewLayer "Contrast Adjustment")

		; choose method for too dark or too light original -- set opacity and mode
		; 0 too dark, 1 too light
		(cond
			((= InOrig 0)
				(begin
					(gimp-layer-set-opacity NewLayer 80)
					(gimp-layer-set-mode NewLayer OVERLAY-MODE)

					(if (not (= InThreshold 0))
						(begin
							; add masklayer with threshold and blur by a factor of 1/15th minimum of height/width
							(let* (
								(BlurMask (/ (if (< TheWidth TheHeight) TheWidth TheHeight) 15))
								)
								(gimp-threshold (aref MaskLayer 0) 0 InThreshold)
								(plug-in-gauss TRUE MaskImage (aref MaskLayer 0) BlurMask BlurMask 0)
							)
							; change layer to mask for border
							(let*	(
								(NewMask (car (gimp-layer-create-mask NewLayer ADD-WHITE-MASK)))
								)
								(gimp-layer-add-mask NewLayer NewMask)
								(gimp-selection-all MaskImage)
								(gimp-edit-copy (aref MaskLayer 0))
								(gimp-floating-sel-anchor (car (gimp-edit-paste NewMask FALSE)))
								(gimp-image-delete MaskImage)
							)
						)
					)

				)
			)
			((= InOrig 1) 
				(begin
					; copy current layer into layer mask
					(gimp-layer-add-mask NewLayer NewMask)
					(gimp-invert NewLayer)
					(gimp-selection-all InImage)
					(gimp-edit-copy NewLayer)
					(gimp-floating-sel-anchor (car (gimp-edit-paste NewMask FALSE)))
;					(gimp-image-delete InImage)

					; Set Tint Options: 0 standard 1 cool 2 warm 3 custom
					(cond
						((= InTint 0) (gimp-context-set-foreground '(148 158 188)))
						((= InTint 1) (gimp-context-set-foreground '(148 205 216)))
						((= InTint 2) (gimp-context-set-foreground '(255 214 184)))
						((= InTint 3) (gimp-context-set-foreground InCustomTint))
					)
					(gimp-drawable-fill NewLayer FOREGROUND-FILL)
					(gimp-layer-set-opacity NewLayer 15)
					(gimp-layer-set-mode NewLayer DARKEN-ONLY-MODE)
				)
			)
		)

		; return to original foreground colour, flatten image if needed
		(gimp-context-set-foreground Old-FG-Color)
		(cond
			((= InFlatten TRUE) (gimp-image-merge-down InImage NewLayer CLIP-TO-IMAGE))
			((= InFlatten FALSE) (gimp-drawable-set-name NewLayer "Contrast Adjustment"))
		)
	)
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
)

(script-fu-register 
	"script-fu-Darla-ContrastFix"
	"<Image>/Script-F_u/_Darla/_Contrast Fix"
	"Contrast Fix \n\
Adds a Contrast Adjustment layer to your image to help fix \
contrast problems. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
      "RGB*"
	SF-IMAGE		"The Image"			0
	SF-DRAWABLE		"The Layer"			0
	SF-OPTION		_"Original is" 			'("Too Dark"
									"Too Bright/Burned Out")
	SF-ADJUSTMENT	_"Blur Amount"			'(10.0 1.0 100.0 1.0 0 2 0)
	SF-ADJUSTMENT	_"Threshold Mask (Dark Only)"	'(0 0 254 1 0 0 0)
	SF-OPTION		_"Burnout tint (Bright Only)"	'("Standard"
									"Cool tone (sky)"
									"Warm tone (skintone)"
									"Custom (choose)")
	SF-COLOR 		_"Burnout tint: Custom"		'(148 158 188)
	SF-TOGGLE		_"Flatten image"			FALSE
)
