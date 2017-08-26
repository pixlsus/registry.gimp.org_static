;
; Amazing Circles, V2.2
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
; Amazing Circles technique, clean border with size colour options, additional 
; options to distort with pinch and/or whirl, add a drop shadow & shading.
; The script is located in menu "<Image> / Script-Fu / Darla / Amazing Circles"
;
; USAGE NOTES:
; This script creates a copy of your original image (or active selection) and 
; applies the "Amazing Circles" technique.  If you choose not to flatten the
; image, you can further adjust the border layer after running this script.
; Hint, the centerpoint is very important, especially if you choose border 
; average as your frame colour -- try selecting from a centerpoint.
;     See http://www.FarcryDesign.com/GIMP/ for more information.
; =============================================================================
;
;
; SCRIPT SUMMARY:
; from selection, copy, paste as new, std size 960x960 (or keep square current), distort polar (not to polar)
; set border options (black, white, border average, current fg/bg)
; layer transform flip vertically, distort polar to polar, canvas size 1000x1000 (or calculate from max, but not stretch), layer to image size, 
; add a border layer, apply mask to both layers, optionally add shading layer and drop shadow (also both with masks applied)
; 
; Version 1.0 (2007) - Initial version
; Version 1.1 
; - add adjustments for Filter, Distort, Whirl and Pinch e.g. angle 0, pinch .6, radius 1.6
; Version 2.0 
; - updated for GIMP 2.4 (changed border average method)
; - added alternate circle method
; - added return to original select state
; Version 2.1 (Jan 2008)
; - replaced user select option with current fg/bg options
; - changed whirlpinch option to interactive, if selected
; Version 2.2 (Jan 2008)
; - adding option for dropshadow & shading, applying masks to each layer individually
; =============================================================================

(define (script-fu-Darla-AmazingCircles InImage InLayer InFinal InBorder InBASize InPercent InGrow InFeather InShade InWhirlPinch InAlt InFlatten)
	(gimp-image-undo-group-start InImage)

	(let*	(
		(Old-FG-Color (car (gimp-context-get-foreground)))
		(Old-BG-Color (car (gimp-context-get-background)))
		(MaskImage (car (gimp-image-duplicate InImage)))
		(MaskLayer (cadr (gimp-image-get-layers MaskImage)))
		(draw-type (car (gimp-drawable-type-with-alpha InLayer)))
		(image-type (car (gimp-image-base-type InImage)))
		(selection-bounds (gimp-selection-bounds InImage))
		(select-offset-x  (cadr selection-bounds))
		(select-offset-y  (caddr selection-bounds))
		(selection-width  (- (cadr (cddr selection-bounds)) select-offset-x))
		(selection-height (- (caddr (cddr selection-bounds)) select-offset-y))
		(TheSize selection-height)
		(CircleSize TheSize)
		(BAColor Old-BG-Color) ; border average
		(TheOffset 0)
		(TheDiam 0)
		(BorderOffset 0)
		(from-selection FALSE)
		(active-selection (car (gimp-selection-save InImage)))
            (new-image (car (gimp-image-new selection-width selection-height image-type)))
     	      (new-draw (car (gimp-layer-new new-image selection-width selection-height draw-type "Selection" 100 NORMAL-MODE)))
		(BorderLayer (car (gimp-layer-new-from-drawable new-draw new-image)))
		(VignetteImage (car (gimp-image-duplicate new-image)))
		(VignetteLayer (cadr (gimp-image-get-layers VignetteImage)))
		)

		; if there is no selection, select the whole image
		(if (= (car (gimp-selection-is-empty InImage)) TRUE)
			(begin
				(gimp-selection-layer-alpha InLayer)
				(set! active-selection (car (gimp-selection-save InImage)))
				(set! from-selection FALSE)
			)
			(begin
				(set! from-selection TRUE)
				(set! active-selection (car (gimp-selection-save InImage)))
			)
		)

		; create new image from selection
            (gimp-edit-copy InLayer)
            (set! new-image (car (gimp-image-new selection-width selection-height image-type)))
            (set! new-draw (car (gimp-layer-new new-image selection-width selection-height draw-type "Selection" 100 NORMAL-MODE)))
            (gimp-image-add-layer new-image new-draw 0)
            (gimp-drawable-fill new-draw BACKGROUND-FILL)
            (let ((floating-sel (car (gimp-edit-paste new-draw FALSE))))
            (gimp-floating-sel-anchor floating-sel))
            (gimp-image-set-active-layer InImage InLayer)
            (gimp-display-new new-image)
		; get border average colour
		(set! BAColor (car (gimp-image-pick-color new-image new-draw (/ selection-width 2)(/ selection-height 2) TRUE TRUE InBASize)))

		; whirl/pinch, if applicable - interactive
		(if (not (= InWhirlPinch 0)) (plug-in-whirl-pinch FALSE new-image new-draw 0 0 0))

		; calculate desired final and circle size to work with
		; if min current width/height < InFinal, then TheSize= current, else InFinal (TheSize will be the final size to use, the lesser of InFinal or 1000)
		(if (< selection-width selection-height) (set! TheSize selection-width) (set! TheSize selection-height))
		(if (< TheSize InFinal) (set! TheSize TheSize) (set! TheSize InFinal))
		; choose CircleSize with by calculating border percent and set image to this size
		(set! CircleSize (* TheSize (- 1 (* .01 InPercent))))
		(gimp-image-scale new-image CircleSize CircleSize)

		; distort routine (with alternate actions, if selected)
		; distort polar (not to polar)
		(plug-in-polar-coords TRUE new-image new-draw 100 0 1 0 0)
		; layer transform flip vertically
		(gimp-image-flip new-image 1)
		; distort polar to polar (or alt)
		(if (not (= InAlt 0))
			(begin
				(plug-in-polar-coords TRUE new-image new-draw 100 0 1 0 0)
				(gimp-image-flip new-image 1)
				(plug-in-polar-coords TRUE new-image new-draw 100 0 1 0 1)
				(gimp-image-flip new-image 1)
			)
			(begin
				(plug-in-polar-coords TRUE new-image new-draw 100 0 1 0 1)
			)
		)

		; canvas size set to larger final size to accommodate border, layer to image size
		(set! TheOffset (* TheSize (/ InPercent 200)))
		(gimp-image-resize new-image TheSize TheSize TheOffset TheOffset)
		(gimp-layer-resize-to-image-size new-draw)

		; fill border layer with colour according to color option chosen
		; Border Options: 0 white 1 black 2 border average 3 current foreground 4 current background 
		(cond
			((= InBorder 0) (gimp-context-set-foreground '(255 255 255)))
			((= InBorder 1) (gimp-context-set-foreground '(0 0 0)))
			((= InBorder 2) (gimp-context-set-foreground BAColor))
			((= InBorder 3) (gimp-context-set-foreground Old-FG-Color))
			((= InBorder 4) (gimp-context-set-foreground Old-BG-Color))
		)
		(set! BorderLayer (car (gimp-layer-new-from-drawable new-draw new-image)))
		(gimp-image-add-layer new-image BorderLayer -1)
		(gimp-drawable-fill BorderLayer FOREGROUND-FILL)

		; set fg/bg to black and white for masking
		(gimp-context-set-foreground '(255 255 255))
		(gimp-context-set-background '(0 0 0))

		; add layer, select circle in the centre, feather, fill with black.
		(gimp-drawable-fill (aref MaskLayer 0) FOREGROUND-FILL)
		(set! TheDiam (- CircleSize (* 2 InGrow)))
		(set! BorderOffset (/ (- TheSize TheDiam) 2))
		(gimp-ellipse-select MaskImage BorderOffset BorderOffset TheDiam TheDiam 2 TRUE TRUE InFeather)
		(gimp-edit-fill (aref MaskLayer 0) BACKGROUND-FILL)

		; change layer to mask for border
		(let*	(
			(BorderMask (car (gimp-layer-create-mask BorderLayer ADD-WHITE-MASK)))
			(VignetteMask (car (gimp-layer-create-mask new-draw ADD-WHITE-MASK)))
			(CircleMask (car (gimp-layer-create-mask new-draw ADD-WHITE-MASK)))
			)
			(gimp-layer-add-mask BorderLayer BorderMask)
			(gimp-selection-all MaskImage)
			(gimp-edit-copy (aref MaskLayer 0))
			(gimp-floating-sel-anchor (car (gimp-edit-paste BorderMask FALSE)))
;			(gimp-layer-remove-mask BorderLayer MASK-APPLY)
			(gimp-layer-set-apply-mask BorderLayer FALSE)
			(gimp-drawable-set-name BorderLayer "Border")

			; add vignette, if applicable
			(if (not (= InShade 0)) 
				(begin
					(set! VignetteLayer (car (gimp-layer-new-from-drawable new-draw new-image)))
					(gimp-image-add-layer new-image VignetteLayer -1)
					(gimp-selection-all new-image)     
					(gimp-edit-clear VignetteLayer)  
					; calculate outer selection and fill with black
					(gimp-ellipse-select new-image (/ (- TheSize (* CircleSize .9)) 2) (/ (- TheSize (* CircleSize .9)) 2) (* CircleSize .9) (* CircleSize .9) 2 TRUE TRUE (* CircleSize .2))
					(gimp-selection-invert new-image)
					(gimp-edit-fill VignetteLayer BACKGROUND-FILL)
					(gimp-selection-none new-image)
					(gimp-drawable-set-name VignetteLayer "Shading")
					(gimp-layer-set-opacity VignetteLayer 70)
					(gimp-layer-set-mode VignetteLayer OVERLAY-MODE)
					; add inverted mask to vignette
					(gimp-layer-add-mask VignetteLayer VignetteMask)
					(gimp-floating-sel-anchor (car (gimp-edit-paste VignetteMask FALSE)))
					(gimp-invert VignetteMask)
					(gimp-layer-remove-mask VignetteLayer MASK-APPLY)
				)
			)

			; add inverted mask to original layer
			(gimp-layer-add-mask new-draw CircleMask)
			(gimp-floating-sel-anchor (car (gimp-edit-paste CircleMask FALSE)))
			(gimp-invert CircleMask)
			(gimp-layer-remove-mask new-draw MASK-APPLY)
			(gimp-image-delete MaskImage)
		)

		; position and label circle layer
		(gimp-image-raise-layer-to-top new-image new-draw)
		(gimp-drawable-set-name new-draw "Circle")

		; add drop shadow & order layers
		(if (not (= InShade 0)) 
			(begin
				(script-fu-drop-shadow new-image new-draw (/ CircleSize 32) (/ CircleSize 32) (/ CircleSize 32) '(0 0 0) 80 FALSE)
				(gimp-image-raise-layer-to-top new-image VignetteLayer)
			)
		)

		; return to original select state & color palette, flatten image if needed
		(if (= from-selection FALSE) (gimp-selection-none InImage))
		(gimp-context-set-foreground Old-FG-Color)
		(gimp-context-set-background Old-BG-Color)
		(if (= InFlatten TRUE) 
			(begin
				(gimp-image-merge-visible-layers new-image 1)
				(set! new-draw (car (gimp-image-get-active-layer new-image)))
				(gimp-drawable-set-name new-draw "Amazing Circle")
			)
		)
	)
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
)

(script-fu-register 
	"script-fu-Darla-AmazingCircles"
	"<Image>/Script-F_u/_Darla/_Amazing Circles"
	"Amazing Circle technique with advanced options.  \n\
Includes options to choose border colour (or calculate from \
border average), border size and vignetting effect, \
whirl/pinch distortion, alternate circle method, and \
to add a drop shadow & shading.  \n\
Will use the whole image if there is no active selection; \
however, a square or circular selection usually gives the \
best results. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
      "RGB* GRAY*"
	SF-IMAGE		"The Image"			0
	SF-DRAWABLE		"The Layer"			0
	SF-ADJUSTMENT	_"Maximum Final Size (sq)"	'(1000 300 2000 1 0 0 0)
	SF-OPTION		_"Border (Color or Method)" 	'("White"
									"Black"
									"Border Average"
									"Current Foreground"
									"Current Background")
	SF-ADJUSTMENT	_"Border average size (if selected):"	'(5 1 20 1 0 0 0)
	SF-ADJUSTMENT	_"Border: Percent of Final"	'(4 1 20 1 0 0 0)
	SF-ADJUSTMENT	_"Border: Grow Amount"		'(3 1 100 1 0 0 0)
	SF-ADJUSTMENT	_"Border: Feather Amount"	'(3 1 100 1 0 0 0)
	SF-TOGGLE		_"Add Shadow & Shading"		FALSE
	SF-TOGGLE		_"Modify with Whirl/Pinch"	FALSE
	SF-TOGGLE		_"Alternate Circle Method"	FALSE
	SF-TOGGLE		_"Flatten image"			FALSE
)
