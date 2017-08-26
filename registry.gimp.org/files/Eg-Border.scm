;
; Borders, V2.8
;
; Martin Egger (martin.egger@gmx.net)
; (C) 2012, Bern, Switzerland
;
; This script was tested with Gimp 2.8
;
; New versions will be distributed from http://registry.gimp.org/ only
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses>.
;
; Define the function
;
(define (script-fu-Eg-Border InImage InLayer InOuterPercentWidth InSymmetric InPortrait InInnerPercent InOuterColor InInnerColor InFeather)
;
	(let*	(
		(TheImage (car (gimp-image-duplicate InImage)))
		(TheLayer (car (gimp-image-flatten TheImage)))
		(isrgb (car (gimp-drawable-is-rgb TheLayer)))
		(TheWidth (car (gimp-image-width TheImage)))
		(TheHeight (car (gimp-image-height TheImage)))
;
; W-Border = % of widht, H-Border = % of height (asymmetric)
;
		(outer-border-width (/ (* TheWidth InOuterPercentWidth) 100))
		(outer-border-top-height (/ (* TheHeight InOuterPercentWidth) 100))
;
; W-Border and H-Border = % of widht (symmetric)
;
		(outer-border-top-height (+ (* outer-border-width InSymmetric) (* outer-border-top-height (* -1 (- InSymmetric 1)))))
		(outer-border-bottom-height outer-border-top-height)
;
; W-Border and Top-H-Border = % of widht, Bottom-H-Border = (1.41*(TheWidth+2*Border) - Border - TheHeight) (asymmetric)
;
		(outer-border-top-height (+ (* outer-border-width InPortrait) (* outer-border-top-height (* -1 (- InPortrait 1)))))
		(temp-bottom (- (* 1.414214 (+ TheWidth (* 2 outer-border-width))) TheHeight outer-border-top-height))
		(outer-border-bottom-height (+ (* temp-bottom InPortrait) (* outer-border-top-height (* -1 (- InPortrait 1)))))
;
		(inner-border-width (/ (* (+ (* TheHeight InPortrait) (* TheWidth (* -1 (- InPortrait 1)))) InInnerPercent) 100))
;
		(image-width (+ TheWidth  (* 2 outer-border-width)))
		(image-height (+ TheHeight outer-border-bottom-height outer-border-top-height))
		(Old-FG-Color (car (gimp-context-get-foreground)))
		)
		(cond
			((= isrgb FALSE) (gimp-image-convert-rgb TheImage))
		)
		(gimp-image-undo-disable TheImage)
		(gimp-selection-none TheImage)
		(gimp-item-set-name TheLayer "WithBorder")
;
; Generate the border
;
		(gimp-image-resize TheImage image-width image-height outer-border-width outer-border-top-height)
;
		(let*	(
			(BorderLayer (car (gimp-layer-new TheImage image-width image-height RGBA-IMAGE "TempLayer" 100 NORMAL-MODE)))
			)
			(gimp-image-insert-layer TheImage BorderLayer 0 -1)
			(gimp-edit-clear BorderLayer)
;
			(gimp-context-set-feather FALSE)
;
			(gimp-image-select-rectangle TheImage CHANNEL-OP-REPLACE 0 0 image-width image-height)
			(gimp-image-select-rectangle TheImage CHANNEL-OP-SUBTRACT outer-border-width outer-border-top-height TheWidth TheHeight) 
			(gimp-context-set-foreground InOuterColor)
			(gimp-edit-fill BorderLayer FOREGROUND-FILL) 
;
			(cond
				((> InInnerPercent 0) 
					(begin
						(gimp-context-set-feather InFeather)
						(gimp-context-set-feather-radius (* 1.4 inner-border-width) (* 1.4 inner-border-width))
						(gimp-image-select-rectangle TheImage CHANNEL-OP-REPLACE (- outer-border-width inner-border-width) (- outer-border-top-height inner-border-width) (+ TheWidth (* inner-border-width 2)) (+ TheHeight (* inner-border-width 2)))
						(gimp-context-set-feather FALSE)
						(gimp-image-select-rectangle TheImage CHANNEL-OP-SUBTRACT outer-border-width outer-border-top-height TheWidth TheHeight) 
						(gimp-context-set-foreground InInnerColor)
						(gimp-edit-fill BorderLayer FOREGROUND-FILL)
					)
				)
			)
			(gimp-image-merge-down TheImage BorderLayer CLIP-TO-IMAGE)
		)
;
		(gimp-selection-none TheImage)
		(gimp-display-new TheImage)
		(gimp-image-undo-enable TheImage)
		(gimp-context-set-foreground Old-FG-Color)
	)
;
; Finish work
;
	(gimp-displays-flush)
;
)
;
; Register the function with the GIMP
;
(script-fu-register
	"script-fu-Eg-Border"
	_"_Border"
	"Generate a border around an image"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"28.12.2012"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-ADJUSTMENT 	"Outer border size (width in percent)" '(18 1.0 100 1.0 0 2 0)
	SF-TOGGLE "Use symmetric outer borders" FALSE
	SF-TOGGLE "Use portrait style border" FALSE
	SF-ADJUSTMENT	"Inner border size (in percent)" '(0.20 0.0 10.0 0.1 0 2 0)
	SF-COLOR "Outer border color" '(255 255 255)
	SF-COLOR "Inner border color" '(0 0 0)
	SF-TOGGLE "Feather inner border" FALSE
)
;
(script-fu-menu-register "script-fu-Eg-Border"
			 "<Image>/Filters/Eg")
;
