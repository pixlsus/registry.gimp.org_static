;
; Copyright, V2.8
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
(define (script-fu-Eg-Copyright InImage InLayer InText InFont InPercent InReserve InOpacity InColorPre InColor InPosition InBlur InFlatten)
;	
; Save history
;
	(gimp-image-undo-group-start InImage)
	(if (= (car (gimp-drawable-is-rgb InLayer)) FALSE ) (gimp-image-convert-rgb InImage))
;
	(let*	(
		(TheWidth (car (gimp-image-width InImage)))
		(TheHeight (car (gimp-image-height InImage)))
		(Old-FG-Color (car (gimp-context-get-foreground)))
		(FontSize (/ (* TheHeight InPercent) 100))
		(BlurSize (* FontSize 0.07))
		(text-size (gimp-text-get-extents-fontname InText FontSize PIXELS InFont))
		(text-width (car text-size))
		(text-height (cadr text-size))
		(reserve-width (/ (* TheWidth InReserve) 100))
		(reserve-height (/ (* TheHeight InReserve) 100))
		(text-x 0)
		(text-y 0)
		)
;
; Generate copyright text on the image
;
; Select the text color
;
		(cond
;
; Gray
;
			((= InColorPre 0) (gimp-context-set-foreground '(127 127 127)))
;
; Black
;
			((= InColorPre 1) (gimp-context-set-foreground '(15 15 15)))
;
; White
;
			((= InColorPre 2) (gimp-context-set-foreground '(240 240 240)))
;
; Selection
;
			((= InColorPre 3) (gimp-context-set-foreground InColor))
		)
;
;	Select position
;
		(cond 
;
;	Bottom right
;
			((= InPosition 0) 
				(begin
					(set! text-x (- TheWidth (+ text-width reserve-width)))
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Bottom left
;
			((= InPosition 1) 
				(begin
					(set! text-x reserve-width)
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Bottom center
;
			((= InPosition 2)
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Top right
;
			((= InPosition 3) 
				(begin
					(set! text-x (- TheWidth (+ text-width reserve-width)))
					(set! text-y reserve-height)
				)
			)
;
;	Top left
;
			((= InPosition 4) 
				(begin
					(set! text-x reserve-width)
					(set! text-y reserve-height)
				)
			)
;
;	Top center
;
			((= InPosition 5) 
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y reserve-height)
				)
			)
;
;	Image center
;
			((= InPosition 6) 
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y (/ (- TheHeight text-height) 2))
				)
			)
		)
;
		(let*	(
			(TextLayer (car (gimp-text-fontname InImage -1 text-x text-y InText -1 TRUE FontSize PIXELS InFont)))
			)
			(gimp-layer-set-opacity TextLayer InOpacity)
;
; Blur the text, if we need to
;
			(if (= InBlur TRUE) (plug-in-gauss TRUE InImage TextLayer BlurSize BlurSize TRUE))
;
; Flatten the image, if we need to
;
			(cond
				((= InFlatten TRUE) (gimp-image-merge-down InImage TextLayer CLIP-TO-IMAGE))
				((= InFlatten FALSE) 
					(begin
						(gimp-item-set-name TextLayer "Copyright")
						(gimp-image-set-active-layer InImage InLayer)
					)
				)
			)
		)
		(gimp-context-set-foreground Old-FG-Color)
	)
;
; Finish work
;
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
;
)
;
; Register the function with the GIMP
;
(script-fu-register
	"script-fu-Eg-Copyright"
	_"C_opyright"
	"Generate a copyright mark on an image"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"28.02.2012"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-STRING 	"Copyright" "\302\251 M. Egger, 2012"
	SF-FONT 	"Font" "Arial Bold"
	SF-ADJUSTMENT 	"Text Height (Percent of image height)" '(10 1.0 100 1.0 0 2 0)
	SF-ADJUSTMENT	"Distance from border (Percent of image height)" '(3 0.0 10 1.0 0 2 0)
	SF-ADJUSTMENT	"Layer Opacity" '(60.0 1.0 100.0 1.0 0 2 0)
	SF-OPTION	"Copyright color (preset)" '("Gray"
				"Black"
				"White"
				"Color from selection")
	SF-COLOR 	"Copyright color (selection)" '(220 220 220)
	SF-OPTION 	"Copyright position" '("Bottom right"
				"Bottom left"
				"Bottom center"
				"Top right"
				"Top left"
				"Top center"
				"Image center")
	SF-TOGGLE 	"Blur copyright" TRUE
	SF-TOGGLE	"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-Copyright"
			 "<Image>/Filters/Eg")
;
