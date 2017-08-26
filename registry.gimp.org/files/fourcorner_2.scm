; This script is intended for use with
; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; 4-corner Gradient
; This script generates a new image containing a gradient based on the colors of the four corner points.
; Copyright (C) 2009 Charles Belov
; docorbit@sonic.net
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
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Version history
; 1.2 Correct menu location to File > Create > Gradients.
; 2.0 Choose most efficient direction for generation.
;     Support triangular repetition.
; 2.2 Document parameters in layer name.
; 2.4 Rearrange layer name to make parameters more obvious.
;     Show colors in Web hex.

; Convert color to hex
; These 3 lines of code courtesy Saul Goode http://registry.gimp.org/node/15346#comment-2789
(define (byte->hexstr x)
(define (nyb->hex x) (if (> x 9) (integer->char (+ 87 x)) (integer->char (+ 48 x))))  ; 55 for uppercase, 87 for lowercase
(string-append (make-string 1 (nyb->hex (trunc (/ x 16)))) (make-string 1 (nyb->hex (modulo x 16)))))
; End convert color to hex

(define (script-fu-four-corner-grad-2-4 width height ulColor urColor llColor lrColor horzBlend horzCycling vertBlend vertCycling execSequence)

	(let* (
			(layerName 
				(string-append 
					(byte->hexstr (car ulColor))
					(byte->hexstr (cadr ulColor))
					(byte->hexstr (caddr ulColor))
					" "
					(byte->hexstr (car urColor))
					(byte->hexstr (cadr urColor))
					(byte->hexstr (caddr urColor))
					" "
					(byte->hexstr (car llColor))
					(byte->hexstr (cadr llColor))
					(byte->hexstr (caddr llColor))
					" "
					(byte->hexstr (car lrColor))
					(byte->hexstr (cadr lrColor))
					(byte->hexstr (caddr lrColor))
					", "
					(case horzBlend
; _"RGB" _"HSV"
						((0) "RGB")
						((1) "HSV")
						(else "error")
					)
					", "
					(case horzCycling
; _"No cycling" _"1 (make horizontally tileable)" _"1 1/2 (retain rightmost color)" _"2 (make horizontally tileable)" _"2 1/2 (retain rightmost color)"
						((0) "no cycling")
						((1) "1 cycle")
						((2) "1 1/2 cycles")
						((3) "2 cycles")
						((4) "2 1/2 cycles")
						(else "error")
					)
					", "
					(case vertBlend
; _"RGB" _"HSV"
						((0) "RGB")
						((1) "HSV")
						(else "error")
					)
					", "
					(case vertCycling
; _"No cycling" _"1 (make vertically tileable)" _"1 1/2 (retain bottom color)" _"2 (make vertically tileable)" _"2 1/2 (retain bottom color)"
						((0) "no cycling")
						((1) "1 cycle")
						((2) "1 1/2 cycles")
						((3) "2 cycles")
						((4) "2 1/2 cycles")
						(else "error")
					)
					", "
					(case execSequence
; _"Faster" _"Top/bottom edges first" _"Left/right edges first"
						((0) "faster")
						((1) "top/bottom edges first")
						((2) "left/right edges first")
						(else "error")
					)
					". "
					"4-corner Gradient v2.4"
				)
			)
			(img (car (gimp-image-new width height RGB)))
			(layer (car (gimp-layer-new img width height RGB layerName 100 NORMAL)))
			(colX 0)
			(rowY 0)
			(cycleWidth width)
			(cycleHeight height)
			(horzRepeat REPEAT-NONE)
			(vertRepeat REPEAT-NONE)
			(horzFirst 1)
		  )
		
		(if (< 0 horzCycling)
			(begin
				(set! horzRepeat REPEAT-TRIANGULAR)
				(set! cycleWidth (/ width (+ horzCycling 1)))
			)
		)
		
		(if (< 0 vertCycling)
			(begin
				(set! vertRepeat REPEAT-TRIANGULAR)
				(set! cycleHeight (/ height (+ vertCycling 1)))
			)
		)
		
		(gimp-context-push)
		(gimp-image-undo-group-start img)
		(gimp-image-add-layer img layer 0)
		
; Select most efficient direction unless overridden
		(if (< height width)
			(set! horzFirst 0)
		)
		(if (> execSequence 0)
			(set! horzFirst 1)
		)
		(if (> execSequence 1)
			(set! horzFirst 0)
		)

		(if (> horzFirst 0)
			(begin
; Main routine via width
; blend in first row
				(gimp-context-set-foreground ulColor)
				(gimp-context-set-background urColor)
				(gimp-rect-select img 0 0 width 1 CHANNEL-OP-REPLACE 0 0)
				(gimp-edit-blend layer horzBlend NORMAL-MODE GRADIENT-LINEAR 100 0 horzRepeat FALSE FALSE 0 0 TRUE 0 0 (- cycleWidth 1) 0)
 
; blend in last row
				(gimp-context-set-foreground llColor)
				(gimp-context-set-background lrColor)
				(gimp-rect-select img 0 (- height 1) width 1 CHANNEL-OP-REPLACE 0 0)
				(gimp-edit-blend layer horzBlend NORMAL-MODE GRADIENT-LINEAR 100 0 horzRepeat FALSE FALSE 0 0 TRUE 0 (- height 1) (- cycleWidth 1) (- height 1))

; blend in each column		
				(while (< colX width)
					(gimp-context-set-foreground (car (gimp-image-pick-color img layer colX 0 FALSE FALSE 0)))
					(gimp-context-set-background (car (gimp-image-pick-color img layer colX (- height 1) FALSE FALSE 0)))
					(gimp-rect-select img colX 0 1 height CHANNEL-OP-REPLACE 0 0)
					(gimp-edit-blend layer vertBlend NORMAL-MODE GRADIENT-LINEAR 100 0 vertRepeat FALSE FALSE 0 0 TRUE colX 0 colX (- cycleHeight 1))
					(set! colX (+ colX 1))
				)
; end main routine via width
			)
			(begin
; Main routine via height
; blend in first column
				(gimp-context-set-foreground ulColor)
				(gimp-context-set-background llColor)
				(gimp-rect-select img 0 0 1 height CHANNEL-OP-REPLACE 0 0)
				(gimp-edit-blend layer vertBlend NORMAL-MODE GRADIENT-LINEAR 100 0 vertRepeat FALSE FALSE 0 0 TRUE 0 0 0 (- cycleHeight 1))
 
; blend in last column
				(gimp-context-set-foreground urColor)
				(gimp-context-set-background lrColor)
				(gimp-rect-select img (- width 1) 0 1 height CHANNEL-OP-REPLACE 0 0)
				(gimp-edit-blend layer vertBlend NORMAL-MODE GRADIENT-LINEAR 100 0 vertRepeat FALSE FALSE 0 0 TRUE (- width 1) 0 (- width 1) (- cycleHeight 1))

; blend in each row		
				(while (< rowY height)
					(gimp-context-set-foreground (car (gimp-image-pick-color img layer 0 rowY FALSE FALSE 0)))
					(gimp-context-set-background (car (gimp-image-pick-color img layer (- width 1) rowY FALSE FALSE 0)))
					(gimp-rect-select img 0 rowY width 1 CHANNEL-OP-REPLACE 0 0)
					(gimp-edit-blend layer horzBlend NORMAL-MODE GRADIENT-LINEAR 100 0 horzRepeat FALSE FALSE 0 0 TRUE 0 rowY (- cycleWidth 1) rowY)
					(set! rowY (+ rowY 1))
				)

; end main routine via height
			)
		)

		(gimp-image-undo-group-end img)
		(gimp-context-pop)
		(gimp-display-new img)

	)
)

(script-fu-register "script-fu-four-corner-grad-2-4"
                    "4-corner Gradient..."
					"This script generates a new image containing a gradient based on the colors of the four corner points."
					"Charles Belov <docorbit@sonic.net>"
					"Charles Belov"
					"2009-04-06"
					""
					SF-VALUE "Width" "100"
					SF-VALUE "Height" "100"
					SF-COLOR "Upper left color" '(255 0 0)
					SF-COLOR "Upper right color" '(0 0 255)
					SF-COLOR "Lower left color" '(0 255 0)
					SF-COLOR "Lower right color" '(255 255 0)
					SF-OPTION _"Horizontal color blend" '(_"RGB" _"HSV")
					SF-OPTION _"Horizontal cycles" '(_"No cycling" _"1 (make horizontally tileable)" _"1 1/2 (retain rightmost color)" _"2 (make horizontally tileable)" _"2 1/2 (retain rightmost color)")
					SF-OPTION _"Vertical color blend" '(_"RGB" _"HSV")
					SF-OPTION _"Vertical cycles" '(_"No cycling" _"1 (make vertically tileable)" _"1 1/2 (retain bottom color)" _"2 (make vertically tileable)" _"2 1/2 (retain bottom color)")
					SF-OPTION _"Sequence" '(_"Faster" _"Top/bottom edges first" _"Left/right edges first")
)

(script-fu-menu-register "script-fu-four-corner-grad-2-4" 
                         "<Image>/File/Create/Gradients")

