; This script is intended for use with
; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; 2-dimensional Gradient
; This script generates a new image containing a gradient based on the corners or edges of a rectangle.
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
; 1.0 Create 2-D gradient script-fu from merging 4-corner gradient and gradient-to-gradient scripts-fu
; 1.2 Correct problem of non-English GIMP installations failing on attempting to load the RGB or HSV gradients by name

; Convert number 0-255 to hex
; byte->hexstr function courtesy Saul Goode http://registry.gimp.org/node/15346#comment-2789
(define (byte->hexstr x)
	(define (nyb->hex y) 
		(if (> y 9) 
			(integer->char (+ 87 y))  ; 55 for uppercase, 87 for lowercase
			(integer->char (+ 48 y)) 
		)
	)  
	(string-append 
		(make-string 1 (nyb->hex (trunc (/ x 16)))) 
		(make-string 1 (nyb->hex (modulo x 16)))
	)
)
; End convert number 0-255 to hex

; Convert 2-character hex to number
(define (hexstr->byte x)
	(define (hex->nyb y)
		(case (char->integer y) 
			((48 49 50 51 52 53 54 55 56 57) (- (char->integer y) 48))
			((65 66 67 68 69 70) (- (char->integer y) 55))
			((97 98 99 100 101 102) (- (char->integer y) 87))
			(else 256)
		)	
	)
	(+ (* (hex->nyb (string-ref x 0)) 16) (hex->nyb (string-ref x 1)))
)
; End convert 2-character hex to number

(define (script-fu-2d-grad-1-2 img existingLayer paramSource tlColor trColor blColor brColor tGradient bGradient lGradient rGradient fillBlend tMethod bMethod lMethod rMethod fillMethod edgeRepeatRule allowImgInRandom)
	

;*************************************************************************************************************************
;
;  Randomize options on request
;
;*************************************************************************************************************************

	(if (or (= paramSource 1) (= paramSource 2))
		(begin
			(set! tlColor (list (rand 255) (rand 255) (rand 255)))
			(set! blColor (list (rand 255) (rand 255) (rand 255)))
			(set! trColor (list (rand 255) (rand 255) (rand 255)))
			(set! brColor (list (rand 255) (rand 255) (rand 255)))
		)
	)
	(if (or (= paramSource 1) (= paramSource 4))
		(begin
			(set! fillBlend (rand 3))
			(if (= allowImgInRandom TRUE)
				(begin
					(set! tMethod (rand 6))
					(set! bMethod (rand 6))
					(set! lMethod (rand 6))
					(set! rMethod (rand 6))
				)
				(begin
					(set! tMethod (rand 5))
					(set! bMethod (rand 5))
					(set! lMethod (rand 5))
					(set! rMethod (rand 5))
				)
			)
			(set! fillMethod (rand 4))
		)
	)
	(if (or (= paramSource 1) (= paramSource 3))
		(begin
;			(if (or (= tMethod 3) (= tMethod 4) (= bMethod 3) (= bMethod 4) (= lMethod 3) (= lMethod 4) (= rMethod 3) (= rMethod 4))
;				(begin
					(let* (
							(gradientData (gimp-gradients-get-list ""))
							(gradientList (cadr gradientData))
							(gradientCount (car gradientData))
						)
;						(if (not (= fillMethod 1))
;							(if (or (= tMethod 3) (= tMethod 4))
								(set! tGradient (list-ref gradientList (rand gradientCount)))
;							)
;							(if (or (= bMethod 3) (= bMethod 4))
								(set! bGradient (list-ref gradientList (rand gradientCount)))
;							)
;						)
;						(if (> fillMethod 0) 
;							(if (or (= lMethod 3) (= lMethod 4))
								(set! lGradient (list-ref gradientList (rand gradientCount)))
;							)
;							(if (or (= rMethod 3) (= rMethod 4))
								(set! rGradient (list-ref gradientList (rand gradientCount)))
;							)
;						)
					)
;				)						
;			)
			(let* (
					(edgeRepeatCountList '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 4 4 5 6 7 8 9))
					(edgeRepeatTypeList '("S" "S" "S" "T"))
				)
				(set! edgeRepeatRule
					(string-append
						(number->string (list-ref edgeRepeatCountList (rand (length edgeRepeatCountList))))
						(list-ref edgeRepeatTypeList (rand (length edgeRepeatTypeList)))							
						" "
						(number->string (list-ref edgeRepeatCountList (rand (length edgeRepeatCountList))))
						(list-ref edgeRepeatTypeList (rand (length edgeRepeatTypeList)))							
						" "
						(number->string (list-ref edgeRepeatCountList (rand (length edgeRepeatCountList))))
						(list-ref edgeRepeatTypeList (rand (length edgeRepeatTypeList)))							
						" "
						(number->string (list-ref edgeRepeatCountList (rand (length edgeRepeatCountList))))
						(list-ref edgeRepeatTypeList (rand (length edgeRepeatTypeList)))
					)
				)
			)
		)
	)


;*************************************************************************************************************************
;
;  Retrieve options from current 2DG layer on request
;
;*************************************************************************************************************************
	
	(if (or (= paramSource 5) (= paramSource 6) (= paramSource 7) (= paramSource 8))
		(begin
			(let* (
					(existingLayerName (car (gimp-drawable-get-name existingLayer)))
					(parseError FALSE)
					(tempTlColor '(999 999 999))
					(tempBlColor '(999 999 999))
					(tempTrColor '(999 999 999))
					(tempBrColor '(999 999 999))
				)

;
;; .2dg-a 3ab567 bf7d1a 2915d3 407428; RGB; CCW, CCW, <Gr, CCW; /up; 0T 0S 0S 5S; 'Full saturation spectrum CW', 'Golden', 'Tropical Colors', 'Pastels'.  
;; (remember end position for substring command is exclusive, not inclusive)
;;
;;
				(if (equal? (substring existingLayerName 0 4) ".2dg")
					()
					(set! parseError TRUE)
				)
				(if (equal? (substring existingLayerName 4 6) "-a")
					()
					(set! parseError TRUE)
				)
				(if (= parseError FALSE)
					(begin
						(if (or (= paramSource 5) (= paramSource 6))
							(begin
								(set! tempTlColor (list (hexstr->byte (substring existingLayerName 7 9)) (hexstr->byte (substring existingLayerName 9 11)) (hexstr->byte (substring existingLayerName 11 13))))
								(set! tempTrColor (list (hexstr->byte (substring existingLayerName 14 16)) (hexstr->byte (substring existingLayerName 16 18)) (hexstr->byte (substring existingLayerName 18 20))))
								(set! tempBlColor (list (hexstr->byte (substring existingLayerName 21 23)) (hexstr->byte (substring existingLayerName 23 25)) (hexstr->byte (substring existingLayerName 25 27))))
								(set! tempBrColor (list (hexstr->byte (substring existingLayerName 28 30)) (hexstr->byte (substring existingLayerName 30 32)) (hexstr->byte (substring existingLayerName 32 34))))
								(if (and (< (car tempTlColor) 256) (< (cadr tempTlColor) 256) (< (caddr tempTlColor) 256))
									(set! tlColor tempTlColor)
									(set! parseError TRUE) 
								)
								(if (and (< (car tempTrColor) 256) (< (cadr tempTrColor) 256) (< (caddr tempTrColor) 256))
									(set! trColor tempTrColor)
									(set! parseError TRUE) 
								)
								(if (and (< (car tempBlColor) 256) (< (cadr tempBlColor) 256) (< (caddr tempBlColor) 256))
									(set! blColor tempBlColor)
									(set! parseError TRUE) 
								)
								(if (and (< (car tempBrColor) 256) (< (cadr tempBrColor) 256) (< (caddr tempBrColor) 256))
									(set! brColor tempBrColor)
									(set! parseError TRUE) 
								)
							)
						)
						(if (or (= paramSource 5) (= paramSource 7))
							(begin
								(let* (
										(startPos 80)
										(endPos (string-length existingLayerName))
										(curPos (+ startPos 1))
										(tempTGradient "")
										(tempBGradient "")
										(tempLGradient "")
										(tempRGradient "")
									)
									(while (and  (> curPos startPos) (< curPos endPos)) 
										(if (equal? (substring existingLayerName curPos (+ curPos 4)) "', '")
											(begin
												(set! tempTGradient (substring existingLayerName startPos curPos))
												(set! startPos (+ curPos 4))
											)
											(set! curPos (+ curPos 1))
										)
									)
									(set! curPos (+ startPos 1))
									(while (and  (> curPos startPos) (< curPos endPos)) 
										(if (equal? (substring existingLayerName curPos (+ curPos 4)) "', '")
											(begin
												(set! tempBGradient (substring existingLayerName startPos curPos))
												(set! startPos (+ curPos 4))
											)
											(set! curPos (+ curPos 1))
										)
									)
									(set! curPos (+ startPos 1))
									(while (and  (> curPos startPos) (< curPos endPos)) 
										(if (equal? (substring existingLayerName curPos (+ curPos 4)) "', '")
											(begin
												(set! tempLGradient (substring existingLayerName startPos curPos))
												(set! startPos (+ curPos 4))
											)
											(set! curPos (+ curPos 1))
										)
									)
									(set! curPos (+ startPos 1))
									(while (and  (> curPos startPos) (< curPos endPos)) 
										(if (equal? (substring existingLayerName curPos (+ curPos 2)) "'.")
											(begin
												(set! tempRGradient (substring existingLayerName startPos curPos))
												(set! startPos (+ curPos 4))
											)
											(set! curPos (+ curPos 1))
										)
									)
									(set! tGradient tempTGradient)
									(set! bGradient tempBGradient)
									(set! lGradient tempLGradient)
									(set! rGradient tempRGradient)
								)
							)
						)
						(if (or (= paramSource 5) (= paramSource 8))
							(begin
								(case (string->symbol (substring existingLayerName 36 39))
									('RGB (set! fillBlend 0))
									('CW- (set! fillBlend 1))
									('CCW (set! fillBlend 2))
								)
								(case (string->symbol (substring existingLayerName 41 44))
									('RGB (set! tMethod 0))
									('CW- (set! tMethod 1))
									('CCW (set! tMethod 2))
									('>Gr (set! tMethod 3))
									('<Gr (set! tMethod 4))
									('Img (set! tMethod 5))
								)
								(case (string->symbol (substring existingLayerName 46 49))
									('RGB (set! bMethod 0))
									('CW- (set! bMethod 1))
									('CCW (set! bMethod 2))
									('>Gr (set! bMethod 3))
									('<Gr (set! bMethod 4))
									('Img (set! bMethod 5))
								)
								(case (string->symbol (substring existingLayerName 51 54))
									('RGB (set! lMethod 0))
									('CW- (set! lMethod 1))
									('CCW (set! lMethod 2))
									('>Gr (set! lMethod 3))
									('<Gr (set! lMethod 4))
									('Img (set! lMethod 5))
								)
								(case (string->symbol (substring existingLayerName 56 59))
									('RGB (set! rMethod 0))
									('CW- (set! rMethod 1))
									('CCW (set! rMethod 2))
									('>Gr (set! rMethod 3))
									('<Gr (set! rMethod 4))
									('Img (set! rMethod 5))
								)
								(case (string->symbol (substring existingLayerName 61 64))
									('T2B (set! fillMethod 0))
									('L2R (set! fillMethod 1))
									('/dn (set! fillMethod 2))
									('/up (set! fillMethod 3))
								)
								(set! edgeRepeatRule (substring existingLayerName 66 77))
							)
						)
					)
				)
			)
		)
	)

;*************************************************************************************************************************
;
;  Process options and create a new gradient layer
;
;*************************************************************************************************************************

	(let* (
			(tRepeatCount (- (char->integer (string-ref edgeRepeatRule 0)) 48))
			(tRepeatType REPEAT-NONE)
			(bRepeatCount (- (char->integer (string-ref edgeRepeatRule 3)) 48))
			(bRepeatType REPEAT-NONE)
			(lRepeatCount (- (char->integer (string-ref edgeRepeatRule 6)) 48))
			(lRepeatType REPEAT-NONE)
			(rRepeatCount (- (char->integer (string-ref edgeRepeatRule 9)) 48))
			(rRepeatType REPEAT-NONE)
			(gradientData (gimp-gradients-get-list ""))
			(gradientList (cadr gradientData))
			(layerName 
				(string-append
					".2dg-a "
;					(if (or (and (< tMethod 3) (not (= fillMethod 1))) (and (< lMethod 3) (< fillMethod 0)))
;						(begin
							(byte->hexstr (car tlColor))
							(byte->hexstr (cadr tlColor))
							(byte->hexstr (caddr tlColor))
;						)
;						(begin
;							"......"
;						)
;					)
					" "
;					(if (or (and (< tMethod 3) (not (= fillMethod 1))) (and (< rMethod 3) (< fillMethod 0)))
;						(begin
							(byte->hexstr (car trColor))
							(byte->hexstr (cadr trColor))
							(byte->hexstr (caddr trColor))
;						)
;						(begin
;							"......"
;						)
;					)
					" "
;					(if (or (and (< bMethod 3) (not (= fillMethod 1))) (and (< lMethod 3) (< fillMethod 0)))
;						(begin
							(byte->hexstr (car blColor))
							(byte->hexstr (cadr blColor))
							(byte->hexstr (caddr blColor))
;						)
;						(begin
;							"......"
;						)
;					)
					" "
;					(if (or (and (< bMethod 3) (not (= fillMethod 1))) (and (< rMethod 3) (< fillMethod 0)))
;						(begin
							(byte->hexstr (car brColor))
							(byte->hexstr (cadr brColor))
							(byte->hexstr (caddr brColor))
;						)
;						(begin
;							"......"
;						)
;					)
					"; "
					(case fillBlend
						((0) "RGB")
						((1) "CW-")
						((2) "CCW")
					)
					"; "
					(case tMethod
						((0) "RGB")
						((1) "CW-")
						((2) "CCW")
						((3) ">Gr")
						((4) "<Gr")
						((5) "Img")
					)
					", "
					(case bMethod
						((0) "RGB")
						((1) "CW-")
						((2) "CCW")
						((3) ">Gr")
						((4) "<Gr")
						((5) "Img")
					)
					", "
					(case lMethod
						((0) "RGB")
						((1) "CW-")
						((2) "CCW")
						((3) ">Gr")
						((4) "<Gr")
						((5) "Img")
					)
					", "
					(case rMethod
						((0) "RGB")
						((1) "CW-")
						((2) "CCW")
						((3) ">Gr")
						((4) "<Gr")
						((5) "Img")
					)
					"; "
					(case fillMethod
						((0) "T2B")
						((1) "L2R")
						((2) "/dn")
						((3) "/up")
					)
					"; "
					edgeRepeatRule
					"; '"
					tGradient
					"', '"
					bGradient
					"', '"
					lGradient
					"', '"
					rGradient
					"'. "
				)
			)
			(width (car (gimp-image-width img)))
			(height (car (gimp-image-height img)))
			(selectionBounds (gimp-selection-bounds img)) 
			(selectionX1 0)
			(selectionY1 0)
			(selectionX2 (- width 1))
			(selectionY2 (- height 1))
			(selectionWidth width)
			(selectionHeight height)
			(layer (car (gimp-layer-new img width height RGB layerName 100 NORMAL)))
			(colX 0)
			(rowY 0)
			(colX1 0)
			(rowY1 0)
			(colX2 0)
			(rowY2 0)
			(cycleWidth width)
			(cycleHeight height)
			(horzFirst 1)
			(origForegroundColor (car (gimp-context-get-foreground)))
			(origBackgroundColor (car (gimp-context-get-background)))
			(floatingLayer 0)
			(tReverse FALSE)
			(bReverse FALSE)
			(lReverse FALSE)
			(rReverse FALSE)
			(layerMask 0)
			(points (make-vector 8))
			(dither TRUE)
			(supersample FALSE)
		)

		(if (and (> tRepeatCount 0) (< tRepeatCount 10))
			(begin
				(case (string-ref edgeRepeatRule 1)
					((#\T) (set! tRepeatType REPEAT-TRIANGULAR))
					((#\S) (set! tRepeatType REPEAT-SAWTOOTH))
				)
			)
				(set! tRepeatCount 0)
		)
		(if (and (> bRepeatCount 0) (< bRepeatCount 10))
			(begin
				(case (string-ref edgeRepeatRule 4)
					((#\T) (set! bRepeatType REPEAT-TRIANGULAR))
					((#\S) (set! bRepeatType REPEAT-SAWTOOTH))
				)
			)
				(set! bRepeatCount 0)
		)
		(if (and (> lRepeatCount 0) (< lRepeatCount 10))
			(begin
				(case (string-ref edgeRepeatRule 7)
					((#\T) (set! lRepeatType REPEAT-TRIANGULAR))
					((#\S) (set! lRepeatType REPEAT-SAWTOOTH))
				)
			)
				(set! lRepeatCount 0)
		)
		(if (and (> rRepeatCount 0) (< rRepeatCount 10))
			(begin
				(case (string-ref edgeRepeatRule 10)
					((#\T) (set! rRepeatType REPEAT-TRIANGULAR))
					((#\S) (set! rRepeatType REPEAT-SAWTOOTH))
				)
			)
				(set! rRepeatCount 0)
		)
		
		(gimp-context-push)
		(gimp-image-undo-group-start img)
		(gimp-image-add-layer img layer (car (gimp-image-get-layer-position img existingLayer)))
		(gimp-layer-add-alpha layer)

		(if (= (car selectionBounds) TRUE)
			(begin
				(set! selectionX1 (cadr selectionBounds))
				(set! selectionY1 (caddr selectionBounds))
				(set! selectionX2 (- (cadr (cddr selectionBounds)) 1))
				(set! selectionY2 (- (caddr (cddr selectionBounds)) 1))
				(set! selectionWidth (+ (- selectionX2 selectionX1) 1))
				(set! selectionHeight (+ (- selectionY2 selectionY1) 1))
				(set! cycleWidth selectionWidth)
				(set! cycleHeight selectionHeight)
				(set! colX selectionX1)
				(set! rowY selectionY1)
				(set! layerMask (car (gimp-layer-create-mask layer ADD-SELECTION-MASK)))
			)
		)

		(gimp-selection-all img)
		(gimp-edit-clear layer)
						
		(if (or (< fillMethod 1) (> fillMethod 1))
; anything but left to right (for now)
			(begin
; draw top row
				(if (< tMethod 3)
					(begin
						(gimp-context-set-foreground tlColor)
						(gimp-context-set-background trColor)
;						(gimp-message "Debug: Set TLC, TRC")
					)
					(begin
						(gimp-context-set-foreground origForegroundColor)
						(gimp-context-set-background origBackgroundColor)
;						(gimp-message "Debug: Set original colors")
					)
				)
				(case tMethod
					((0) (gimp-context-set-gradient (list-ref gradientList 2)))
					((1) (gimp-context-set-gradient (list-ref gradientList 0)))
					((2) (gimp-context-set-gradient (list-ref gradientList 1)))
					((3) (gimp-context-set-gradient tGradient))
					((4) (begin
							(gimp-context-set-gradient tGradient)
							(set! tReverse TRUE)
						))
; 5 don't set gradient
				)
				(if (> tRepeatCount 0)
					(set! cycleWidth (inexact->exact (round (/ selectionWidth (+ tRepeatCount 1)))))
					(set! cycleWidth selectionWidth)
				)			
				(gimp-rect-select img selectionX1 selectionY1 selectionWidth 1 CHANNEL-OP-REPLACE 0 0)
				(if (= tMethod 5)
					(begin
						(gimp-edit-copy-visible img)
						(set! floatingLayer (car (gimp-edit-paste layer TRUE)))
						(gimp-floating-sel-anchor floatingLayer)
;						(gimp-message "Used current image")
					)
					(begin
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 tRepeatType tReverse supersample 5 2 dither selectionX1 selectionY1 (+ (- cycleWidth 1) selectionX1) selectionY1)
;						(gimp-message (string-append "Debug: Used gradient " (car (gimp-context-get-gradient))))
					)
				)
; draw bottom row
				(if (< bMethod 3)
					(begin
						(gimp-context-set-foreground blColor)
						(gimp-context-set-background brColor)
;						(gimp-message "Debug: Set BLC, BRC")
					)
					(begin
						(gimp-context-set-foreground origForegroundColor)
						(gimp-context-set-background origBackgroundColor)
;						(gimp-message "Debug: Set original colors")
					)
				)
				(case bMethod
					((0) (gimp-context-set-gradient (list-ref gradientList 2)))
					((1) (gimp-context-set-gradient (list-ref gradientList 0)))
					((2) (gimp-context-set-gradient (list-ref gradientList 1)))
					((3) (gimp-context-set-gradient bGradient))
					((4) (begin
							(gimp-context-set-gradient bGradient)
							(set! bReverse TRUE)
						))
; 5 don't set gradient
				)
				(if (> bRepeatCount 0)
					(set! cycleWidth (inexact->exact (round (/ selectionWidth (+ bRepeatCount 1)))))
					(set! cycleWidth selectionWidth)
				)			
				(gimp-rect-select img selectionX1 selectionY2 selectionWidth 1 CHANNEL-OP-REPLACE 0 0)
				(if (= bMethod 5)
					(begin
						(gimp-edit-copy-visible img)
						(set! floatingLayer (car (gimp-edit-paste layer TRUE)))
						(gimp-floating-sel-anchor floatingLayer)
;						(gimp-message "Debug: Used current image")
					)
					(begin
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 bRepeatType bReverse supersample 5 2 dither selectionX1 selectionY2 (+ (- cycleWidth 1) selectionX1) selectionY2)
;						(gimp-message (string-append "Debug: Used gradient " (car (gimp-context-get-gradient))))
					)
				)
			)
		)
		
		(if (> fillMethod 0)
; anything but top to bottom (for now)
			(begin
; draw left column
				(if (< lMethod 3)
					(begin
						(gimp-context-set-foreground tlColor)
						(gimp-context-set-background blColor)
;						(gimp-message "Debug: Set TLC, BLC")
					)
					(begin
						(gimp-context-set-foreground origForegroundColor)
						(gimp-context-set-background origBackgroundColor)
;						(gimp-message "Debug: Set original colors")
					)
				)
				(case lMethod
					((0) (gimp-context-set-gradient (list-ref gradientList 2)))
					((1) (gimp-context-set-gradient (list-ref gradientList 0)))
					((2) (gimp-context-set-gradient (list-ref gradientList 1)))
					((3) (gimp-context-set-gradient lGradient))
					((4) (begin
							(gimp-context-set-gradient lGradient)
							(set! lReverse TRUE)
						))
; 5 don't set gradient
				)
				(if (> lRepeatCount 0)
					(set! cycleHeight (inexact->exact (round (/ selectionHeight (+ lRepeatCount 1)))))
					(set! cycleHeight selectionHeight)
				)			
				(gimp-rect-select img selectionX1 selectionY1 1 selectionHeight CHANNEL-OP-REPLACE 0 0)
				(if (= lMethod 5)
					(begin
						(gimp-edit-copy-visible img)
						(set! floatingLayer (car (gimp-edit-paste layer TRUE)))
						(gimp-floating-sel-anchor floatingLayer)
;						(gimp-message "Used current image")
					)
					(begin
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 lRepeatType lReverse supersample 5 2 dither selectionX1 selectionY1 selectionX1 (+ (- cycleHeight 1) selectionY1))
;						(gimp-message (string-append "Debug: Used gradient " (car (gimp-context-get-gradient))))
					)
				)
; draw right column
				(if (< rMethod 3)
					(begin
						(gimp-context-set-foreground trColor)
						(gimp-context-set-background brColor)
;						(gimp-message "Debug: Set TRC, BRC")
					)
					(begin
						(gimp-context-set-foreground origForegroundColor)
						(gimp-context-set-background origBackgroundColor)
;						(gimp-message "Debug: Set original colors")
					)
				)
				(case rMethod
					((0) (gimp-context-set-gradient (list-ref gradientList 2)))
					((1) (gimp-context-set-gradient (list-ref gradientList 0)))
					((2) (gimp-context-set-gradient (list-ref gradientList 1)))
					((3) (gimp-context-set-gradient rGradient))
					((4) (begin
							(gimp-context-set-gradient rGradient)
							(set! rReverse TRUE)
						))
; 5 don't set gradient
				)
				(if (> rRepeatCount 0)
					(set! cycleHeight (inexact->exact (round (/ selectionHeight (+ rRepeatCount 1)))))
					(set! cycleHeight selectionHeight)
				)			
				(gimp-rect-select img selectionX2 selectionY1 1 selectionHeight CHANNEL-OP-REPLACE 0 0)
				(if (= rMethod 5)
					(begin
						(gimp-edit-copy-visible img)
						(set! floatingLayer (car (gimp-edit-paste layer TRUE)))
						(gimp-floating-sel-anchor floatingLayer)
;						(gimp-message "Debug: Used current image")
					)
					(begin
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 rRepeatType rReverse supersample 5 2 dither selectionX2 selectionY1 selectionX2 (+ (- cycleHeight 1) selectionY1))
;						(gimp-message (string-append "Debug: Used gradient " (car (gimp-context-get-gradient))))
					)
				)
			)
		)

		
; fill logic		
		(case fillBlend
			((0) (gimp-context-set-gradient (list-ref gradientList 2)))
			((1) (gimp-context-set-gradient (list-ref gradientList 0)))
			((2) (gimp-context-set-gradient (list-ref gradientList 1)))
		)					
		(case fillMethod
; blend in each column
			((0)
				(begin
					(while (< colX (+ selectionX2 1))
						(gimp-context-set-foreground (car (gimp-image-pick-color img layer colX selectionY1 FALSE FALSE 0)))
						(gimp-context-set-background (car (gimp-image-pick-color img layer colX selectionY2 FALSE FALSE 0)))
						(gimp-rect-select img colX selectionY1 1 selectionHeight CHANNEL-OP-REPLACE 0 0)
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE supersample 5 2 dither colX selectionY1 colX (+ (- cycleHeight 1) selectionY1))
						(set! colX (+ colX 1))
					)
				)
			)
; blend in each row
			((1)
				(begin
					(while (< rowY (+ selectionY2 1))
						(gimp-context-set-foreground (car (gimp-image-pick-color img layer selectionX1 rowY FALSE FALSE 0)))
						(gimp-context-set-background (car (gimp-image-pick-color img layer selectionX2 rowY FALSE FALSE 0)))
						(gimp-rect-select img selectionX1 rowY selectionWidth 1 CHANNEL-OP-REPLACE 0 0)
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE supersample 5 2 dither selectionX1 rowY (+ (- cycleWidth 1) selectionX1) rowY)
						(set! rowY (+ rowY 1))
					)
				)
			)
			((2) ; falling diagonal
				(begin
					(set! colX1 (- selectionX2 2))
					(set! rowY1 selectionY1)
					(set! colX2 selectionX2)
					(set! rowY2 (+ selectionY1 2))
					(while (> colX2 (+ selectionX1 1))
						(vector-set! points 0 (+ colX1 0.15))
						(vector-set! points 1 (- rowY1 0.65))
						(vector-set! points 2 (- colX1 0.65))
						(vector-set! points 3 (+ rowY1 0.15))
						(vector-set! points 4 (- colX2 0.85))
						(vector-set! points 5 (+ rowY2 0.15))
						(vector-set! points 6 (+ colX2 0.15))
						(vector-set! points 7 (- rowY2 0.85))
						(gimp-free-select img 8 points CHANNEL-OP-REPLACE FALSE FALSE 0)
						(gimp-context-set-foreground (car (gimp-image-pick-color img layer colX1 rowY1 FALSE FALSE 0)))
						(gimp-context-set-background (car (gimp-image-pick-color img layer colX2 rowY2 FALSE FALSE 0)))
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE supersample 5 2 dither colX1 rowY1 colX2 rowY2)
						(if (< rowY2 selectionY2)
							(set! rowY2 (+ rowY2 1))
							(set! colX2 (- colX2 1))
						)		
						(if (> colX1 selectionX1)
							(set! colX1 (- colX1 1))
							(set! rowY1 (+ rowY1 1))
						)
					)
				)
			)
			((3) ; rising diagonal
				(begin
					(set! colX1 selectionX2)
					(set! rowY1 (- selectionY2 1))
					(set! colX2 (- selectionX2 1))
					(set! rowY2 selectionY2)
					(while (> rowY2 (+ selectionY1 0))
						(vector-set! points 0 (+ colX1 1.5))
						(vector-set! points 1 (- rowY1 0.5))
						(vector-set! points 2 (+ colX1 1.45))
						(vector-set! points 3 (+ rowY1 0.5))
						(vector-set! points 4 (- colX2 0.5))
						(vector-set! points 5 (+ rowY2 1.5))
						(vector-set! points 6 (- colX2 0.5))
						(vector-set! points 7 (+ rowY2 0.5))
						(gimp-free-select img 8 points CHANNEL-OP-REPLACE FALSE FALSE 0)
						(gimp-context-set-foreground (car (gimp-image-pick-color img layer colX1 rowY1 FALSE FALSE 0)))
						(gimp-context-set-background (car (gimp-image-pick-color img layer colX2 rowY2 FALSE FALSE 0)))
						(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE supersample 5 2 dither colX1 rowY1 colX2 rowY2)
						(if (> colX2 selectionX1)
							(set! colX2 (- colX2 1))
							(set! rowY2 (- rowY2 1))
						)		
						(if (> rowY1 selectionY1)
							(set! rowY1 (- rowY1 1))
 							(set! colX1 (- colX1 1))
						)
					)
				)
			)
		)

	


		(if (= (car selectionBounds) TRUE)
			(begin
				(gimp-rect-select img selectionX1 selectionY1 selectionWidth selectionHeight CHANNEL-OP-REPLACE 0 0)
				(gimp-layer-add-mask layer layerMask)
				(gimp-layer-set-edit-mask layer FALSE)
			)
			(gimp-selection-none img)
		)
		(gimp-image-undo-group-end img)
		(gimp-context-pop)
		(gimp-displays-flush)

	)
)

;*************************************************************************************************************************
;
;  Registration of the Script-Fu
;
;*************************************************************************************************************************

(script-fu-register "script-fu-2d-grad-1-2"
                    "2-Dimensional Gradient (2DG)..."
					"This script generates a new layer containing a two-dimensional gradient based on the colors of the four corner points or edges."
					"Charles Belov <docorbit@sonic.net>"
					"Charles Belov"
					"2009-04-09"
					"RGB*"
					SF-IMAGE "Image" 0
					SF-DRAWABLE "Drawable" 0
					SF-OPTION _"Option source" '(_"Use options shown" _"Random - all options" _"Random - color options" _"Random - gradient options" _"Random - blend/method/rule options" _"Current 2DG layer - all options" _"Current 2DG layer - color options" _"Current 2DG layer - gradient options" _"Current 2DG layer - blend/method/rule options")
					SF-COLOR "Top left color" '(255 0 0)
					SF-COLOR "Top right color" '(0 0 255)
					SF-COLOR "Bottom left color" '(0 255 0)
					SF-COLOR "Bottom right color" '(255 255 0)
					SF-GRADIENT "Top edge gradient" "Abstract 1"
					SF-GRADIENT "Bottom edge gradient" "Golden"
					SF-GRADIENT "Left edge gradient" "Tropical Colors"
					SF-GRADIENT "Right edge gradient" "Pastels"
					SF-OPTION _"Fill blend" '(_"RGB" _"HSV (clockwise hue)" _"HSV (counter-clockwise)")
					SF-OPTION _"Top edge method" '(_"Top left to top right RGB" _"Top left to top right HSV (clockwise hue)" _"Top left to top right HSV (counter-clockwise)" _"Top edge gradient" _"Reversed top edge gradient" _"Existing image")
					SF-OPTION _"Bottom edge method" '(_"Bottom left to bottom right RGB" _"Bottom left to bottom right HSV (clockwise hue)" _"Bottom left to bottom right HSV (counter-clockwise)" _"Bottom edge gradient" _"Reversed bottom edge gradient" _"Existing image")
					SF-OPTION _"Left edge method" '(_"Top left to bottom left RGB" _"Top left to bottom left HSV (clockwise hue)" _"Top left to bottom left HSV (counter-clockwise)" _"Left edge gradient" _"Reversed left edge gradient" _"Existing image")
					SF-OPTION _"Right edge method" '(_"Top right to bottom right RGB" _"Top right to bottom right HSV (clockwise hue)" _"Top right to bottom right HSV (counter-clockwise)" _"Right edge gradient" _"Reversed right edge gradient" _"Existing image")
					SF-OPTION _"Fill method" '(_"Top to bottom" _"Left to right" _"Falling diagonal" _"Rising diagonal")
					SF-STRING "Edge repeat rule (0-9 and S or T for T B L R)" "0T 0T 0T 0T"
					SF-TOGGLE "Allow image source as an edge method in randomly-generated layers" FALSE
)

(script-fu-menu-register "script-fu-2d-grad-1-2" 
						"<Image>/Filters/Render/Pattern")
;                         "<Image>/Filters")

