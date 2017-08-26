
;; resizebrush.scm -*-scheme-
;; resizes a (gbr)-brush
;; version 1.0    2005-11-13
;; 
;; Copyright (C) 2005 by Michael Hoelzen <MichaelHoelzen@aol.com>
;; http://www.remoserv.de
;;
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
; version 1.1
; the #resiZed.gbr will now be saved in the gimp-directory instead of
; the gimp-data-directory which is write-protected in unix
; 
; version 1.2
; changed \\brushes\\ to /brushes/ for unix-confirmity
;
; version 1.3 Modify by James Hwang
; ??????,??????????????
;
; version 1.4 Modify by James Hwang
; ????
;
; version 2.4 fixed for Gimp 2.4 by James Hwang
; ????
;version 2.6 fixed for Gimp 2.6 by Lee Dennegar
;by initializing variables to zero 
;which was not necessary in earlier versions.


(define (script-fu-resize-brush image drawable resFactor angSel mirror bgray?)
	(define brushnameResi "#resiZed")
	(define filesave (string-append "" gimp-directory "/brushes/" brushnameResi ".gbr"))
	(define brushnameIn (car (gimp-context-get-brush)))
	(define (testIfresiZed a)
		(let* (
			(len1 0)
			(len2 0)
			(sstr "")
			)
			(set! len1 (string-length a))
			(set! len2 (string-length brushnameResi))
			(if (>= len1 len2)
			(begin
				(set! sstr (substring a (- len1 len2) len1))
				(if (string=? sstr brushnameResi)
				(begin
					(set! len1 (- len1 len2))
					(substring a 0 len1)
				)
				a
				)
			)
			a
			)
			; (if (not (null? (string-search brushnameResi a)))
				; (substring a 0 (string-search brushnameResi a))
				; a
			; )
		)
	)
	(let* (
		(actualbrush 0)
		(layer 0)
		(scolor)
		(bcolor 0)
		(arrayToPaint 0)  
		)
	(set! actualbrush (testIfresiZed brushnameIn))
	;(set! actualbrush brushnameIn)
	(gimp-context-set-brush actualbrush) 
	(let* 
		(   
			(brushnameOut (string-append actualbrush brushnameResi))
			(spacing (car (gimp-brush-get-spacing actualbrush)))
			(brushDimens (gimp-brush-get-info actualbrush))
			(brushWidth (car brushDimens))
			(brushHeight (cadr brushDimens))
			(resiZedWidth (* brushWidth resFactor))
			(resiZedHeight(* brushHeight resFactor))
			(centerX (/ brushWidth 2))
			(centerY (/ brushHeight 2))
			(centerXpluseins(+ centerX 1))
			(centerYpluseins(+ centerY 1))
			(angle (* angSel (/ 3.14159 180)))
		 )
		 
		(and (> resiZedWidth 1)(> resiZedHeight 1)
		    (begin
				(set! arrayToPaint (cons-array 4 'double))
				(aset arrayToPaint 0 centerX)
				(aset arrayToPaint 1 centerY)
				(aset arrayToPaint 2 centerXpluseins)
				(aset arrayToPaint 3 centerYpluseins)
				(set! image (car (gimp-image-new brushWidth brushHeight RGB)))
				(set! layer (car (gimp-layer-new image brushWidth brushHeight 1 "layer 1" 100 0)))
				(gimp-image-undo-disable image)
				(gimp-image-add-layer image layer 0)
				(gimp-drawable-fill layer 3)
				(if (= bgray? TRUE)
					(begin
						(set! scolor (car (gimp-context-get-foreground)))
						(set! bcolor (car (gimp-context-get-background)))
						(gimp-context-set-foreground '(0 0 0))
						(gimp-context-set-background '(255 255 255))
						(gimp-paintbrush-default layer 4 arrayToPaint)
						(gimp-image-scale image resiZedWidth resiZedHeight)
						(if (not (= angle 0))
							(begin
								(gimp-drawable-transform-rotate-default layer angle 1 0 0 1 0)
								(gimp-image-resize-to-layers image)
							)
						)
						(cond
							((= mirror 1)
				              	(gimp-image-flip image ORIENTATION-HORIZONTAL))
							((= mirror 2)
				              	(gimp-image-flip image ORIENTATION-VERTICAL))
						)
						(gimp-image-convert-grayscale image)
						(set! layer (car (gimp-image-flatten image)))
						(gimp-context-set-foreground scolor)
						(gimp-context-set-background bcolor)
					)
					(begin
						(gimp-paintbrush-default layer 4 arrayToPaint)
						(gimp-image-scale image resiZedWidth resiZedHeight)
						(if (not (= angle 0))
							(begin
								(gimp-drawable-transform-rotate-default layer angle 1 0 0 1 0)
								(gimp-image-resize-to-layers image)
							)
						)
						(cond
							((= mirror 1)
				              	(gimp-image-flip image ORIENTATION-HORIZONTAL))
							((= mirror 2)
				              	(gimp-image-flip image ORIENTATION-VERTICAL))
						)
					)
				)
				(file-gbr-save 1 image layer filesave filesave spacing brushnameOut)
				(gimp-image-undo-enable image)
				(gimp-image-delete image)
				(gimp-brushes-refresh)
				(gimp-context-set-brush brushnameOut)))
	)
	)
)
;
(script-fu-register "script-fu-resize-brush"
					_"_Resize Rotate Brush"
                    "Resize Rotate a brush, Base on resizebrush.scm by Michael Hoelzen"
                    "James Huang"
                    "James Huang"
                    "2008"
                    ""
                    SF-IMAGE       "Image"         0
                    SF-DRAWABLE    "Drawable"      0
                    SF-ADJUSTMENT _"Scale Factor"       '(1.0 0.1 3.0 0.1 1 1 0)
                    SF-ADJUSTMENT _"Rotate Degrees" 	  '(0 -180 180 1 10 0 0)
					SF-OPTION     _"Transform"    	  '("None" "Mirror" "Invert")
                    SF-TOGGLE     "Grayscale"           TRUE
                    ;SF-ADJUSTMENT _"resFactor"       '(1.0 0.1 3.0 0.1 1 1 0)
                    ;SF-ADJUSTMENT _"Angle" 	  '(0 0 359 1 10 0 0)
                    ;SF-TOGGLE     "Grayscale"           TRUE
)
(script-fu-menu-register "script-fu-resize-brush"
                         _"<Image>/Script-Fu/BrushUtil")
