;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; steel.scm
; Version 09.10.22 (For The Gimp 2.6) 22.10.2009
; Create a text effect that looks like shiny steel (or gold) and a dropshadow.
;
; Copyright (C) 2009 Marcos Pereira (majpereira) <majpereira@hotmail.com>
; ((C) 2007 Marcos Pereira (majpereira) <majpereira@hotmail.com>)
; ((C) 2001 Hani Al-Ers & Samer Yhya <samer-yhya@yahoo.com>)
;----------------------------------------------------------------------------------
; Baseado no script:
; => STEEL versão de 2001 para o Gimp 2.0, de Hani Al-Ers & Samer Yhya.
;    This script was inspired by polykarbon computer art tutorial:
;    http://www.polykarbon.com/tutorials/index.htm
;    Any comments? e-mail me at: samer-yhya@yahoo.com

; => steel.scm versão 07.06.11 (For The Gimp 2.2) 11.06.2007, de Marcos Pereira
;    (majpereira)
;----------------------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or 
; modify it under the terms of the GNU General Public License   
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;==================================================================================
; "set-pt" and "spline1" (spline-chrome-it) is from chrome-it.scm
; --> CHROME-IT by Spencer Kimball - 1997
  (define (set-pt a index x y)
    (begin
      (aset a (* index 2) x)
      (aset a (+ (* index 2) 1) y)
    )
  )

  (define (spline1)
    (let* ((a (cons-array 18 'byte)))
      (set-pt a 0 0 0)
      (set-pt a 1 31 235)
      (set-pt a 2 63 23)
      (set-pt a 3 95 230)
      (set-pt a 4 127 25)
      (set-pt a 5 159 210)
      (set-pt a 6 191 20)
      (set-pt a 7 223 240)
      (set-pt a 8 255 31)
      a
    )
  )

(define (apply-steel-metal-logo-effect img logo-layer size bg-color sh-color gradient color-g? color-g)
	(let* ((feather (/ size 5))
	(smear 7.5)
	(period (/ size 3))
	(amplitude (/ size 40))
	(shrink (+ 1 (/ size 60)))
	(depth (/ size 20))
	(width (car (gimp-drawable-width logo-layer)))
	(height (car (gimp-drawable-height logo-layer)))
	(img-width (+ width 20))
	(img-height (+ height 20))
	(bg-layer (car (gimp-layer-new img img-width img-height RGB-IMAGE "Background" 100 NORMAL-MODE)))
	(shadow-layer (car (gimp-layer-new img img-width img-height RGBA-IMAGE "Shadow" 100 NORMAL-MODE)))
	(reflect-layer (car (gimp-layer-new img width height RGBA-IMAGE "Reflection" 100 NORMAL-MODE)))
	(channel 0)
	(fs 0)
	(layer-mask 0)
	(dont-drop-me 0)
	(old-gradient (car (gimp-context-get-gradient)))
	(old-fg (car (gimp-context-get-foreground)))
	(old-bg (car (gimp-context-get-background))))
;----------------------------------------------------------------------------------
	(gimp-image-resize img img-width img-height 0 0)
	(gimp-image-add-layer img bg-layer 1)
	(gimp-layer-set-lock-alpha logo-layer TRUE)
	(gimp-context-set-background bg-color)
	(gimp-edit-fill bg-layer BACKGROUND-FILL)

	(gimp-context-set-foreground '(132 132 132))
	(gimp-edit-bucket-fill logo-layer FG-BUCKET-FILL NORMAL-MODE 100 255 TRUE 0 0)

	(gimp-context-set-gradient gradient)
	(gimp-edit-blend logo-layer CUSTOM-MODE OVERLAY-MODE GRADIENT-LINEAR 100 0
	 REPEAT-NONE FALSE FALSE 0 0 FALSE 0 0 (* width 3) (* height 3))
	(gimp-rect-select img 0 (- (/ height 2) feather) img-width (* 2 feather) CHANNEL-OP-REPLACE FALSE 0)
	(plug-in-gauss-iir 1 img logo-layer smear TRUE TRUE)
	(gimp-selection-none img)
	(gimp-layer-translate logo-layer 5 5)
	(gimp-layer-resize logo-layer img-width img-height 5 5)

	(gimp-selection-layer-alpha logo-layer)
	(set! channel (car (gimp-selection-save img)))
	(gimp-selection-shrink img shrink)
	(gimp-selection-invert img)
	(plug-in-gauss-rle 1 img channel feather TRUE TRUE)
	(gimp-selection-layer-alpha logo-layer)
	(gimp-selection-invert img)
	(gimp-context-set-background '(0 0 0))
	(gimp-edit-fill channel BACKGROUND-FILL)
	(gimp-selection-none img)

	(plug-in-bump-map 1 img logo-layer channel 135 45 depth 0 0 0 0 FALSE FALSE LINEAR)

	(gimp-curves-spline logo-layer 0 18 (spline1))

	(gimp-selection-layer-alpha logo-layer)
	(gimp-context-set-foreground '(0 0 0))
	(gimp-edit-bucket-fill logo-layer FG-BUCKET-FILL COLOR-MODE 100 255 TRUE 0 0)

	(gimp-context-set-foreground sh-color)
	(gimp-edit-bucket-fill logo-layer FG-BUCKET-FILL COLOR-MODE 20 255 TRUE 0 0)

	(plug-in-unsharp-mask 1 img logo-layer 5 0.5 0)
	(if (not (= color-g? 0))
	(begin
	(gimp-context-set-gradient color-g)
	(gimp-edit-blend logo-layer CUSTOM-MODE COLOR-MODE GRADIENT-LINEAR 100 0
	 REPEAT-NONE FALSE FALSE 0 0 FALSE 0 0 width height )
	(gimp-edit-blend logo-layer CUSTOM-MODE OVERLAY-MODE GRADIENT-LINEAR 100
	 0 REPEAT-NONE FALSE FALSE 0 0 FALSE 0 0 width height )))

	(set! dont-drop-me (car (script-fu-drop-shadow img logo-layer 5 5 10 '(0 0 0) 80 TRUE)))
	(set! width (car (gimp-image-width img)))
	(set! height (car (gimp-image-height img)))
	(gimp-selection-none img)

	(gimp-context-set-gradient old-gradient)
	(gimp-context-set-background old-bg)
	(gimp-context-set-foreground old-fg)))
;==================================================================================
(define (script-fu-steel-metal-logo-alpha img
					logo-layer
					size
					bg-color
					sh-color
					gradient
					color-g?
					color-g)
	(begin
	(gimp-image-undo-group-start img)
	(gimp-layer-resize-to-image-size logo-layer)
	(gimp-image-raise-layer-to-top img logo-layer)
	(apply-steel-metal-logo-effect img logo-layer size bg-color sh-color gradient color-g? color-g)
	(gimp-image-undo-group-end img)
	(gimp-displays-flush)))
;----------------------------------------------------------------------------------
(script-fu-register "script-fu-steel-metal-logo-alpha"
		    _"<Image>/Script-Fu/Alpha to Logo/Steel..."
		    _"Metallic color effect"
		"Marcos Pereira <majpereira@hotmail.com>"
		"Marcos Pereira (majpereira)"
		"22.10.2009"
		    "RGBA"
                    SF-IMAGE      "Image" 0
                    SF-DRAWABLE   "Drawable" 0
		    SF-ADJUSTMENT _"Effect Size (pixels)" '(100 2 1000 1 10 0 1)
		    SF-COLOR      _"Background Color" '(255 255 255)
		    SF-COLOR      _"Shine Color" '(09 111 237)
		    SF-GRADIENT   _"Pattern Gradient" "Flare Rays Size 1"
		    SF-TOGGLE     _"Use Color Gradient?" FALSE
		    SF-GRADIENT   _"Color Gradient" "Golden"
		    )
;==================================================================================
(define (script-fu-steel-metal-logo text
				size
				font
				bg-color
				sh-color
				gradient
			        color-g?
				color-g)
	(let* ((img (car (gimp-image-new 256 256 0)))
	(text-layer (car (gimp-text-fontname img -1 0 0 text 0 TRUE size PIXELS font))))
	(gimp-image-undo-disable img)
	(gimp-drawable-set-name text-layer text)
	(apply-steel-metal-logo-effect img text-layer size bg-color sh-color gradient color-g? color-g)
	(gimp-image-undo-enable img)
	(gimp-display-new img)))
;----------------------------------------------------------------------------------
(script-fu-register "script-fu-steel-metal-logo"
		    _"<Toolbox>/Xtns/Logos/Steel..."
		    _"Shiny metallic logo with shadows"
		"Marcos Pereira <majpereira@hotmail.com>"
		"Marcos Pereira (majpereira)"
		"22.10.2009"
		    ""
		    SF-STRING     _"Text" "The GIMP"
		    SF-ADJUSTMENT _"Font Size (pixels)" '(150 2 1000 1 10 0 1)
		    SF-FONT       _"Font" "Sans Bold"
		    SF-COLOR      _"Background Color" '(255 255 255)
		    SF-COLOR      _"Shine Color" '(09 111 237)
		    SF-GRADIENT   _"Pattern Gradient" "Flare Rays Size 1"
		    SF-TOGGLE     _"Use Color Gradient?" FALSE
		    SF-GRADIENT   _"Color Gradient" "Golden"
		    )
;==================================================================================
