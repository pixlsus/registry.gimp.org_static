; blur_and_shadow.scm is a script for The GIMP
;
; The script is located in "<Image> / Script-Fu / Decor / Blur & Shadow..."
;
; Last changed: 17 November 2009
;
; Copyright (C) 2009 Guillaume Duwelz-Rebert <gduwelzrebert@gmail.com>
;
; --------------------------------------------------------------------
;
; Changelog:
;
; Version 1.0
; - Initial release
; Version 2.0
; - Add sliders to control blur ratio and shadow ratio
; - Add toggle button to control fore/background de-saturation
;
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;

(define (script-fu-blur_and_shadow theImage theDrawable inShadowRatio inBlurRatio inForegroundDesaturate inBackgroundDesaturate)

  ; Variable init
  (let* (
	  (foregroundWidth  (car (gimp-image-width theImage)))
	  (foregroundHeight (car (gimp-image-height theImage)))
	  (backgroundWidth)
	  (backgroundHeight)
	  (foregroundX0)
	  (foregroundY0)
	  (foregroundX1)
	  (foregroundY1)
	  (blurRatio (/ inBlurRatio 10000))
	  (blurSize)
	  (shadowRatio (/ inShadowRatio 1000))
	  (shadowsize)
        )
	
    ; Increase Background size by 12,5%
    (set! backgroundWidth (* 1.125 foregroundWidth))
    (set! backgroundHeight (* 1.125 foregroundHeight))
    (set! foregroundX0 (/ (- backgroundWidth foregroundWidth) 2))
    (set! foregroundY0 (/ (- backgroundHeight foregroundHeight) 2))
    (set! foregroundX1 (+ foregroundX0 foregroundWidth))
    (set! foregroundY1 (+ foregroundY0 foregroundHeight))
    (set! blurSize (* blurRatio foregroundWidth))
    (set! shadowsize (* shadowRatio foregroundWidth))

;; Save Original picture to Original layer

    ; Rename current picture to "Original"
    (gimp-item-set-name theDrawable "Original")

;; Create a new background layer

    ; Duplicate selected layer
    (define theBackground (car (gimp-layer-copy theDrawable TRUE)))
    (gimp-image-insert-layer theImage theBackground 0 0)
    
    ; Rename current picture to "Background"
    (gimp-item-set-name theBackground "Background")

    ; Increase background by 12,5%
    (gimp-layer-scale theBackground backgroundWidth backgroundHeight TRUE)
    (gimp-image-resize-to-layers theImage)

    ; Blur the background
    (gimp-image-set-active-layer theImage theBackground)
    (plug-in-gauss-rle2 1 theImage theBackground blurSize blurSize)
    
    ; Flush the result to update the display
    (gimp-displays-flush)

;; Create a new foreground layer

    ; Duplicate selected layer
    (define theForeground (car (gimp-layer-copy theDrawable TRUE)))
    (gimp-image-insert-layer theImage theForeground 0 0)
    
    ; Rename duplicated picture to "Foreground"
    (gimp-item-set-name theForeground "Foreground")

    ; Flush the result to update the display
    (gimp-displays-flush)
    
;; Create a new layer for the foreground border

    ; Add a layer to add a border to the foreground
    (define borderLayer (car (gimp-layer-copy theForeground TRUE)))
    (gimp-image-insert-layer theImage borderLayer 0 1)

    ; Rename duplicated picture to "foregroundBorder"
    (gimp-item-set-name borderLayer "foregroundBorder")
    
    ; scale the border layer to image size
    (gimp-layer-resize-to-image-size borderLayer)
    
    ; Make a rectangular selection 2 pixel larger than the foreground in each direction
    (gimp-image-select-rectangle theImage CHANNEL-OP-ADD (- foregroundX0 2) (- foregroundY0 2) (+ foregroundWidth 4) (+ foregroundHeight 4))
    
    ; Select the border layer as the active layer
    (gimp-image-set-active-layer theImage borderLayer)
    
    ; Choose the black color to be the background color
    (gimp-context-set-background '(0 0 0))
    
    ; fill the rectangular selection with the background color
    (gimp-edit-fill borderLayer BACKGROUND-FILL)

    ; Flush the result to update the display
    (gimp-displays-flush)

;; Create a new layer for the foreground shadow

    ; Add a layer to add a shadow to the foreground
    (define shadowLayer (car (gimp-layer-copy theForeground TRUE)))
    (gimp-image-insert-layer theImage shadowLayer 0 2)
        
    ; Rename duplicated picture to "foregroundShadow"
    (gimp-item-set-name shadowLayer "foregroundShadow")

    ; scale the border layer to image size
    (gimp-layer-resize-to-image-size shadowLayer)
    
    ; Make a rectangular selection  larger of shadowsize than the foreground in each direction
    (gimp-image-select-rectangle theImage CHANNEL-OP-ADD (- foregroundX0 2) (- foregroundY0 2) (+ (+ foregroundWidth 4) shadowsize) (+ (+ foregroundHeight 4) shadowsize))
    
    ; Select the shadow layer as the active layer
    (gimp-image-set-active-layer theImage shadowLayer)
    
    ; fill the rectangular selection with the background color
    (gimp-edit-fill shadowLayer BACKGROUND-FILL)
    
    ; Remove any selection done to be able to blur the shadow
    (gimp-selection-none theImage)

    ; Blur the shadow
    (plug-in-gauss-rle2 1 theImage shadowLayer (* shadowsize 2) (* shadowsize 2))

    ; Select the shadow layer as the active layer
    (gimp-image-set-active-layer theImage shadowLayer)
    
    ; Check whether first toggle button has been ticked
    (if (= inForegroundDesaturate TRUE) 
           (gimp-desaturate-full theForeground DESATURATE-LIGHTNESS)
	   ()
    )

    ; Check whether second toggle button has been ticked
    (if (= inBackgroundDesaturate TRUE) 
           (gimp-desaturate-full theBackground DESATURATE-LIGHTNESS)
	   ()
    )

    ; Flush the result to update the display
    (gimp-displays-flush)


;; Finish the process by selecting the background layer to be the active layer 
    
    ; Select the background layer as the active layer
    (gimp-image-set-active-layer theImage theBackground)

  )
)

(script-fu-register
          "script-fu-blur_and_shadow"							;func name
          "Blur & Shadow"								;menu label
          "Creates a picture which is a duplication\
of the current picture blurred as background,\
and the current picture itself reduce with a\
shadow around it as foreground."							;description
          "Guillaume Duwelz-Rebert"							;author
          "copyright 2009, Guillaume Duwelz-Rebert"					;copyright notice
          "November 10, 2009"								;date created
          ""										;image type that the script works on
	  SF-IMAGE		"Image"				0
          SF-DRAWABLE		"Drawable"			0
	  SF-ADJUSTMENT  	"Shadow ratio"			'(8 1 20 1 10 1 0)	;a slider
	  SF-ADJUSTMENT  	"Blur ratio"			'(167 1 500 1 10 1 0)	;a slider
	  SF-TOGGLE		"Desaturate foreground"		FALSE			;a toggle button
	  SF-TOGGLE		"Desaturate background"		FALSE			;a toggle button
        )
        (script-fu-menu-register "script-fu-blur_and_shadow" _"<Image>/Filters/Decor")
