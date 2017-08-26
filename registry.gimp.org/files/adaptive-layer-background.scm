;adaptive-layer-background.scm
; by Dirk Sohler
; https://0x7be.de

; Version 1.0 (2013-03-14)

; Description
;
; This script creates a background for the given layer, containg some kind of
; color gradient, but adaptive to the layer’s contents.

; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html


(define (
	 script-fu-adaptive-layer-background
	 inImage
	 inDrawable
	 inBlur
	 inFinalBlur
	 inIntensity
	 inColor
	 inUseColor
	 inInteractive)
  (gimp-image-undo-group-start inImage)
  (let


    (
     (bgLayer (car(gimp-layer-copy inDrawable 1)))
     (bgName (car (gimp-drawable-get-name inDrawable)))
     (width (car (gimp-image-width inImage)))
     (height (car (gimp-image-height inImage)))
     (fgOld (car (gimp-palette-get-foreground)))
     (counter 1)
     (bgLayerCopy 0)
     (thisCopy 0)
     (bgColorLayer 0)
     (finalBackground 0)
     (interactive 0)
     )

    ; Set interactive blurring
    (if (= inInteractive 1)
      (begin (set! interactive 0)) ; enable interactive
      (begin (set! interactive 1)) ; disable interactive
      )

    ; Create first background layer
    (gimp-image-insert-layer inImage bgLayer -1 -1)
    (gimp-layer-resize-to-image-size bgLayer)
    (plug-in-gauss interactive inImage bgLayer inBlur 0 0)
    (gimp-edit-copy bgLayer)

    (while (< counter inIntensity)
	   (set! counter (+ counter 1))
	   (set! thisCopy (car (gimp-edit-paste bgLayer 0)))
	   (gimp-floating-sel-anchor thisCopy)
      )

    (plug-in-gauss interactive inImage bgLayer inFinalBlur inFinalBlur 0)

    ; Set solid color background color (or not), move layer one down
    ; and set name.
    (if (= inUseColor 1)
      (begin
	(gimp-palette-set-foreground inColor)
	(set! bgColorLayer (car (gimp-layer-new inImage width height
						0 "" 100 0)))
	(gimp-image-insert-layer inImage bgColorLayer -1 -1)
	(gimp-drawable-fill bgColorLayer 0)
	(gimp-palette-set-foreground fgOld)
	(gimp-image-lower-item inImage bgColorLayer)
	(set! finalBackground (car (gimp-image-merge-down inImage bgLayer 1)))
	(gimp-layer-set-name finalBackground
			     (string-append bgName " (" _"Background" ")"))
	(gimp-image-lower-item inImage finalBackground)
	)
      (begin
	(gimp-layer-set-name bgLayer
			     (string-append bgName " (" _"Background" ")"))
	(gimp-image-lower-item inImage bgLayer)
	)
      )


    )
  (gimp-image-undo-group-end inImage)
  (gimp-displays-flush)
  )


(script-fu-register
  "script-fu-adaptive-layer-background"
  "<Image>/Filters/Generic/Adaptive layer background"
  _"Adds an adaptive background for the selected layer"
  "Dirk Sohler <spam@0x7be.de>"
  "Dirk Sohler"
  "2013-03-14"
  "RGB*"
  SF-IMAGE	_"Image"			0
  SF-DRAWABLE	_"Drawable"			0
  SF-ADJUSTMENT	_"Amount of horizontal stretch *" '(2000 1 5984 1 100 0 1)
  SF-ADJUSTMENT	_"Strength of final blur *"	'(100 1 5984 1 50 0 1)
  SF-ADJUSTMENT	_"Background Intensity"		'(4 1 50 1 5 0 1)
  SF-COLOR	_"Background base color"	'(83 83 83)
  SF-TOGGLE	_"Fill background"		1
  SF-TOGGLE	_"Interactive blurring (ignores * values)"		0
  )

