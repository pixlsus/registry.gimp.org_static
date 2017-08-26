;highlight-from-selection.scm
; by Dirk Sohler
; https://0x7be.de

; Version 1.0 (2013-01-21)

; Description
;
; This script adds a filter for highlighting the contents of a selection by
; adding blur and desaturation (both optional) to the layer’s other contents.

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
	 script-fu-highlight-from-selection-german-gui
	 inImage
	 inLayer
	 inRadius
	 inBlur
	 inOpacity
	 inDesaturate)
  (gimp-image-undo-group-start inImage)


  (let

    (
     (colorLayer (car (gimp-layer-copy inLayer TRUE)))
     (grayLayer (car (gimp-layer-copy inLayer TRUE)))
     (layername (car (gimp-drawable-get-name inLayer)))
     )

    (gimp-image-add-layer inImage colorLayer -1)
    (gimp-image-add-layer inImage grayLayer -1)
    (gimp-layer-set-name colorLayer (string-append layername _" (Hervorhebungsebene)"))

    (gimp-selection-feather inImage inRadius)
    (gimp-edit-clear colorLayer)
    (gimp-edit-clear grayLayer)
    (gimp-selection-none inImage)

    (gimp-layer-set-opacity grayLayer inOpacity)

    (if (= inDesaturate TRUE)
      (gimp-desaturate grayLayer)
      )

    (if (> inBlur 0)
      (plug-in-gauss
	1
	inImage
	(car (gimp-image-merge-down inImage grayLayer 2))
	inBlur
	inBlur
	0)
      )

    (if (= inBlur 0)
      (car (gimp-image-merge-down inImage grayLayer 2))
      )

    )
  (gimp-image-undo-group-end inImage)
  (gimp-displays-flush)
  )


(script-fu-register
  "script-fu-highlight-from-selection-german-gui"
  "<Image>/Filters/Generic/Hervorhebung nach Auswahl"
  _"Hebt die Auswahl durch Entsättigung und Unschärfe des Restes hervor."
  "Dirk Sohler <spam@0x7be.de>"
  "Dirk Sohler"
  "2013-01-21"
  "RGB*"
  SF-IMAGE	_"Image"				0
  SF-DRAWABLE	_"Drawable"				0
  SF-ADJUSTMENT	_"Herforhebungsradius"			'(100 0 500 1 10 0 0)
  SF-ADJUSTMENT	_"Stärke der Unschärfe"			'(3 0 10 1 10 0 0)
  SF-ADJUSTMENT	_"Deckkraft der Entsättigung"		'(50 0 100 1 10 0 0)
  SF-TOGGLE	_"Nicht hervorgehobene Ebenenteile entsättigen"	TRUE
  )

