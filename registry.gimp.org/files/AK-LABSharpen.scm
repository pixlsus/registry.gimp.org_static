;
; LAB Sharpen, v. 1.0
;
; Adam Kemp (gimp@adamkemp.com)
; (C) 2009
;
; This script does LAB sharpening in GIMP using the unsharp mask. The steps it automates are:
; 1. Decompose the image into LAB
; 2. Run the unsharp mask filter on the Luminance (L) layer using the supplied settings.
; 3. Recompose the image from the LAB image
;
; This script was tested with Gimp 2.6
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
(define (script-fu-AK-LABSharpen InImage InLayer InRadius InAmount InThreshold)
  ;
  ; Save history			
  ;
  (gimp-image-undo-group-start InImage)
  (if (= (car (gimp-drawable-is-rgb InLayer)) FALSE ) (gimp-image-convert-rgb InImage))
  ;
  (let*
    (
	  ; Decompose into LAB
	  (LABImage (car (plug-in-decompose TRUE InImage InLayer "LAB" TRUE)))
	  ; Get the Luminance layer
	  (LuminanceLayer (aref (cadr (gimp-image-get-layers LABImage)) 0))
	)
	; Select the Luminance layer
	(gimp-image-set-active-layer LABImage LuminanceLayer)

	; Do the sharpen in the luminance layer
	(plug-in-unsharp-mask TRUE LABImage LuminanceLayer InRadius InAmount InThreshold)

	; Recompose
	(plug-in-recompose TRUE LABImage LuminanceLayer)

	; Delete the temporary LAB image
	(gimp-image-delete LABImage)
		
  )
  ;
  ; Finish work
  ;
  (gimp-image-undo-group-end InImage)
  (gimp-displays-flush)
  ;
  )
;
(script-fu-register 
  "script-fu-AK-LABSharpen"
  _"LAB Sharpen"
  "Sharpen an image using the luminance channel"
  "Adam Kemp (gimp@adamkemp.com)"
  "Adam Kemp"
  "2009/09/18"
  "RGB*"
  SF-IMAGE	"The Image"		0
  SF-DRAWABLE	"The Layer"		0
  SF-ADJUSTMENT	"Radius"	'(1 0.1 120.0 0.1 1.0 1 0)
  SF-ADJUSTMENT	"Amount"	'(0.75 0.1 10.0 0.01 1.0 2 0)
  SF-ADJUSTMENT	"Threshold"	'(0 0 255 1 5 0 0)
  )
;
(script-fu-menu-register "script-fu-AK-LABSharpen"
						 "<Image>/Filters/AK")
;
