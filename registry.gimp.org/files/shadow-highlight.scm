;
; Shadow/Highlight filter. V0.8
;
; Magnus Stålnacke (at telia.com under the username jemamo)
; (C) April 27 2008, Sweden
;
; This script was tested with Gimp 2.4
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; If You did not receive a copy of the GNU General Public License
; along with this script, see http://www.gnu.org/licenses/
; 

(define (script-fu-shadow-highlight image drawable shadows highlights shtw hitw blur InFlatten)

;make shadow layer
(let ((shadows-layer (car (gimp-layer-copy drawable 1))))
(gimp-drawable-set-name shadows-layer "Shadows")
(gimp-image-add-layer image shadows-layer -1)

;make it grey, blurred, inverted and set as overlay
(gimp-desaturate shadows-layer)
(plug-in-gauss-iir2 1 image shadows-layer blur blur)
(gimp-invert shadows-layer)
(gimp-layer-set-mode shadows-layer 5)

;copy shadow to highlight
(define highlights-layer (car (gimp-layer-copy shadows-layer 1)))
(gimp-drawable-set-name highlights-layer "Highlights")
(gimp-image-add-layer image highlights-layer -1)

;process the shadows layer
(gimp-levels shadows-layer 0 0 255 shtw 0 255)
(plug-in-colortoalpha 1 image shadows-layer '(0 0 0))
(gimp-layer-set-opacity shadows-layer shadows)

;process the highlights layer
(gimp-levels highlights-layer 0 0 255 hitw 0 255)
(plug-in-colortoalpha 1 image highlights-layer '(255 255 255))
(gimp-layer-set-opacity highlights-layer highlights)

; Flatten the image, if needed.
(cond
 ((= InFlatten TRUE)
  (begin(gimp-image-merge-visible-layers image 0))
 )
)

;Update image window
(gimp-displays-flush)))

(script-fu-register "script-fu-shadow-highlight"
	"<Image>/Filters/Light and Shadow/Shadow&Highlight"
	"Removes shadows and highlights from a photograph"
	"Magnus Stålnacke"
	"Magnus Stålnacke"
	"April 27, 2008"
	"RGB* GRAY*"
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
	SF-ADJUSTMENT "Shadow Amount"    '(50  0  100  1  1  0  0)
	SF-ADJUSTMENT "Highlight Amount" '(50  0  100  1  1  0  0)
	SF-ADJUSTMENT "Shadow Tonal width"    '(1  0.3  1.7  0.1  0.1  1  0)
	SF-ADJUSTMENT "Highlight Tonal width" '(1  0.3  1.7  0.1  0.1  1  0)
	SF-ADJUSTMENT "Contrast Radius" '(25  1  49   1   1   0   0)
	SF-TOGGLE "Flatten Image" FALSE
)

