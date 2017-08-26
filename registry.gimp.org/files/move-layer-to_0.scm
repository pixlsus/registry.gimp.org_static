; Plugin  : move-layer-to.scm
; Author  : Gary Bucher 
; Date    : January 6, 2012
; Tested on GIMP version: 2.6.11
;
; Description: 
;  Moves the top left corner of the active layer to the specified coordinate.
; 
; -----------------------------------------------------------------------------
;
; License:
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; -----------------------------------------------------------------------------
;
; Define the function:
(define (script-fu-move-layer-to image layer absolutex absolutey)
	(let* ( 
			(layerOffsets (gimp-drawable-offsets layer))
			(layerOffsetsX (car layerOffsets))
			(layerOffsetsY (cadr layerOffsets))
			(activelayer 0)
			(offsetx 0)
			(offsety 0)
			)
		(set! activelayer (car (gimp-image-get-active-layer image)))
		(set! offsetx (- absolutex layerOffsetsX))
		(set! offsety (- absolutey layerOffsetsY))
		(gimp-layer-translate activelayer offsetx offsety)
		(gimp-displays-flush)
    )
)

(script-fu-register "script-fu-move-layer-to"
		    "<Image>/Layer/M_ove To"
		    "Move the top left corner of the active layer to the specified coordinate"
		    "Gary Bucher"
		    "Based on work by Giuseppe Conte and Salvatore Celli"
		    "January 6, 2012 Ver. 1.0"
		    "*"
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
 		    SF-ADJUSTMENT "Left (x)" '(0 1 9999 1 10 0 1)		    
 		    SF-ADJUSTMENT "Top (y)" '(0 1 9999 1 10 0 1)
		    )
