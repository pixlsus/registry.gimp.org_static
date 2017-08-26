; scanlines.scm
; by Devin Watson
; Seemed easy enough to convert into a plug-in, so I did
; The script is located in menu "<Image> / Script-Fu / Enhance / Scanlines..."
;
; Last changed: 17 June 2008
;
; Copyright (C) 2008 Devin Watson
;
; Uses the "Erase Every Other Row" code by Federico Mena Quintero
; --------------------------------------------------------------------
;  
; Changelog:
;  Version 0.1
;    - Initial plug-in script
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
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.

(define (script-fu-scanlines theImage theLayer scanlineColor opacityLevel lineOrientation flattenImage?)

    (let* 
	    (
			;Get the image width and height, used to calculate later
			(imageWidth (car (gimp-image-width theImage)))
			(imageHeight (car (gimp-image-height theImage)))
			(scaleWidth (* imageWidth 2))
			(scaleHeight (* imageHeight 2))
			(workLayer (car (gimp-layer-copy theLayer 1)))
			(which 0)
			(type 0)
	        (position-x (car (gimp-drawable-offsets theLayer)))
	        (position-y (cadr (gimp-drawable-offsets theLayer)))

			;Save the original foreground color
			(orgFGColor (car (gimp-context-get-foreground)))
			;(orgBGColor (car (gimp-context-get-background)))
	    )

	    ;Start an undo group so the process can be undone with one undo
	    (gimp-image-undo-group-start theImage)

	    ;Select none
	    (gimp-selection-none theImage)

	    ;Add the new layer to the image
	    (gimp-image-add-layer theImage workLayer 0)
		(gimp-image-set-active-layer theImage workLayer)
	    (gimp-displays-flush)

	    ;Rename the layer
	    (gimp-drawable-set-name workLayer "Scanlines")

		;Set the foreground color to scanlineColor
		(gimp-context-set-foreground scanlineColor)

		; Fill the layer with the scaline color set in the Foreground Color
		(gimp-drawable-fill workLayer FOREGROUND-FILL)

		;Put this in from erase-rows.scm.  For some odd reason this would not work by simply calling the plug-in
	    (letrec ((loop (lambda (i max)
	                     (if (< i max)
	                         (begin
	                           (if (= lineOrientation 0)
	                               (gimp-rect-select theImage position-x (+ i position-y) imageWidth 1 CHANNEL-OP-REPLACE FALSE 0)
	                               (gimp-rect-select theImage (+ i position-x) position-y 1 imageHeight CHANNEL-OP-REPLACE FALSE 0))
	                           (if (= type 0)
	                               (gimp-edit-clear workLayer)
	                               (gimp-edit-fill workLayer BACKGROUND-FILL))
	                           (loop (+ i 2) max))))))
	      (loop (if (= which 0)
	                0
	                1)
	            (if (= lineOrientation 0)
	                imageHeight
	                imageWidth)
	      )
	    )

		;Now we have to scale the layer up 200%
		(gimp-layer-scale workLayer scaleWidth scaleHeight FALSE)

		;Set the work layer mode to OVERLAY (5)
		(gimp-layer-set-mode workLayer SCREEN-MODE)

		;Set the opacity level
		(gimp-layer-set-opacity workLayer opacityLevel)

		(if (eqv? flattenImage? TRUE)
			(gimp-image-flatten theImage)
		)

		;Set the foreground and background colors back to their originals
		;(gimp-context-set-background orgBGColor)
		(gimp-context-set-foreground orgFGColor)

	    ;Select none
	    (gimp-selection-none theImage)
		(gimp-image-set-active-layer theImage workLayer)
	    ;Finish the undo group for the process
	    (gimp-image-undo-group-end theImage)

	    ;Ensure the updated image is displayed
	    (gimp-displays-flush)
	)

)

(script-fu-register 	"script-fu-scanlines"
			"Scanlines..."
			"Generates simple video scanlines"
			"Devin Watson <dmwatson@gmail.com>"
			"Devin Watson"
			"17 June 2008"
			"RGB* GRAY* INDEXED*"
			SF-IMAGE 			_"Image"	  	  0
			SF-DRAWABLE 		_"Layer"		  0
			SF-COLOR 			_"Scanline Color" '(0 255 42)
			SF-ADJUSTMENT		_"Opacity"		  '(30 0 100 1 10 0 1)
			SF-OPTION 			_"Orientation" '("Horizontal" "Vertical")
			SF-TOGGLE    	 	_"Flatten Image"  FALSE
)

(script-fu-menu-register "script-fu-scanlines"
			 "<Image>/Script-Fu/Enhance")