; PS-style Gradient Map
; by Devin Watson
; Based on a tutorial found on the GimpTalk.com forums:
; http://www.gimptalk.com/forum/topic/PS-style-gradient-map-for-GIMP-5750-1.html  
; Seemed easy enough to convert into a plug-in, so I did
; The script is located in menu "<Image> / Script-Fu / Artistic / PS Gradient Map..."
;
; Last changed: 17 June 2008
;
; Copyright (C) 2008 Devin Watson
;
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

(define (script-fu-ps-gradmap theImage theLayer gradStart gradEnd opacityLevel flattenImage?)

    (let* 
	    (
			(workLayer (car (gimp-layer-copy theLayer 0)))
			;Save the original foreground and background colors
			(orgFGColor (car (gimp-context-get-foreground)))
			(orgBGColor (car (gimp-context-get-background)))
	    )

	    ;Start an undo group so the process can be undone with one undo
	    (gimp-image-undo-group-start theImage)

	    ;Select none
	    (gimp-selection-none theImage)

	    ;Add the duplicate layer to the image
	    (gimp-image-add-layer theImage workLayer 0)

	    ;Rename the layer
	    (gimp-drawable-set-name workLayer "Work Layer")

		;Desaturate the layer
		(gimp-desaturate workLayer)

		;Set the foreground color to gradStart
		(gimp-context-set-foreground gradStart)

		;Set the background color to gradEnd)
		(gimp-context-set-background gradEnd)

		;Run the gradient color map
		(plug-in-gradmap 1 theImage workLayer)

		;Set the layer mode to COLOR-MODE (13)
		(gimp-layer-set-mode workLayer COLOR-MODE)

		;Set the opacity level
		(gimp-layer-set-opacity workLayer opacityLevel)

		(if (eqv? flattenImage? TRUE)
			(gimp-image-flatten theImage)
		)

		;Set the foreground and background colors back to their originals
		(gimp-context-set-background orgBGColor)
		(gimp-context-set-foreground orgFGColor)

	    ;Finish the undo group for the process
	    (gimp-image-undo-group-end theImage)

	    ;Ensure the updated image is displayed
	    (gimp-displays-flush)
	)

)

(script-fu-register 	"script-fu-ps-gradmap"
			"PS Gradient Map..."
			"Create Photoshop-style gradient map"
			"Devin Watson <dmwatson@gmail.com>"
			"Devin Watson"
			"17 June 2008"
			"RGB*"
			SF-IMAGE 			_"Image"	  	  0
			SF-DRAWABLE 		_"Layer"		  0
			SF-COLOR 			_"Gradient Start" '(41 10 89)
			SF-COLOR 			_"Gradient End"   '(255 124 0)
			SF-ADJUSTMENT		_"Opacity"		  '(100 0 100 1 10 0 1)
			SF-TOGGLE    	 	_"Flatten Image"  FALSE
)

(script-fu-menu-register "script-fu-ps-gradmap"
			 "<Image>/Script-Fu/Artistic")