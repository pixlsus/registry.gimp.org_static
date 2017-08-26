; Soft focus is a script for The GIMP
;
; Simulates a soft focus effect on your image.
;
; The script is located in menu "<Image> / Script-Fu / Enhance / Soft Focus..."
;
; Last changed: 23 July 2008
;
; Copyright (C) 2008 Harry Phillips <script-fu@tux.com.au>
;
; --------------------------------------------------------------------
;  
; Changelog:
;  Version 0.2 (23 July 2008)
;    - Removed all references to setting the opacity of the brush, I 
;      just could not get it to work.
;
;  Version 0.1 (31 May 2008)
;    - Initial script
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
;
;


(define (script-fu-soft-focus	 	theImage
					theLayer
					propagateNumber
					opacityLevel
					)

    (let* 
    (
	;Read the image width and height
	(imageWidth (car (gimp-image-width theImage)))
	(imageHeight (car (gimp-image-height theImage)))

	(softness (car (gimp-layer-copy theLayer 0)))

	(blurAmount (* (/ propagateNumber 2) 3.2727273))
	(motionLength (* propagateNumber 1))
	(mask)

    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Select none
    (gimp-selection-none theImage)

    ;Add the softness layer to the image
    (gimp-image-add-layer theImage softness 0)

    ;Rename the layer
    (gimp-drawable-set-name softness "Softness")

    ;Apply the 'Value propagate' as many times as requested
    (begin 
	(while (> propagateNumber 0)
		(plug-in-vpropagate 1 theImage softness 0 1 1 15 0 255)
		(set! propagateNumber (- propagateNumber 1))
	)
    )
	
    ;Apply the blur with the calculated blur amount
    (plug-in-gauss 1 theImage softness blurAmount blurAmount 0)

    ;Apply a motion blur
    (plug-in-mblur 1 theImage softness 0 motionLength 45 0 0)

    ;Change the softness layers opacity
    (gimp-layer-set-opacity softness opacityLevel)

    ;Add a layer mask
    (set! mask (car (gimp-layer-create-mask softness 0)))
    (gimp-layer-add-mask softness mask)

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)

    
    )
)


(script-fu-register "script-fu-soft-focus"
            _"<Image>/Script-Fu/Enhance/Soft focus..."
            "Gives the image a soft focus effect"
            "Harry Phillips"
            "Harry Phillips"
            "May. 31 2008"
            "*"
            SF-IMAGE		"Image"			0
            SF-DRAWABLE		"Drawable"		0
	    SF-ADJUSTMENT	_"Iterations"		'(24 1 10000 1 10 0 1)
	    SF-ADJUSTMENT	_"Opacity"		'(34 0 100 1 10 1 0)
)
