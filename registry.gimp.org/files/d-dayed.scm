; D-Dayed is a script for The GIMP
;
; Makes the image like the over saturated grainy war photos as evoked in the
; cinematography of films like Saving Private Ryan
;
; The script is located in "<Image> / Script-Fu / Artistic / D-Dayed..."
;
; Last changed: 14 June 2008
;
; Copyright (C) 2008 Harry Phillips <script-fu@tux.com.au>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 0.11
;    - Changed the 'Blur factor' chooser
;
;  Version 0.1
;    - Initial release based on the 'Quick sketch' script
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
;



(define (script-fu-d-dayed	 	theImage
					theLayer
					blurAmount
					)

    ;Initiate some variables
    (let* (
	(layerGrey (car (gimp-drawable-is-gray theLayer)))
	(sketchLayer)
	(effectLayer (car (gimp-layer-copy theLayer 1)))
	(merged)
	(mask)
    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Select none
    (gimp-selection-none theImage)

    ;Copy the original layer
    (gimp-image-add-layer theImage effectLayer 0)

    ;Rename the layer
    (gimp-drawable-set-name effectLayer "Effect Layer")

    ;Change the layer Greyscale if it isn't already
    (if (= layerGrey 0) (gimp-desaturate effectLayer))
    
    ;Copy the layer
    (set! sketchLayer (car (gimp-layer-copy effectLayer 1)))
    (gimp-image-add-layer theImage sketchLayer 0)
    
    ;Invert the layer
    (gimp-invert sketchLayer)
    
    ;Change the layers mode to dodge
    (gimp-layer-set-mode sketchLayer 16)
    
    ;Blur the dodge layer
    (plug-in-gauss 1 theImage sketchLayer blurAmount blurAmount 0)

    ;Merge the top layer down and keep track of the newly merged layer
    (set! merged (car (gimp-image-merge-down theImage sketchLayer 0)))
    
    ;Add a greyscale copy layer mask
    (set! mask (car (gimp-layer-create-mask merged 5)))
    (gimp-layer-add-mask merged mask)

    ;Change the effect layer mode to burn
    (gimp-layer-set-mode merged 17)

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)
    
    )
)

(script-fu-register "script-fu-d-dayed"
            _"<Image>/Script-Fu/Artistic/D-dayed..."
            "Create a sketch from a photo"
            "Harry Phillips"
            "Harry Phillips"
            "Jun. 14 2008"
            "*"
            SF-IMAGE		"Image"     0
            SF-DRAWABLE		"Drawable"  0
	    SF-ADJUSTMENT	_"Blur factor"      '(30 5 10000 1 1 0 1)

)
