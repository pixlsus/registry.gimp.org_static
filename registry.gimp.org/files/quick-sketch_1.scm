; Quick sketch is a script for The GIMP
;
; Quick sketch turns a photo into what looks like a artists sketch
;
; The script is located in "<Image> / Filters / Artistic / Quick sketch..."
;
; Last changed: 2009-02-09
;
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
; Copyright (C) 2009 Michael Schumacher <schumaml@gmx.de>
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 1.1 (2009-02-09)
;    - fixed let* syntax
;
;  Version 1.0 (14th Jun 2008)
;    - Changed so that only the layer is converted to grey scale.
;    - Removed changing image back to RGB mode as an option.
;
;  Version 0.21
;    - Added changing image back to RGB mode as an option.
;
;  Version 0.2
;    - Changed image back to RGB mode.
;
;  Version 0.1
;    - Initial release
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



(define (script-fu-quick-sketch 	theImage
					theLayer
					blurAmount
					)

    ;Initiate some variables
    (let* (
	(layerCopy 0)
	(layerGrey (car (gimp-drawable-is-gray theLayer)))
    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Rename the layer
    (gimp-drawable-set-name theLayer "Original")
    
    ;Select none
    (gimp-selection-none theImage)

    ;Change the layer Greyscale if it isn't already
    (if (= layerGrey 0) (gimp-desaturate theLayer))
    
    (set! layerCopy (car (gimp-layer-copy theLayer 1)))
    
    ;Copy the layer
    (gimp-image-add-layer theImage layerCopy 0)
    
    ;Rename the layer
    (gimp-drawable-set-name layerCopy "Dodge layer")
    
    ;Invert the layer
    (gimp-invert layerCopy)
    
    ;Change the layers mode
    (gimp-layer-set-mode layerCopy 16)
    
    ;Blur the dodge layer
    (plug-in-gauss 1 theImage layerCopy blurAmount blurAmount 0)
    
    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)

    
    )
)


(script-fu-register "script-fu-quick-sketch"
            _"<Image>/Script-Fu/Artistic/Quick sketch..."
            "Create a sketch from a photo"
            "Harry Phillips"
            "Harry Phillips"
            "Sep. 9 2007"
            "*"
            SF-IMAGE		"Image"     0
            SF-DRAWABLE		"Drawable"  0
	    SF-ADJUSTMENT	_"Blur factor"      '(30 5 200 1 1 0 1)

)
