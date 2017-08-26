; Split Tone is a script for The GIMP
;
; This script converts an image to one that has one colour for highlights
; and one for shadows.
;
; The script is located in "<Image> / Script-Fu / Colours / Split Tone..."
;
; Last changed: 5 August 2007
;
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 1.8 (8th August 2007)
;    - Removed redundant code by designing a single function that does all
;      the actions
;
;  Version 1.7 (5th August 2007)
;    - Added GPL3 licence 
;    - Menu location at the top of the script
;    - Removed the "script-fu-menu-register" section
;
;  Version 1.6
;    - Made the script compatible with GIMP 2.3
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

(define (layer-colour-add 	image
				layer
				name
				width
				height
				colour
				opacity
				invertMask)
	(let* (
		(layerCopy (car (gimp-layer-copy layer 1)))
		(newLayer (car (gimp-layer-new image width height 1 "Overlay" 100 5)))
		(mergedLayer)
		(mask)
	)
	;Set the background colour
	(gimp-context-set-background colour)

	;Copy the layer
	(gimp-image-add-layer image layerCopy 0)
	
	;Rename the layer
	(gimp-drawable-set-name layerCopy name)
	
	;Add the new layer
	(gimp-image-add-layer image newLayer 0)
	
	;Set the layers mode
	(gimp-layer-set-mode newLayer 5)
	
	;Fill the layer with BG colour
	(gimp-edit-fill newLayer 1)
	
	;Merge the layer down
	(set! mergedLayer (car (gimp-image-merge-down image newLayer 0)))
	
	;Add a layer mask
	(set! mask (car (gimp-layer-create-mask mergedLayer 5)))
	(gimp-layer-add-mask mergedLayer mask)
	(if (= invertMask TRUE) (gimp-invert mask))
	
	;Change the merged layers opacity
	(gimp-layer-set-opacity mergedLayer opacity)

	;Return
		))
			


(define (script-fu-split-tone 	theImage
				theLayer
				highColour
				highOpacity
				shadColour
				shadOpacity
	)

    ;Initiate some variables
    (let* (
	;Read the current colours
	(myBackground (car (gimp-context-get-background)))

	;Read the image width and height
	(imageWidth (car (gimp-image-width theImage)))
	(imageHeight (car (gimp-image-height theImage)))

	(layerRGB)
    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Select none
    (gimp-selection-none theImage)

    ;Detect if it is RGB
    (set! layerRGB (car (gimp-drawable-is-rgb theLayer)))

    ;Change the image RGB if it isn't already
    (if (= layerRGB 0) (gimp-image-convert-rgb theImage))

    ;Desaturate the layer
    (gimp-desaturate theLayer)

    ;Add the shadows layer
    (layer-colour-add theImage theLayer "Shadows" imageWidth imageHeight shadColour shadOpacity TRUE)
    
    ;Add the highlights layer
    (layer-colour-add theImage theLayer "Highlights" imageWidth imageHeight highColour highOpacity FALSE)

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Set the BG colour back to what it was
    (gimp-context-set-background myBackground)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)

    
    )
)


(script-fu-register "script-fu-split-tone"
            _"<Image>/Script-Fu/Colours/Split Tone..."
            "Turns a B&W image into a split tone image"
            "Harry Phillips"
            "Harry Phillips"
            "Feb. 03 2006"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-COLOR "Highlight colour" '(255 198 0)
            SF-ADJUSTMENT   _"Highlight opacity:"     '(100 0 100 1 1 0 0)
            SF-COLOR "Shadows colour" '(43 198 255)
	    SF-ADJUSTMENT   _"Shadow opacity:"      '(100 0 100 1 1 0 0)
)
