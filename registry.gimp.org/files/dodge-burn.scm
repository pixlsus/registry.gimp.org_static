; Dodge burn is a script for The GIMP
;
; This script produces a dodge and burn effect on an image
;
; The script is located in "<Image> / Script-Fu / Enhance / Dodge and burn..."
;
; Last changed: 5th July 2008
;
; Copyright (C) 2008 Harry Phillips <script-fu@tux.com.au>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 0.1
;    - Initial version
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

(define (my-layer-stuff		myImage
				myLayer
				modeOp
				thinNum
				thickNum
	)

    ;Initiate some variables
    (let* (
	(firstTemp (car (gimp-layer-copy myLayer 1)))
	(thinTemp)
	(thickTemp)
	(merged)
    )


    ;Rename the layer
    (gimp-drawable-set-name firstTemp "First temp")

    ;Add the first layer to the image
    (gimp-image-add-layer myImage firstTemp 0)

    ;Desaturate the layer
    (gimp-desaturate firstTemp)

    ;Copy and add the dodge me layer as the thin layer
    (set! thinTemp (car (gimp-layer-copy firstTemp 1)))
    (gimp-image-add-layer myImage thinTemp 0)

    ;Blur the thin layer
    (plug-in-gauss 1 myImage thinTemp thinNum thinNum 0)

    (if (= modeOp 1)

	;Change the mode of the thin layer to lighten
    	(gimp-layer-set-mode thinTemp 10)

	;Change the mode of the thin layer to darken
    	(gimp-layer-set-mode thinTemp 9)
    )


    ;Blur the dodge me layer
    (plug-in-gauss 1 myImage firstTemp thickNum thickNum 0)

    ;Copy the dodge me layer as a new layer
    (set! thickTemp (car (gimp-layer-copy firstTemp 1)))

    ;Add the new layer to the image
    (gimp-image-add-layer myImage thickTemp 1)

    ;Merge the top layer down and keep track of the newly merged layer
    (set! merged (car (gimp-image-merge-down myImage thinTemp 0)))

    ;Change the mode of the dodge copy layer to difference mode
    (gimp-layer-set-mode merged 6)

    ;Merge the top layer down and keep track of the newly merged layer
    (set! merged (car (gimp-image-merge-down myImage merged 0)))

    (if (= modeOp 1)

	(begin
    		;Rename the layer
    		(gimp-drawable-set-name merged "Dodge channel")

    		;Change the mode of the dodge copy layer to dodge mode
    		(gimp-layer-set-mode merged 16)
	)

	(begin

    		;Rename the layer
    		(gimp-drawable-set-name merged "Burn channel")

    		;Change the mode of the dodge copy layer to dodge mode
    		(gimp-layer-set-mode merged 17)

		;Invert layer
		(gimp-invert merged)
	)
    )

    ;Return
    ))



(define (script-fu-dodge-burn 	theImage
				theLayer
				thinAmount
				thickAmount
	)

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Select none
    (gimp-selection-none theImage)

    ;Do the dodge layer first
    (my-layer-stuff theImage theLayer 1 thinAmount thickAmount)

    ;Do the burn layer
    (my-layer-stuff theImage theLayer 0 thinAmount thickAmount)



    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)


)


(script-fu-register "script-fu-dodge-burn"
            _"<Image>/Script-Fu/Enhance/Dodge and burn..."
            "Dodge and burn a image"
            "Harry Phillips"
            "Harry Phillips"
            "Jul. 05 2008"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-ADJUSTMENT   _"Thin amount:"     '(5 0 1000 1 1 0 1)
	    SF-ADJUSTMENT   _"Thick amount:"      '(25 0 10000 1 1 0 1)
)




