; Blur Border is a script for The GIMP
;
; Creates a blur border with a colour in between.
;
; The script is located in menu "<Image> / Script-Fu / Misc / Blur Border..."
;
; Last changed: 7 May 2008
;
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 1.2.1 (27rd July 2008)
;    - Changed the script so that it only operates on RBG images
;
;  Version 1.2 (23rd July 2008)
;    - Added a check to ensure the border can fit in the image
;
;  Version 1.1 (7th May 2008)
;    - Total redesign of the script using layers
;
;  Version 1.0 (9th February 2008)
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

(define (script-fu-blur-border		theImage
					theLayer
					borderColour
					borderSize
					blurAmount
					blurSize)

    (let* 
    (
	;Read the current colours
	(myForeground (car (gimp-context-get-foreground)))
	(myBackground (car (gimp-context-get-background)))

	;Read the image width and height
	(imageWidth (car (gimp-image-width theImage)))
	(imageHeight (car (gimp-image-height theImage)))

	;Calculate the blur selection size
	(blurWidth (- imageWidth (* blurSize 2)))
	(blurHeight (- imageHeight (* blurSize 2)))

	;Calculate the inner border sizes
	(colourWidth (- blurWidth (* borderSize 2)))
	(colourHeight (- blurHeight (* borderSize 2)))
	(colourLoc (+ blurSize borderSize))

	(colourLayer)
	(mask)
	(blurLayer)
	(sizeError FALSE)

    )

    ;Check that the image is wide enough for the blur border
    (if (< blurWidth 0)
	(set! sizeError TRUE)
	())

    ;Check that the image is high enough for the blur border
    (if (< blurHeight 0)
	(set! sizeError TRUE)
	())

    ;Check that the image is wide enough for the colour border
    (if (< colourWidth 0)
	(set! sizeError TRUE)
	())

    ;Check that the image is high enough for the colour border
    (if (< colourHeight 0)
	(set! sizeError TRUE)
	())



    ;See if all the elements will fit in the image.
    (if (= sizeError TRUE)
	;Size is too small
	(gimp-message "The image is not big enough for those borders to fit")
	
	;Size is large enough
	(begin

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Select none
    (gimp-selection-none theImage)

    ;Add the colour layer
    (set! colourLayer (car (gimp-layer-new theImage imageWidth imageHeight 1 "Colour border" 100 0)))
    (gimp-image-add-layer theImage colourLayer -1)

    ;Set the foreground 
    (gimp-context-set-background borderColour)

    ;Fill the layer with BG colour
    (gimp-edit-fill colourLayer 1)

    ;Select the inner 
    (gimp-rect-select theImage colourLoc colourLoc colourWidth colourHeight 0 FALSE 0)

    ;Invert the selection
    (gimp-selection-invert theImage)

    ;Add a layer mask
    (set! mask (car (gimp-layer-create-mask colourLayer 4)))
    (gimp-layer-add-mask colourLayer mask)

    ;Select none
    (gimp-selection-none theImage)

    ;Copy the layer
    (set! blurLayer (car (gimp-layer-copy theLayer 1)))
    
    ;Add the blur layer to the image
    (gimp-image-add-layer theImage blurLayer 0)

    ;Rename the layer
    (gimp-drawable-set-name blurLayer "Blur layer")

    ;Select the blur 
    (gimp-rect-select theImage blurSize blurSize blurWidth blurHeight 2 FALSE 0)

    ;Invert the selection
    (gimp-selection-invert theImage)

    ;Blur the border
    (plug-in-gauss 1 theImage blurLayer blurAmount blurAmount 0)

    ;Add a layer mask
    (set! mask (car (gimp-layer-create-mask blurLayer 4)))
    (gimp-layer-add-mask blurLayer mask)

    ;Reset the background colour
    (gimp-context-set-background myBackground)

    ;Select none
    (gimp-selection-none theImage)

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)


))))


(script-fu-register "script-fu-blur-border"
            _"<Image>/Script-Fu/Borders/Blur Border..."
            "Gives a blurred border with a colour in between"
            "Harry Phillips"
            "Harry Phillips"
            "9 Feb 2008"
            "RGB*"
            SF-IMAGE		"Image"     0
            SF-DRAWABLE		"Drawable"  0
            SF-COLOR		"Border colour" '(255 255 255)
            SF-ADJUSTMENT	_"Border size"     '(50 0 100 1 10 1 0)
	    SF-ADJUSTMENT	_"Blur factor"         '(60 5 100 1 10 0 1)
	    SF-ADJUSTMENT	_"Blur border size"    '(400 0 1024 1 10 0 1)

)            






    





