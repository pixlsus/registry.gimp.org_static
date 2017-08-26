; EvenTone is a script for The GIMP
;
; Evens out tonal values in an image. Works great for weird lighting in photographs
; and hiding skin blemishes.
;
; The script is located in "<Image> / Script-Fu / EvenTone "
;
; Last changed: 14 June 2008
;
; I shamelessly copied Harry Phillips' d-dayed script.
;
; Copyright (C) 2009 Ian Ganse <ian.ganse@gamil.com>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 0.1
;    - Initial release based on the 'd-dayed' script.
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


(define (script-fu-even-tone	 	theImage
					theLayer
					blurAmount
					)

    ;Initialize some variables
    (let* (
	(layerGrey (car (gimp-drawable-is-gray theLayer)))
	(sharpLayerTwo)
	(screenLayer)
	(multiplyLayer)
	(softFocusLayer)
	(saturationLayer)
	(sharpLayerOne (car (gimp-layer-copy theLayer 1)))
	(merged)
	(screenMask)
	(multiplyMask)
    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Select none
    (gimp-selection-none theImage)

	;;;;;;;;;;;;;;;;;;;;
	; Soft focus Layer ;
	;;;;;;;;;;;;;;;;;;;;

    ;Copy the layer for the screen
    (set! softFocusLayer (car (gimp-layer-copy theLayer 1)))
    (gimp-image-add-layer theImage softFocusLayer 0)

    ;Rename the layer
    (gimp-drawable-set-name softFocusLayer "Soft Focus")

    ;Blur the layer
    (plug-in-gauss 1 theImage softFocusLayer blurAmount blurAmount 0)

    ;Change the screen layer mode to Soft Light
    (gimp-layer-set-mode softFocusLayer 19)

	;;;;;;;;;;;;;;;;;;
	; Multiply Layer ;
	;;;;;;;;;;;;;;;;;;

    ;Copy the layer for the screen
    (set! multiplyLayer (car (gimp-layer-copy theLayer 1)))
    (gimp-image-add-layer theImage multiplyLayer 0)

    ;Rename the layer
    (gimp-drawable-set-name multiplyLayer "Darken highlights")

    ;Add a greyscale copy layer mask
    (set! multiplyMask (car (gimp-layer-create-mask multiplyLayer 5)))
    (gimp-layer-add-mask multiplyLayer multiplyMask)

    ;Change the screen layer mode to Multiply
    (gimp-layer-set-mode multiplyLayer 3)

	;;;;;;;;;;;;;;;;
	; Screen Layer ;
	;;;;;;;;;;;;;;;;

    ;Copy the layer for the screen
    (set! screenLayer (car (gimp-layer-copy theLayer 1)))
    (gimp-image-add-layer theImage screenLayer 0)

    ;Rename the layer
    (gimp-drawable-set-name screenLayer "Lighten shadows")

    ;Add a greyscale copy layer mask and invert it
    (set! screenMask (car (gimp-layer-create-mask screenLayer 5)))
    (gimp-layer-add-mask screenLayer screenMask)
    (gimp-invert screenMask)

    ;Change the screen layer mode to Screen
    (gimp-layer-set-mode screenLayer 4)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; High Pass Sharpen Layer ;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;Copy the original layer
    (gimp-image-add-layer theImage sharpLayerOne 0)

    ;Rename the layer
    (gimp-drawable-set-name sharpLayerOne "High Pass Sharpen")

    ;Change the layer Greyscale if it isn't already
    (if (= layerGrey 0) (gimp-desaturate sharpLayerOne))
    
    ;Copy the layer
    (set! sharpLayerTwo (car (gimp-layer-copy sharpLayerOne 1)))
    (gimp-image-add-layer theImage sharpLayerTwo 0)
    
    ;Invert the layer
    ;(gimp-invert sharpLayerTwo)
    
    ;Change the layers mode to Grain Extract (Multiply is 3)
    (gimp-layer-set-mode sharpLayerTwo 20)
    
    ;Blur the layer
    (plug-in-gauss 1 theImage sharpLayerTwo blurAmount blurAmount 0)

    ;Merge the top layer down and keep track of the newly merged layer
    (set! merged (car (gimp-image-merge-down theImage sharpLayerTwo 0)))
    
    ;Add a greyscale copy layer mask
    ;(set! mask (car (gimp-layer-create-mask merged 5)))
    ;(gimp-layer-add-mask merged mask)

    ;Change the effect layer mode to Overlay
    (gimp-layer-set-mode merged 5)

	;;;;;;;;;;;;;;;;;;;;
	; Saturation Layer ;
	;;;;;;;;;;;;;;;;;;;;

    ;Copy the layer for the screen
    (set! saturationLayer (car (gimp-layer-copy theLayer 1)))
    (gimp-image-add-layer theImage saturationLayer 0)

    ;Rename the layer
    (gimp-drawable-set-name saturationLayer "Saturation")

    ;Change the screen layer mode to Saturation
    (gimp-layer-set-mode saturationLayer 12)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Done with adding layers ;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)
    
    )
)

(script-fu-register "script-fu-even-tone"
            _"<Image>/Filters/Light and Shadow/EvenTone"
            "Even out the tonal values in an image, good for fixing weird light and skin blemishes"
            "Ian Ganse"
            "Ian Ganse"
            "Jan. 7 2009"
            "*"
            SF-IMAGE		"Image"     0
            SF-DRAWABLE		"Drawable"  0
	    SF-ADJUSTMENT	_"Blur factor"      '(20 5 10000 1 1 0 1)

)
