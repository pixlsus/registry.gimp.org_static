; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; This program is free software: you can redistribute it and/or modify
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
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; =========================================================================
;
; Sepia toning:
;   - "classic" according to the Sepia Toning tutorial by Eric R. Jeschke
;     http://www.gimp.org/tutorials/Sepia_Toning/
;   - with hue modification of the original image
;
; Copyright (C) 2013 Jörg Knobloch - www.jorgk.com
;
; "Classic":
;   The script desaturates the original layer and adds a sepia mask in a layer above.
;   The sepia mask layer has a mask which is the inverted desaturated original.
; "With Hue":
;   The script desaturates the original layer by a given value and changes its hue.
;   It adds a sepia mask in a layer above.
;   The sepia mask layer has a mask which is the inverted desaturated original.
;   As blend modes "Color" or "Hue" can be used.
;
; Note: "With Hue" with hue=0, desaturation=100 and blend mode="color" is equivalent to "Classic".

(define (script-fu-sepia-plus image
                         drawable
						 sepia-color
						 sepia-type
						 sepia-hue
						 sepia-desat
						 sepia-mode
						 sepia-leave-mask)
    (let* (
        (sepia-layer 0)
		(mask 0)
		(float-sel 0)
		(image-width  (car (gimp-image-width image)))
		(image-height (car (gimp-image-height image)))
		
		; set values for classic type
		(hue-val 0)
		(de-sat -100)
		(mode-val COLOR-MODE)
	    )

    (if (= sepia-type 1)
        ; set values for hue type
        (begin 
            (set! hue-val sepia-hue)
	        (set! de-sat (- 0 sepia-desat))
	        (if (= sepia-mode 1)
	            (set! mode-val HUE-MODE)
	        )
	    )
    )

	(gimp-image-undo-group-start image)
	(gimp-hue-saturation drawable 0 hue-val 0 de-sat)
	(gimp-edit-copy drawable)
	(set! sepia-layer (car (gimp-layer-new image image-width image-height RGB-IMAGE "Sepia Mask" 100 mode-val)))
	(gimp-image-insert-layer image sepia-layer 0 -1)
	(gimp-context-set-foreground sepia-color)
	(gimp-drawable-fill sepia-layer FOREGROUND-FILL)
	(set! mask (car (gimp-layer-create-mask sepia-layer ADD-WHITE-MASK)))
	(gimp-layer-add-mask sepia-layer mask)
	(set! float-sel (car (gimp-edit-paste mask TRUE)))
	(gimp-invert float-sel)
	(gimp-floating-sel-anchor float-sel)

    (if (= sepia-leave-mask FALSE) 
        (gimp-image-merge-down image sepia-layer EXPAND-AS-NECESSARY)
    )
	
	(gimp-image-undo-group-end image)
	(gimp-displays-flush)
	)
)

(script-fu-register "script-fu-sepia-plus"
                    "Sepia-Plus..."
                    "Add sepia effect to layer"
                    "Jörg Knobloch"
                    "Copyright © 2013 Jörg Knobloch"
                    "2013-02-22"
                    "RGB* GRAY*"
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Layer" 0
                    SF-COLOR "Sepia color" '(162 138 101)
					SF-OPTION "Type" '("Classic" "With hue")
					SF-ADJUSTMENT "Hue" '(180 -180 180 1 10 0 SF-SLIDER)
					SF-ADJUSTMENT "Desaturation" '(60 0 100 1 10 0 SF-SLIDER)
                    SF-OPTION "Blend Mode" '("Color" "Hue")
                    SF-TOGGLE "Leave sepia mask as separate layer" FALSE
					)

(script-fu-menu-register "script-fu-sepia-plus"
                         "<Image>/Filters/Decor")