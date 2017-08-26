; Fix My Pix
; Automated script to enhance the contrast of low-contrast images

; Copyright 2014 Baruch Ben-David (baruch@cpan.org)

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
; A copy of the GNU General Public License is available from:
; <http://www.gnu.org/licenses/>.


(define (script-fu-fix-my-pix inImage inDrawable )
  (gimp-image-undo-group-start inImage) 
  (gimp-context-push) ; Preserve user's settings

  (let* (
	 (base-layer 0) ; Don't want to touch user's layer
	 (new-image 0)
	 (layers 0)
	 (val-layer 0)
	 (sat-layer 0)
	 (hue-layer 0)
	 )
    (gimp-item-set-visible inDrawable TRUE) ; Make sure active layer is visible
    (gimp-image-raise-item-to-top inImage inDrawable) ; Bring it to the top of the stack

    (set! base-layer (car (gimp-layer-copy inDrawable 0)))
    (gimp-drawable-set-name base-layer _"HSL Base Layer")
    (gimp-image-add-layer inImage base-layer 0)
    (gimp-layer-set-opacity base-layer 100)
    (gimp-layer-set-mode base-layer NORMAL-MODE)
 
    (set! new-image (car (plug-in-decompose RUN-NONINTERACTIVE inImage base-layer "HSL" TRUE)))
   
    ; Get layers info, unravel the vector, and select the L layer
    (set! layers (cadr (gimp-image-get-layers new-image)))
    (set! val-layer (vector-ref layers 2))
    (set! sat-layer (vector-ref layers 1))
    (set! hue-layer (vector-ref layers 0))
    (gimp-levels-stretch val-layer)

    (plug-in-recompose RUN-NONINTERACTIVE new-image val-layer)

    (gimp-levels-stretch base-layer) ; See if it will stretch a bit more...

    ; Clean up after ourselves
    (gimp-image-delete new-image)
    (gimp-drawable-set-name base-layer _"HSL Decomp"); Helpful to identify the new layer
    )
    (gimp-context-pop) ; Restore user's settings
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
  )
  
(script-fu-register "script-fu-fix-my-pix"
		    _"Fix My Pix"
		    _"Adjusts contrast by stretching the intensity values on the L channel of the HSL decomposition.  This script may enhance some low-contrast images to create a more pleasing result."
		    "Baruch Ben-David"
		    "Baruch Ben-David"
		    "March 1, 2014"
		    "RGB*"
SF-IMAGE "Image" 0
SF-DRAWABLE "Current Layer" 0
)
(script-fu-menu-register "script-fu-fix-my-pix"
			 "<Image>/Script-Fu/Simple")


