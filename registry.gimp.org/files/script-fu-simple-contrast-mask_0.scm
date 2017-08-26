;
; Simple Contrast Mask
;
; (C) 2010 Andrew Robinson awrobinson@aol.com
;
; Simple Contrast Mask
;   Create a contrast mask layer. The script-fu is called 
;   simple because it does not pop up any dialog boxes. It 
;   sets defaults and goes. I find this advantageous when I 
;   am editing lots and lots of images, such as after a day 
;   of shooting with my digitalcamera. "Stupid" might be a 
;   better name. 

; Acknowledgements:
;   script-fu-template by Simon Budig
;   script-fu contrast mask example by Tanked Up Underwater 
;     Imaging

; Contrast Mask - mimics the operation done in film 
;   processing. A copy of the image is desaturated, inverted,
;   and blurred. It it blended back with the original image
;   in overlay mode. This serves to enhance contrast in dark
;   and light areas. 

; Script-fu definition of varibles
; The script is works on the background layer of the current
; image. There are no user input variables
;
; image - the current image
; active-layer - the background layer
; width, height - width and height of current image.
; blur-radius - the radius for the Gausian blur. Calculated as
;   one percent of the shorter of the width or height
; contrast-mask - the layer id of the, you guessed it, 
;   contrast mask.
;

(define (script-fu-simple-contrast-mask image drawable)

    ; Local variables

    (let* (
        (active-layer   (car  (gimp-image-get-active-drawable image)))
        (width  (car  (gimp-drawable-width active-layer)))
        (height (car  (gimp-drawable-height active-layer)))
        (blur-radius (quotient width 100))
        (if (< height width) (blur-radius (quotient height 100)))
        (contrast-mask (car (gimp-layer-copy active-layer TRUE)))
        )

        ; Preserve the original context

        (gimp-context-push)

        ; Enable UNDO

         (gimp-image-undo-group-start image)

        ; Copy the background layer and add it to the
        ; image just above that layer

        (gimp-image-add-layer image contrast-mask -1)
        (gimp-layer-set-name contrast-mask "Contrast Mask")

        ; Turn the layer into a contrast mask by 
        ; desaturating, inverting, blurring, and 
        ; blending in overlay mode
        
        (gimp-desaturate contrast-mask)
        (gimp-invert contrast-mask)
        (plug-in-gauss-iir 1 image contrast-mask blur-radius 1 1)
        (gimp-layer-set-mode contrast-mask OVERLAY-MODE)
        (gimp-layer-set-opacity contrast-mask 75)

        ; End undo group

        (gimp-image-undo-group-end image)

        ; Restore the context saved earlier again.

        (gimp-context-pop)

        ; finally we notify the UI that something has changed.

        (gimp-displays-flush)
    )
)

; Register the script

(script-fu-register "script-fu-simple-contrast-mask"
		    "Simple Contrast Mask"
		    "Adds a contrast mask layer to the active image"
		    "Andrew Robinson <awrobinson@aol.com>"
		    "Copyright 2010 Andrew Robinson"
		    "2010-06-05"
		    "RGB* GRAY*"
		    SF-IMAGE "Input Image" 0
		    SF-DRAWABLE "Input Drawable" 0
)

(script-fu-menu-register "script-fu-simple-contrast-mask" "<Image>/Colors")

