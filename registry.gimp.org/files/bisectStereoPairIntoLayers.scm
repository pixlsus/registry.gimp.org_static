; ------------------------------------------------------------------------------
; Bisect stereo pair into 2 separate layers
; 
; (c) 2008 Kay Stenschke <info@stenschke.com>
; All rights reserved
;
; This script is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; The GNU General Public License can be found at
; http://www.gnu.org/copyleft/gpl.html.
;
;
; Changelog: (add new entries on top)
; V1.0 (2008-26-08) Tested and working with Gimp 2.4.5
; -------------------------------------------------------------------------------------
(define (script-fu-bisectStereoPairIntoLayers img drawable)
	(let*	; @note: Scope of the following variables ends with mathching brace to the one before "let*"
		( (width (car (gimp-drawable-width drawable)))				;image width
		  (height (car (gimp-drawable-height drawable)))			;image height
		  (layer)
		) ; --------------------------------------------------------------------
		(gimp-drawable-set-name drawable "Original stereo pair")		;rename original layer
		(gimp-rect-select img 0 0 (/ width 2) height REPLACE FALSE 0)		;select left half
		(gimp-edit-copy drawable)						;copy
		(set! layer (car (gimp-edit-paste drawable -1)))			;paste, get handle
		(gimp-floating-sel-to-layer layer)					;create layer from selection (handle)
		(gimp-drawable-set-name layer "Left image")				;change layer name
		(gimp-rect-select img (/ width 2) 0 (/ width 2) height REPLACE FALSE 0);select right half
		(gimp-edit-copy drawable)						;copy
		(set! layer (car (gimp-edit-paste drawable -1)))			;paste, get handle
		(gimp-floating-sel-to-layer layer)					;create layer from selection (handle)
		(gimp-layer-set-offsets layer 0 0)					;move right image layer to left top
		(gimp-drawable-set-name layer "Right image")				;change layer name
		(gimp-layer-set-visible drawable FALSE)					;switch original layer invisible
		(gimp-image-crop img (/ width 2) height 0 0)				;crop image to 1/2 of the original width
		; ---------------------------------------------------------------------- done. cleanup:
		(gimp-selection-clear img)						;clear selection
	)
	(gimp-displays-flush)								;flush output
 )	; ------------------------------------------------------------------------------ register (into image/Script-Fu menu):
(script-fu-register "script-fu-bisectStereoPairIntoLayers"
                    _"<Image>/Script-Fu/Stereo imaging/Bisect stereo pair into layers"
		     "Bisect stereo pair into two separate layers"
		     "Kay Stenschke <info@stenschke.com>"
                     "Kay Stenschke"
                     "2008-26-08"
                     "RGB*, GRAY*" 
                     SF-IMAGE    "Image" 0
		     SF-DRAWABLE "Drawable" 0
)	; ------------------------------------------------------------------------------
