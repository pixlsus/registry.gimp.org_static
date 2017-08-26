; ------------------------------------------------------------------------------
; Adjoin 2 separate layers into stereo pair
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
; V1.0 (2008-10-07) Tested and working with Gimp 2.4.5, Gimp 2.6
; -------------------------------------------------------------------------------------
(define (script-fu-adjoinLayersIntoSteropair img drawable layer)
	(let*	; @note: Scope of the following variables ends with mathching brace to the one before "let*"
		( (width (car (gimp-drawable-width drawable)))				;image width
		  (height (car (gimp-drawable-height drawable)))			;image height
		  (curLayer)
		  
		) ; --------------------------------------------------------------------
		(gimp-layer-set-offsets layer width 0)					;move layer to right of lower layer
		(gimp-image-resize-to-layers img)					;resize canvas fitting to layers
		(gimp-image-set-active-layer img layer)					;activate layer with right image
		(gimp-image-merge-visible-layers img 0)					;merge visible layers
          
		; ---------------------------------------------------------------------- done. cleanup:
		(gimp-selection-clear img)						;clear selection
	)
	(gimp-displays-flush)								;flush output
 )	; ------------------------------------------------------------------------------ register (into image/Script-Fu menu):
(script-fu-register "script-fu-adjoinLayersIntoSteropair"
                    _"<Image>/Script-Fu/Stereo imaging/Adjoin 2 separate layers into stereo pair"
		     "Adjoin 2 separate layers into stereo pair"
		     "Kay Stenschke <info@stenschke.com>"
                     "Kay Stenschke"
                     "2008-26-08"
                     "RGB*, GRAY*" 
                     SF-IMAGE    "Image" 0
		     SF-DRAWABLE "Drawable" 0
                     SF-LAYER	 "Select right image layer" 0				;show dialog to select right layer on start
)	; ------------------------------------------------------------------------------
