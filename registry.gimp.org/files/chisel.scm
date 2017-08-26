; chisel.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.2 (20080812)

; Description
;
; creates a hard chisel bevel on the selection, or using the layer alpha
;

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (script-fu-chisel img inLayer inWidth inSoften inCurve inAizmuth inElevation inDepth inMode inLocation inBlur inKeepBump)
  (let*
    (
	   (varNoSelection (car (gimp-selection-is-empty img)))
       (varSavedSelection 0)
	   (varBlurredSelection 0)
	   (varBumpmapLayer)
	   (varBevelLayer)
	   (varLoopCounter 0)
	   (varFillValue)
	   (varNumBytes 256)
	   (varAdjCurve    (cons-array varNumBytes 'byte))
	   (varLayerName (car (gimp-drawable-get-name inLayer)))
    )
    ;  it begins here
    (gimp-context-push)
    (gimp-image-undo-group-start img)
	
	;save selection or select all if no selection
	(if (= varNoSelection TRUE)
	  (if (= (car (gimp-drawable-has-alpha inLayer)) TRUE)  ;check for alpha
	    (gimp-selection-layer-alpha inLayer) ;  transfer the alpha to selection
	    (gimp-selection-all img)  ;else select the whole image
      )
	)
	(set! varSavedSelection (car (gimp-selection-save img)))
	
	(set! varBumpmapLayer (car (gimp-layer-new-from-drawable inLayer img)))
    (gimp-drawable-set-name varBumpmapLayer (string-append varLayerName " bumpmap"))
	(gimp-image-add-layer img varBumpmapLayer -1)
	(if (= inLocation 1) ;if outside, enlarge the layer canvas
	  (gimp-layer-resize varBumpmapLayer (+ (car (gimp-drawable-width inLayer)) (* 2 inWidth))
	                                   (+ (car (gimp-drawable-height inLayer)) (* 2 inWidth))
									   inWidth
									   inWidth)
	)
	
	;blur selection for soft chisel
	(gimp-selection-feather img inSoften)
	(set! varBlurredSelection (car (gimp-selection-save img)))
	
	
	; create bevel in bumpmap layer black to white
    (gimp-context-set-foreground '(0 0 0))
    (gimp-drawable-fill varBumpmapLayer FOREGROUND-FILL)

	
	(while (<= varLoopCounter inWidth)
	  (set! varFillValue (/ (* varLoopCounter 255) inWidth))
		
	  ;avoid distortion
	  (gimp-selection-load varBlurredSelection)	
	  
	  (if (= inLocation 0)
	    (gimp-selection-shrink img varLoopCounter) ;inside
	    (gimp-selection-grow img (- inWidth varLoopCounter)) ;outside
      )
	  
	  (gimp-context-set-foreground (list varFillValue varFillValue varFillValue)) ;shade of grey
		
	  (if (= (car (gimp-selection-is-empty img)) FALSE)
        (gimp-edit-fill varBumpmapLayer FOREGROUND-FILL) 
		(set! varLoopCounter (+ inWidth 1))
      )
		
	  (set! varLoopCounter (+ varLoopCounter 1))
	)
    (gimp-selection-none img)
   
    ;stretch across full range before curve adjust
    (plug-in-c-astretch RUN-NONINTERACTIVE img varBumpmapLayer)
	
	;curve adjust
	(set! varLoopCounter 0)
	(while (< varLoopCounter varNumBytes)
	  (aset varAdjCurve   varLoopCounter (round (* 255 (pow (/ varLoopCounter varNumBytes) inCurve))))
      (set! varLoopCounter (+ varLoopCounter 1))
	)
	(gimp-curves-explicit varBumpmapLayer HISTOGRAM-VALUE varNumBytes varAdjCurve)  
	
    ;make bevel from  bumpmap
 	(set! varBevelLayer (car (gimp-layer-new-from-drawable inLayer img)))
    (gimp-drawable-set-name varBevelLayer (string-append varLayerName " bevel"))
	(gimp-image-add-layer img varBevelLayer -1)	
	(if (= inLocation 1) ;if outside, enlarge the layer canvas
	  (gimp-layer-resize varBevelLayer (+ (car (gimp-drawable-width inLayer)) (* 2 inWidth))
	                                   (+ (car (gimp-drawable-height inLayer)) (* 2 inWidth))
									   inWidth
									   inWidth)
	)

    (gimp-context-set-foreground '(127 127 127))
    (gimp-drawable-fill varBevelLayer FOREGROUND-FILL)

	(plug-in-bump-map RUN-NONINTERACTIVE img varBevelLayer varBumpmapLayer inAizmuth inElevation inDepth 0 0 0 0 
	                  TRUE (cond ((= inMode 0) FALSE) ((= inMode 1) TRUE)) LINEAR)
	(gimp-layer-set-mode varBevelLayer HARDLIGHT-MODE)
	
	;delete outside the desited bevel
    (if (= inLocation 0)
	  (begin ;inside
   	    (gimp-selection-load varSavedSelection)
		(gimp-selection-invert img)
		(if (= (car (gimp-selection-is-empty img)) FALSE)
          (gimp-edit-clear varBevelLayer)
        )
   	    (gimp-selection-load varSavedSelection)
	    (gimp-selection-shrink img inWidth)
		(if (= (car (gimp-selection-is-empty img)) FALSE)
          (gimp-edit-clear varBevelLayer)
        )
      )		
	  (begin ;outside
   	    (gimp-selection-load varSavedSelection)
		(if (= (car (gimp-selection-is-empty img)) FALSE)
          (gimp-edit-clear varBevelLayer)
        )
   	    (gimp-selection-load varSavedSelection)
	    (gimp-selection-grow img inWidth)
		(gimp-selection-invert img)
		(if (= (car (gimp-selection-is-empty img)) FALSE)
          (gimp-edit-clear varBevelLayer)
        )
      )		
	)

	; blur if desired
    (if (> inBlur 0)
	  (begin
	  	(gimp-selection-none img)
	    (gimp-layer-set-lock-alpha varBevelLayer TRUE)
		(plug-in-gauss RUN-NONINTERACTIVE img varBevelLayer inBlur inBlur 0)
	    (gimp-layer-set-lock-alpha varBevelLayer FALSE)
	  )
	)
	
	;delete bumpmap layer
	(if (= inKeepBump TRUE)
	  (gimp-drawable-set-visible varBumpmapLayer FALSE)
	  (gimp-image-remove-layer img varBumpmapLayer)
	)
	
    ;load initial selection back up 
	(if (= varNoSelection TRUE)
	  (gimp-selection-none img)
	  (begin
	    (gimp-selection-load varSavedSelection)
	  )
	)

	;and delete the channels
	(gimp-image-remove-channel img varSavedSelection)
	(gimp-image-remove-channel img varBlurredSelection)
	
	(gimp-image-set-active-layer img inLayer)
	
	;done
    (gimp-progress-end)
	(gimp-image-undo-group-end img)
	(gimp-displays-flush)
	(gimp-context-pop)
  )
)

(script-fu-register "script-fu-chisel"
        		    "<Image>/Filters/Decor/_Chisel..."
                    "Create a Chisel Effect"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "July 2008"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-ADJUSTMENT "Bevel Width" '(20 2 256 1 5 0 0)
                    SF-ADJUSTMENT "Bevel Softness" '(0 0 20 1 5 0 0)
                    SF-ADJUSTMENT "Bevel Curve" '(1 0.1 10 0.1 1 1 0)
                    SF-ADJUSTMENT "Azimuth" '(135 0 360 1 5 0 0)
                    SF-ADJUSTMENT "Elevation" '(20 0.5 90 1 5 1 0)
                    SF-ADJUSTMENT "Depth" '(20 1 65 1 5 0 0)
                    SF-OPTION     "Mode" '("Chisel Edges" "Carve")					
                    SF-OPTION     "Location" '("Inside" "Outside")					
                    SF-ADJUSTMENT "Effect Blur" '(0 0 20 1 5 0 0)					
				    SF-TOGGLE     "Keep Bumpmap?" FALSE)