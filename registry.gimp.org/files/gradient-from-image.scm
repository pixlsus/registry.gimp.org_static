; gradient-from-image.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.1 (20090408)

; Description
;
; Script to create a gradient from a horizontal image of a gradient
; Will appear in the Gradients Menu
;
; Changes
; 1.1 - Added option to sample alpha as well.
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

(define (script-fu-gradient-from-image img inLayer inSegments inSmooth inName inAlpha)
  (let* 
    (
	  (img (car (gimp-image-duplicate img)))  ;create a duplicate and work on that destructively.
	  (width (car (gimp-image-width img)))
	  (height (car (gimp-image-height img)))
	  (segments (truncate inSegments))
	  (colors (+ segments (if (= inSmooth TRUE) 1 0)))
	  (theGradient "")
	  (counter 0)
	 	(varAlpha 100)
	 	(varNextAlpha 100)
	)
	
	;it begins here
 	(gimp-image-undo-group-start img)

	;flatten inage and get drawable (layer)
	(set! inLayer (car (gimp-image-merge-visible-layers img 1)))
	
	; blur then resize to number of colors  by 1
  (gimp-image-scale img colors 1)
	
	; set up selection as copy of alpha
 	(if (= inAlpha TRUE) (gimp-selection-layer-alpha inLayer))

  ;create new gradient
	(set! theGradient (car (gimp-gradient-new inName)))
	(gimp-context-set-gradient theGradient)
	
	;subdivide
	(gimp-gradient-segment-range-split-uniform theGradient 0 0 segments)

	(while (< counter segments)	
   	(if (= inAlpha TRUE)
      (begin
        (set! varAlpha (/ (* 100 (car (gimp-selection-value img counter 0))) 255))
        (set! varNextAlpha (/ (* 100 (car (gimp-selection-value img (+ counter 1) 0))) 255))
      )
    )
	  (gimp-gradient-segment-set-left-color theGradient counter (car (gimp-image-pick-color img inLayer counter 0 FALSE FALSE 0)) varAlpha)
	  (gimp-gradient-segment-set-right-color theGradient counter (car (gimp-image-pick-color img inLayer (+ counter (if (= inSmooth TRUE) 1 0)) 0 FALSE FALSE 0)) (if (= inSmooth TRUE) varNextAlpha varAlpha))
		(set! counter (+ counter 1))
	)
	
	;done
	(gimp-image-undo-group-end img)
	(gimp-image-delete img)
  )
)

(script-fu-register "script-fu-gradient-from-image"
        		    "<Gradients>/Gradient from _Image..."
                    "Create a gradient from an image of a gradient."
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Jan 2008"
                    ""
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-ADJUSTMENT "Segments in the gradient" (list 25 2 256 1 10 0 SF-SLIDER)
                    SF-TOGGLE     "Smooth Gradient" TRUE
                    SF-STRING     "Gradient Name"  "Gradient From Image"				
                    SF-TOGGLE     "Sample Transparency" FALSE
)				