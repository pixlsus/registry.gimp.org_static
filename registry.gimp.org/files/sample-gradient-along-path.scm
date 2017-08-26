; sample-gradient-along-path.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.2 (20100625)

; Description
;
; Script to create a gradient by sampling along a path
; Will appear in the Gradients Menu
;
; Changes
; 1.1 - Added option to sample alpha as well.
; 1.2 - Clamp on image boundaries to fix error when path is outside image
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
(define (script-fu-sample-gradient-along-path img inLayer inPath inSamples inRadius inSmooth inName inAlpha)
  (let* 
    (
	  (varSegments (truncate (- inSamples (if (= inSmooth TRUE) 1 0))))
	  (varGradient "")
	  (varFirstStroke (aref (cadr (gimp-vectors-get-strokes inPath)) 0))
	  (varPathLength (car (gimp-vectors-stroke-get-length inPath varFirstStroke 1)))
 	  (varCheck (list-ref (gimp-vectors-stroke-get-point-at-dist inPath varFirstStroke varPathLength 1) 3))
	  (varCounter 0)
	  (varPos 0)
	  (varNextPos 0)
	  (varAlpha 100)
	  (varNextAlpha 100)
	  (varSelEmpty (car (gimp-selection-is-empty img)))
	  (varSelOrig 0)
      (varWidth (car (gimp-image-width img)))
      (varHeight (car (gimp-image-height img)))
	)
	
	  ; it begins here
	  (gimp-context-push)
	  (gimp-image-undo-group-start img)
    ;save selection
    (if (= varSelEmpty FALSE) 
      (begin
        (set! varSelOrig (car (gimp-selection-save img)))
        (gimp-selection-none img)
      )
    )
    ;backtrack to get last good length
    (while (= varCheck FALSE)
      (set! varPathLength (- varPathLength 0.001))
      (set! varCheck (list-ref (gimp-vectors-stroke-get-point-at-dist inPath varFirstStroke varPathLength 1) 3))
    )
	
	  ; set up selection as copy of alpha
	  (if (= inAlpha TRUE) (gimp-selection-layer-alpha inLayer))
	
    ;create new gradient
    (set! varGradient (car (gimp-gradient-new inName)))
    (gimp-context-set-gradient varGradient)
	
    ;subdivide
    (gimp-gradient-segment-range-split-uniform varGradient 0 0 varSegments)

    (while (< varCounter varSegments)	
      (set! varPos (gimp-vectors-stroke-get-point-at-dist inPath varFirstStroke (* varPathLength (/ varCounter varSegments)) 1))
      (set! varNextPos (gimp-vectors-stroke-get-point-at-dist inPath varFirstStroke (* varPathLength (/ (+ varCounter 1) varSegments)) 1))
   	  (if (= inAlpha TRUE)
        (begin
          (set! varAlpha (/ (* 100 (car (gimp-selection-value img (max (min (list-ref varPos 0) varWidth) 0) (max (min (list-ref varPos 1) varHeight) 0)))) 255))
          (set! varNextAlpha (/ (* 100 (car (gimp-selection-value img (max (min (list-ref varNextPos 0) varWidth) 0) (max (min (list-ref varNextPos 1) varHeight) 0)))) 255))
        )
      )
      (gimp-gradient-segment-set-left-color varGradient varCounter (car (gimp-image-pick-color img inLayer (max (min (list-ref varPos 0) (- varWidth inRadius)) inRadius) (max (min (list-ref varPos 1) (- varHeight inRadius)) inRadius) TRUE TRUE inRadius)) varAlpha)
;      (gimp-gradient-segment-set-left-color varGradient varCounter (car (gimp-image-pick-color img inLayer (max (min (list-ref varPos 0) varWidth) 0) (max (min (list-ref varPos 1) varHeight) 0) TRUE TRUE inRadius)) varAlpha)
      (if (= inSmooth TRUE) 
        (gimp-gradient-segment-set-right-color varGradient varCounter (car (gimp-image-pick-color img inLayer (max (min (list-ref varNextPos 0) (- varWidth inRadius)) inRadius) (max (min (list-ref varNextPos 1) (- varHeight inRadius)) inRadius) TRUE TRUE inRadius)) varNextAlpha)
;        (gimp-gradient-segment-set-right-color varGradient varCounter (car (gimp-image-pick-color img inLayer (max (min (list-ref varNextPos 0) varWidth) 0) (max (min (list-ref varNextPos 1) varHeight) 0) TRUE TRUE inRadius)) varNextAlpha)
      ;else
        (gimp-gradient-segment-set-right-color varGradient varCounter (car (gimp-image-pick-color img inLayer (max (min (list-ref varPos 0) (- varWidth inRadius)) inRadius) (max (min (list-ref varPos 1) (- varHeight inRadius)) inRadius) TRUE TRUE inRadius)) varAlpha)
;        (gimp-gradient-segment-set-right-color varGradient varCounter (car (gimp-image-pick-color img inLayer (max (min (list-ref varPos 0) varWidth) 0) (max (min (list-ref varPos 1) varHeight) 0) TRUE TRUE inRadius)) varAlpha)
      )  
      (set! varCounter (+ varCounter 1))
	)
	
  (if (= varSelEmpty FALSE) 
    (begin
      (gimp-selection-load varSelOrig)
      (gimp-image-remove-channel img varSelOrig)
    )
    (gimp-selection-none img)
  )
	;done
	(gimp-image-undo-group-end img)
  (gimp-context-pop)
  )
)

(script-fu-register "script-fu-sample-gradient-along-path"
                    "<Gradients>/Sample Gradient along a _Path..."
                    "Create a gradient by sampling along the active path."
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Oct 2008"
                    ""
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-VECTORS    "Path" -1
                    SF-ADJUSTMENT "Number of Samples" (list 25 2 256 1 10 0 SF-SLIDER)
                    SF-ADJUSTMENT "Sample Average Radius" (list 3 1 10 1 2 0 SF-SLIDER)
                    SF-TOGGLE     "Smooth Gradient" TRUE
                    SF-STRING     "Gradient Name"  "Sampled Gradient"
                    SF-TOGGLE     "Sample Transparency" FALSE
)