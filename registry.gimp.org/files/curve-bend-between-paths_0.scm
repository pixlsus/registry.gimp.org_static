; curve-bend-between-paths.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.3 (20120814)

; Description
;
; Performs a curve bend of the current layer between two paths
; The paths must each have no more than 17 points and should go
; from left to right
;
; Changes
; v1.1 added an option to resize the image to fit layer
; v1.2 Fixed scaling a bit and added a compensation slider
; v1.3 Added a prescale up/postscale down option to reduce the amount of aliasing in the curve bend.

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
(define (script-fu-curve-bend-between-paths img inLayer inTop inBottom inComp inQual inNew? inAfter)
  (let* 
    (
      (width (car (gimp-drawable-width inLayer)))
      (height (car (gimp-drawable-height inLayer)))
      (inComp (/ 1 (exp inComp)))
      (strokeTop (gimp-vectors-stroke-get-points inTop (aref (cadr (gimp-vectors-get-strokes inTop)) 0)))
      (pointsTop (caddr strokeTop))
      (numTop (/ (cadr strokeTop) 6))
      (strokeBottom (gimp-vectors-stroke-get-points inBottom (aref (cadr (gimp-vectors-get-strokes inBottom)) 0)))
      (pointsBottom (caddr strokeBottom))
      (numBottom (/ (cadr strokeBottom) 6))
      (listTopX ())
      (listTopY ())
      (listBottomX ())
      (listBottomY ())
      (minTopY height)      
	  (maxTopY 0)
      (minBottomY height)      
      (maxBottomY 0)      
      (upperPointX (cons-array 17 'double))
      (upperPointY (cons-array 17 'double))
      (lowerPointX (cons-array 17 'double))
      (lowerPointY (cons-array 17 'double))    
      (counter 0)
      (result 0)
    )

    
    ; it begins here
	(gimp-context-push)
	(gimp-image-undo-group-start img)

    ; scale up if required for better quality    
    (when (> inQual 0)
      (gimp-image-scale img (car (gimp-image-width img)) (* 2 inQual (car (gimp-image-height img))))
      (set! height (car (gimp-drawable-height inLayer)))      
      (set! minTopY height)      
      (set! minBottomY height) 
      (set! strokeTop (gimp-vectors-stroke-get-points inTop (aref (cadr (gimp-vectors-get-strokes inTop)) 0)))
      (set! pointsTop (caddr strokeTop))
      (set! strokeBottom (gimp-vectors-stroke-get-points inBottom (aref (cadr (gimp-vectors-get-strokes inBottom)) 0)))
      (set! pointsBottom (caddr strokeBottom))
    )
    
    ;strip out nodes into lists
    (while (< counter (min numTop 17))
      (set! listTopX (append listTopX (list (max (min (vector-ref pointsTop (+ (* counter 6) 2)) width) 0))))
      (set! listTopY (append listTopY (list (max (min (vector-ref pointsTop (+ (* counter 6) 3)) height) 0))))
      (if (< (vector-ref pointsTop (+ (* counter 6) 3)) minTopY)
        (set! minTopY (vector-ref pointsTop (+ (* counter 6) 3))))
      (if (> (vector-ref pointsTop (+ (* counter 6) 3)) maxTopY)
        (set! maxTopY (vector-ref pointsTop (+ (* counter 6) 3))))		
      (set! counter (+ counter 1)))
      
    (set! counter 0)
    (while (< counter (min numBottom 17))
      (set! listBottomX (append listBottomX (list (max (min (vector-ref pointsBottom (+ (* counter 6) 2)) width) 0))))
      (set! listBottomY (append listBottomY (list (max (min (vector-ref pointsBottom (+ (* counter 6) 3)) height) 0))))
      (if (< (vector-ref pointsBottom (+ (* counter 6) 3)) minBottomY)
        (set! minBottomY (vector-ref pointsBottom (+ (* counter 6) 3))))
      (if (> (vector-ref pointsBottom (+ (* counter 6) 3)) maxBottomY)
        (set! maxBottomY (vector-ref pointsBottom (+ (* counter 6) 3))))
      (set! counter (+ counter 1)))

    ;make X a fraction of width
    (set! listTopX (map (lambda (x) (/ x width)) listTopX))
    (set! listBottomX (map (lambda (x) (/ x width)) listBottomX))
        
    ;set the curves to be 0 offset from max and min, so will be from 0..maxval
    (set! listTopY (map (lambda (x) (- x minTopY)) listTopY))
    (set! listBottomY (map (lambda (x) (- x minBottomY)) listBottomY))
    
    ;set Y deviation from a straight line as fraction of image proportion
    (set! listTopY (map (lambda (x) (/ x (* height inComp (/ (car (gimp-drawable-width inLayer)) (car (gimp-drawable-height inLayer)))))) listTopY))
    (set! listBottomY (map (lambda (x) (/ x (* height inComp (/ (car (gimp-drawable-width inLayer)) (car (gimp-drawable-height inLayer)))))) listBottomY))
    
    ;build control points for curve bend call 
    (set! counter 0)
    (while (< counter 17) ; always 17 control points max
      (if (< counter numTop)
        (begin 
          (aset upperPointX counter (list-ref listTopX counter))
          (aset upperPointY counter (list-ref listTopY counter)))
        (begin 
          (aset upperPointX counter -1)
          (aset upperPointY counter -1)))
      (if (< counter numBottom)
        (begin 
          (aset lowerPointX counter (list-ref listBottomX counter))
          (aset lowerPointY counter (list-ref listBottomY counter)))
        (begin 
          (aset lowerPointX counter -1)
          (aset lowerPointY counter -1)))
      (set! counter (+ counter 1)))
      
    ;call the curve-bend plugin
	(set! result (plug-in-curve-bend RUN-NONINTERACTIVE 
                        img inLayer 0 TRUE TRUE inNew? 0 ; smooth
                        numTop upperPointX numTop upperPointY
                        numBottom lowerPointX numBottom lowerPointY
                        256 (cons-array 256 'byte) 256 (cons-array 256 'byte))) ; dummies needed for call
                        
    ;set up new layer if requested
    (when (equal? inNew? TRUE)
      (set! inLayer (car result))
      (gimp-drawable-set-name inLayer (string-append "Curve Bend - " (car (gimp-drawable-get-name inLayer))))
      (gimp-drawable-set-visible inLayer TRUE))
      
    ;scale back if required
    (if (= inQual 1) (gimp-image-scale img (car (gimp-image-width img)) (* 0.5 (car (gimp-image-height img)))))
    (if (= inQual 2) (gimp-image-scale img (car (gimp-image-width img)) (* 0.25 (car (gimp-image-height img)))))    
      
    ;resize canvas to fit
    (if (equal? inAfter 1) (gimp-image-resize-to-layers img))
	  
    ;done
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
  )
)

(script-fu-register "script-fu-curve-bend-between-paths"
                    "<Image>/Filters/Distorts/Distortion Correction Between Paths..."
                    "Curve Bends between a top and bottom path."
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Oct 2009"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"          0
                    SF-DRAWABLE   "drawable"       0
                    SF-VECTORS    "Top Curve"      0
                    SF-VECTORS    "Bottom Curve"   0
                    SF-ADJUSTMENT "Distortion Compensation" (list 0 -1 1 0.01 0.1 2 SF-SLIDER)
                    SF-OPTION     "Quality" (list "Default (fast)" "Better" "Best (slow)")
                    SF-TOGGLE     "Create New Layer" FALSE
                    SF-OPTION     "After Bending"  (list "Do Nothing" "Enlarge Canvas to Fit")
)