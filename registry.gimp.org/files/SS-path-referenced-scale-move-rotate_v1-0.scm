; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version. http://www.gnu.org/copyleft/gpl.html
;  
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;  



(define (script-fu-move-by-path img layer)

; Functional Requirement Specification
; The current layer must be moved a specified distance. A two-anchor path describes 
; how far it is to be moved. 
; The first anchor point marks the active layer's reference point.
; The second anchor point marks the final resting place of the reference point.
; the active layer will move to put the first anchor point on top of the second anchor point.
; This is useful for aligning two layers prior to rotating the active layer when doing
; HDR for example, or for when creating collages.
; For aligning two similar layers, start a path on the active layer's reference point
; Set opacity for the layer down to about 30%
; Mark the same point in the master layer with the second anchor of the path, then run
; this script.
; Often, align is closely followed by rotate. This script will prepare for the rotate
; by setting up the path for the script 'rotate-by-path' 
; A new path will be created with three anchors in an L-shape.
; The first anchor point will be placed at the top of the L-shape, and this point must be moved to the 
; position on the active layer that must be moved.
; The second anchor point is the same as the reference point we moved the layer to. It joins
; the two line segments in the corner of the L-shape. This point MUST NOT be moved.
; The third anchor is located to the right of the reference point. This must be placed on the
; point that the active layer will be rotated to. See SS-rotate-to-path for details.
 

		

	(let* (
	
		; defining all the variables we'll need in the procedure
		(theX0 0)
		(theX1 0)
		(theY0 0)
		(theY1 0)
		(theVectorID)
		(theStrokeID)
		(numStrokes)
		(thePointsList)
		(numPoints)
		(theControlPoints)
		
		)		; end of variable definitions

		(gimp-image-undo-group-start img)	; Always good practice to undo all the steps of a script at once
		
		; get-active-vectors routine returns a singleton list (a cell) containing the ID of the currently active path.
		; if no path is currently active, the return value is -1
		(set! theVectorID (car (gimp-image-get-active-vectors img))) 		
		
		(if (> theVectorID 0)
			(begin
					; Using the referenced vectorID, we must discover the ID of the 'stroke'. 
					; A stroke is a single continuous vectors. The stroke has several 'points' called anchors, 
					; each anchor describing a single locus on the path describing the bezier curve. 
					; Example Results: Image 1; Vector 3; Stroke 1
					; PointsList: (0 18 #(156.0 356.0 156.0 356.0 156.0 356.0 574.0 171.0 574.0 171.0 574.0 171.0 
					;					406.0 358.0 406.0 358.0 406.0 358.0) 0)
					; PointsList breakdown:
					; (0 	- Always 0 at this time. Defines a BEZIER stroke, the only kind gimp uses.
					; 18 	- the total number of points defining the stroke. 
					;			Each point comprises six values, so this stroke has 18/6=3 points
					; #(	- A vector, a scheme language construct. Not to be confused with a gimp 'vectors' object
					; 156.0 356.0 156.0 356.0 156.0 356.0 - describes the bezier control points and position of the first anchor. 
					; First two are coordinates of the first grab handle.
					; Second two are the coordinates of the anchor on the path.
					; Third two are the coordinates of the second grab handle.
					; Three anchors in this example, so three sets of points.
					; ) 0) - Final item on its own defines whether the stroke is "Closed" or not. 0=Open, 1=Closed.
									; '(0 6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; car list = 0
									; cdr list = '(6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; cadr list = 6
									; cddr list = '(#(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; caddr list = #(123.0 123.0 130.0 160.0 90.0 190.0)
									; cdddr list = 0			
			
					; get-strokes returns a list of all the strokes in the path. An example: (3 #(1 2 3))
					; The first value is the number of strokes in the path. The vector contains all the IDs of the strokes.
					; We will only accept a single stroke with two or more points, so 
					; if numStrokes <> 1 then we fail the procedure with a warning.
					; Anything else would be ambiguous. 
					(set! numStrokes (car (gimp-vectors-get-strokes theVectorID)))
					(if (= numStrokes 1)
						(begin
							(set! theStrokeID (vector-ref (cadr (gimp-vectors-get-strokes theVectorID)) 0))
							(set! thePointsList (gimp-vectors-stroke-get-points theVectorID theStrokeID))
							(display (gimp-vectors-stroke-get-points theVectorID theStrokeID))
							(set! numPoints (cadr thePointsList))
							; the number of points are six per anchor in the path. If there are 12 points, 
							; the path is valid for our purposes.
							; If the path does not have 12 points, it is unusable and we simply do nothing except send a warning message.
							(if (= numPoints 12)
								(begin
									(set! theControlPoints (caddr thePointsList))
									; Extract the locations of the two reference points. (X0, Y0) is the active layer's reference point.
									; (X1, Y1) is the reference layer's reference point. The active layer will move to (X1, Y1)

									(set! theX0 (vector-ref theControlPoints 2))
									(set! theY0 (vector-ref theControlPoints 3))
									(set! theX1 (vector-ref theControlPoints 8))
									(set! theY1 (vector-ref theControlPoints 9))
									
									; translate (move) the layer
									(gimp-layer-translate layer (- theX1 theX0) (- theY1 theY0))

									; Now, delete the existing path, create two new anchors fixed at two corners 
									; of the image (the image, not the layer!) and set the third new anchor at
									; (X1, Y1), the reference layer's reference point where the two layers should be 
									; in perfect registration.
									(gimp-image-remove-vectors img theVectorID)
									
									(set! theVectorID (car (gimp-vectors-new img "Rotate-Path"))) 

									(set! theStrokeID (car (gimp-vectors-bezier-stroke-new-moveto theVectorID (+ theX1 100) theY1)))

									(gimp-vectors-bezier-stroke-lineto theVectorID theStrokeID theX1 theY1)
									(gimp-vectors-bezier-stroke-lineto theVectorID theStrokeID theX1 (- theY1 100))
									;(gimp-image-add-vectors img theVectorID -1) (deprecated procedure)_
									(gimp-image-insert-vectors img theVectorID 0 -1)									
									(gimp-image-set-active-vectors img theVectorID)

									(gimp-vectors-set-visible theVectorID TRUE)
									;(gimp-item-set-visible theVectorID TRUE)
									; and finally, tell gimp to redraw everything and clean up
									(gimp-displays-flush)


								)
								(begin   ; else clause of (if (= numPoints 12)
									(gimp-message "Move by Path failed: Path must have exactly two points")
								)
							)	

						)
						(begin ; else clause of (if (= numStrokes 1)
							(gimp-message "Move by Path failed: Path requires one single stroke")
						)
					)
			)
				(begin  ; this is 'else' clause for (if (> theVectorID 0)
					(gimp-message "Move by Path failed: No active path selected")
				)
		)		
	   (gimp-image-undo-group-end img) 
	)
)
	


(define (script-fu-rotate-by-path img layer)

; Functional Requirement Specification
; The typical sequence of operations to bring two layers into perfect registration is to
; scale the layers so the subject of both images are the same size, then bring two specific 
; pixels into perfect registration, and then to rotate one of the layers to complete perfect registration.
; This script assumes the layer scales are correct and a pixel in both layers are in perfect registration.
; The user creates a path starting on a pixel in one layer, then on the registered rotation axis pixel, then
; on the corresponding pixel in the other layer. The points will be identified as A, B, and C.
; A and C mark two pixels on two layers that must be brought into registration, and B describes the axis of
; rotation. The angle b made by lines AB and BC is the angle of rotation to apply.  

;--------------------------------------------
	(define 	pi (* 4 (atan 1)))
;--------------------------------------------
	
	(let* (
		
		(xA 0)
		(yA 0)
		(xB 0)				; the anchor points of A, B, and C
		(yB 0)
		(xC 0)
		(yC 0)
		(theAngleAB 0)
		(theAngleBC 0)
		(theRotationAngle 0)   ; the angle between the two lines AB and BC
		(theVectorID)
		(theStrokeID)
		(theStrokePoints) ; An example: (3 #(1 2 3))
		(theStrokePointsCount) ; the number of control points on the stroke. 
		(theControlPoints) ; will be an array containing a list of all the anchors on the path stroke

		)

		(gimp-image-undo-group-start img)	; Always good practice to undo all the steps of a script at once
		
			; There has to be a vectors (a path) object active. That vectors must have exactly one stroke
			; with exactly three points.  

			; Using the referenced vectorID, we can discover the ID of the 'stroke'. 
			; A stroke is a single continuous vectors. The stroke has several 'points' called anchors, 
			; each anchor describing a single locus on the path describing the bezier curve. 

			; get-strokes returns a list of all the strokes in the path. An example: (3 #(1 2 3))
			; The first value is the number of strokes in the path. The vector contains all the IDs of the strokes.
			; We will only accept a single stroke with two or more points, so 
			; if numStrokes <> 1 then we fail the procedure with a warning.
			; Anything else would be ambiguous. 

			; Example Results: Image 1; Vector 3; Stroke 1
			; PointsList: (0 18 #(156.0 356.0 156.0 356.0 156.0 356.0 574.0 171.0 574.0 171.0 574.0 171.0 
			;					406.0 358.0 406.0 358.0 406.0 358.0) 0)
			; PointsList breakdown:
			; (0 	- Always 0 at this time. Defines a BEZIER stroke, the only kind gimp uses.
			; 18 	- the total number of points defining the stroke. 
			;			Each point comprises six values, so this stroke has 18/6=3 points
			; #(	- A vector, a scheme language construct. Not to be confused with a gimp 'vectors' object
			; 156.0 356.0 156.0 356.0 156.0 356.0 - describes the bezier control points and position of the first anchor. 
			; First two are coordinates of the first grab handle.
			; Second two are the coordinates of the anchor on the path.
			; Third two are the coordinates of the second grab handle.
			; Three anchors in this example, so three sets of points.
			; ) 0) - Final item on its own defines whether the stroke is "Closed" or not. 0=Open, 1=Closed.
									; '(0 6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; car list = 0
									; cdr list = '(6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; cadr list = 6
									; cddr list = '(#(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; caddr list = #(123.0 123.0 130.0 160.0 90.0 190.0)
									; cdddr list = 0			
	
	
			; get-strokes returns a list of all the strokes in the path. An example: (3 #(1 2 3))
			; The first value is the number of strokes in the path. The vector contains all the IDs of the strokes.
			; We will only accept a single stroke with two or more points, so 
			; if numStrokes <> 1 then we fail the procedure with a warning.
			; Anything else would be ambiguous. 
			

			(set! theVectorID (car (gimp-image-get-active-vectors img)))
			(if (> theVectorID 0)
				(begin 
					(set! theStrokePointsCount (car (gimp-vectors-get-strokes theVectorID)))
					(if (= theStrokePointsCount 1)
						(begin
							(set! theStrokeID (vector-ref (cadr (gimp-vectors-get-strokes theVectorID)) 0))
							(set! theStrokePoints (gimp-vectors-stroke-get-points theVectorID theStrokeID))
							(set! theStrokePointsCount (cadr theStrokePoints))
							(if (= theStrokePointsCount 18)
								(begin
									(set! theControlPoints (caddr theStrokePoints))
									(set! xA (vector-ref theControlPoints 2))
									(set! yA (vector-ref theControlPoints 3))
									(set! xB (vector-ref theControlPoints 8))
									(set! yB (vector-ref theControlPoints 9))
									(set! xC (vector-ref theControlPoints 14))
									(set! yC (vector-ref theControlPoints 15))
									; with the positions of the three anchors known, we can calculate the angle between them.
									; the equation of a line segment from high-school math is y=mx+c. The slope is m.
									; m = (y1-y0)/(x1-x0). The angle of the slope to the x-axis is arctan m
									; There is a special case - if the line is vertical, m is infinite. The angle is + or -pi/2 radians
									(if (= (- xA xB) 0)
										(begin
											(set! theAngleAB (* (* 2 pi) (/ (- yA yB) (abs (- yA yB)) ))) 
											; if we don't do this, google "you divided by zero" to find out what can happen
											; We also need to know whether we are vertical 'up' or vertical 'down'
											; because that changes the angle by 180 degrees or pi radians.
											; (yA - yB)/(ABS(yA - yB)) tells us which direction the line is in.
										)
										(begin
											(set! theAngleAB (atan (/ (- yA yB)(- xA xB))))
										)
									)
									(if (= (- xC xB) 0)
										(begin
											(set! theAngleAB (* (* 2 pi) (/ (- yC yB) (abs (- yC yB)) ))) 
										)
										(begin
											(set! theAngleBC (atan (/ (- yC yB)(- xC xB))))
										)
									)
									
									(set! theRotationAngle (- theAngleBC theAngleAB))
									
									(gimp-item-transform-rotate layer theRotationAngle FALSE xB yB)									
									(gimp-displays-flush)
	
								)
								(begin
									(gimp-message "Rotate-by-Path failed: The path must have three anchor points")
								)
							)	
						)
						(begin
							(gimp-message "Rotate-by-Path failed: Path must have exactly one stroke")
						)
					)
				)
				(begin
					(gimp-message "Rotate-by-Path failed: No Active Path")
				)
			)
		   (gimp-image-undo-group-end img) 
	)
)

;==========================================



(define (script-fu-ortho-by-path img layer)

; Functional Requirement Specification
	; The current path must have either 2 or three anchors .
	; If there are only two anchors, the current layer is rotated about the layer centre.
	; if there are three anchors, the layer is rotated about the third anchor. More anchors will be ignored.
	; If the path has less than two anchors, the procedure will fail with a warning.
	; in all cases, the first two anchors define the line which must become vertical or horizontal. 
	; The image is assumed to always require less than 45 degrees rotation, so it will rotate 
	; to bring the path and image to the horizontal or vertical, whichever is nearer.

		
;------------------------------------------
	(define PI (* 4 (atan 1)))		
;------------------------------------------
	
	(let* (
	
		; defining all the variables we'll need in the procedure
		(theAngle 0)
		(theCentre-x 0)
		(theCentre-y 0)
		(theX0 0)
		(theX1 0)
		(theY0 0)
		(theY1 0)
		(theVectorID)
		(numStrokes)
		(thePointsList)
		(numPoints)
		(theControlPoints)
		(theSlope)
		
		)		; end of variable definitions

		(gimp-image-undo-group-start img)	; Always good practice to undo all the steps of a script at once
		
		; get-active-vectors routine returns a singleton list containing the ID of the currently active path.
		; if no path is selected, the return value is -1
		(set! theVectorID (car (gimp-image-get-active-vectors img))) 		
		
		(if (> theVectorID 0)
			(begin
					; Using the referenced vectorID, we must discover the ID of the 'stroke'. 
					; A stroke is a single continuous vectors. The stroke has several 'points' called anchors, 
					; each anchor describing a single locus on the path describing the bezier curve. 
					; Example Results: Image 1; Vector 3; Stroke 1
					; PointsList: (0 18 #(156.0 356.0 156.0 356.0 156.0 356.0 574.0 171.0 574.0 171.0 574.0 171.0 
					;					406.0 358.0 406.0 358.0 406.0 358.0) 0)
					; PointsList breakdown:
					; (0 	- Always 0 at this time. Defines a BEZIER stroke, the only kind gimp uses.
					; 18 	- the total number of points defining the stroke. 
					;			Each point comprises six values, so this stroke has 18/6=3 points
					; #(	- A vector, a scheme language construct. Not to be confused with a gimp 'vectors' object
					; 156.0 356.0 156.0 356.0 156.0 356.0 - describes the bezier control points and position of the first anchor. 
					; First two are coordinates of the first grab handle.
					; Second two are the coordinates of the anchor on the path.
					; Third two are the coordinates of the second grab handle.
					; Three anchors in this example, so three sets of points.
					; ) 0) - Final item on its own defines whether the stroke is "Closed" or not. 0=Open, 1=Closed.
									; '(0 6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; car list = 0
									; cdr list = '(6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; cadr list = 6
									; cddr list = '(#(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; caddr list = #(123.0 123.0 130.0 160.0 90.0 190.0)
									; cdddr list = 0			
			
			
					; get-strokes returns a list of all the strokes in the path. An example: (3 #(1 2 3))
					; The first value is the number of strokes in the path. The vector contains all the IDs of the strokes.
					; We will only accept a single stroke with two or more points, so 
					; if numStrokes <> 1 then we fail the procedure with a warning.
					; Anything else would be ambiguous. 
					(set! numStrokes (car (gimp-vectors-get-strokes theVectorID)))
					(if (= numStrokes 1)
						(begin
							(set! thePointsList (gimp-vectors-stroke-get-points theVectorID (vector-ref (cadr (gimp-vectors-get-strokes theVectorID)) 0)))

							(set! numPoints (cadr thePointsList))
							; the number of points are six per anchor in the path. If there are more than 6 points, 
							; the path is valid for our purposes.
							; If the path has only one anchor, it is unusable and we simply do nothing except send a warning message.
							(if (> numPoints 6)
								(begin
									(set! theControlPoints (caddr thePointsList))
									; Now that we have extracted the information sources we need to calculate the angle of rotation
									; and the centre of rotation
									; First, we need the centre of rotation. If a third anchor exists in the path,
									;that will become our centre of rotation.
									; If the path has only two anchors, we assume centre of rotation to be the centre of the image.
									(if (> numPoints 12)
										(begin
											(set! theCentre-x (vector-ref theControlPoints 14))
											(set! theCentre-y (vector-ref theControlPoints 15))
										)
										(begin   ; else clause for (if (> numPoints 12)
											(set! theCentre-x (/ (car (gimp-image-width img)) 2))
											(set! theCentre-y (/ (car (gimp-image-height img)) 2))
										)
									)
									; the angle of rotation comes from high-school math.
									; the slope of a line y=mx+c is m. m=((Y1-Y0)/(X1-X0)) as long as X1-X0 is not 0.
									; the angle of m from the horizontal is given by arctan m
									; m will always be opposite in sign to 'normal' math because the 'origin' is in the
									; top-left corner of the image. The Y-axis is inverted.
									; We assume that the user needs to correct rotation by less than 45 degrees.
									; If m is greater than 1, the angle of the line is more vertical than horizontal.
									; If that is the case, we find the negative reciprocal f(m) = -1/m to normalise it. 
 
									(set! theX0 (vector-ref theControlPoints 2))
									(set! theY0 (vector-ref theControlPoints 3))
									(set! theX1 (vector-ref theControlPoints 8))
									(set! theY1 (vector-ref theControlPoints 9))
									(if (not (= theX0 theX1))
										(begin
											(set! theSlope (/ (- theY1 theY0) (- theX1 theX0)))
											(if (or (> theSlope 1) (< theSlope -1)) 
												(begin
													(set! theSlope (* (/ 1 theSlope) -1))
												)
												(begin
												)
											)
										)
										(begin
											(set! theSlope 0)   ; if the path segment is perfectly vertical, force no rotation. 
										)
									)
									; So now we have the slope of the line and normalised it to produce the smallest rotation
									; consistent with the line segment, we need to give gimp the angle to rotate by.
									; the angle is f(m) = arctan m and the result will be in Radians
									; We also wish to rotate in the opposite direction to the angle of the path.
									; Change the direction by multiplying the angle by -1
									(set! theAngle (* (atan theSlope) -1))
									
									; perform the actual rotation on both the path and the layer
									(gimp-item-transform-rotate layer theAngle FALSE theCentre-x theCentre-y)									
									(gimp-item-transform-rotate theVectorID theAngle FALSE theCentre-x theCentre-y)
									
									; and finally, tell gimp to redraw everything and clean up
									(gimp-displays-flush)


								)
								(begin   ; else clause of (if (> numPoints 6)
									(gimp-message "Ortho by Path failed: Path must have two or more points")
								)
							)	

						)
						(begin ; else clause of (if (= numStrokes 1)
							(gimp-message "Ortho by Path failed: Path requires one single stroke")
						)
					)
			)
				(begin  ; this is 'else' clause for (if (> theVectorID 0)
					(gimp-message "Ortho by Path failed: No active path selected")
				)
		)		
	   (gimp-image-undo-group-end img) 
	)
)
	



(define (script-fu-scale-by-path img layer)


; Two layers need to be brought into registration, but the two layers are not
; scaled correctly. Perhaps the camera zoom was bumped or the layer has had 
; a transformation done to correct for lens distortion... the reason doesn't matter.
; Find two points on each layer that must be spaced exactly the same distance apart
; on both layers. Add a path from one point to the other point on the reference
; layer, and then add a new stroke (shift-click) to make another separate line between
; the same two reference points on the layer to be scaled. A window frame in both images,
; or the edge of a roof in both layers. 
; The ratio of the lengths of the two line segments determines the scale factor to be applied.

; Do not expect perfect results. The effects of scaling alters the image imperceptibly. 
; Try duplicating a layer, then scaling it up and scaling it back to the original size,
; and set the mode to 'difference'. The larger the difference in scaling, the larger the effect.
; This method will bring you extremely close to what you need.



	(gimp-image-undo-group-start img)	; Always good practice to undo all the steps of a script at once
	(gimp-context-push) ; if the script interferes with any context items, save the original context to
								; be able to retrieve it unchanged later.

	(let* 
		(
		(SS-scale-ratio 1)
		(SS-Length-0 0)
		(SS-Length-1 0)
		(theVectorID)
		(theStroke)
		(theStrokeID)
		(theStroke-list)
		(theControlPoints)
		(numStrokes)
		(numPoints)
		(theX0 0)
		(theY0 0)
		(theX1 0)
		(theY1 0)
		
		)
		(set! theVectorID (car (gimp-image-get-active-vectors img))) 
		
		(if (> theVectorID 0)
			(begin
					; Using the referenced vectorID, we must discover the ID of the 'stroke'. 
					; A stroke is a single continuous vectors. The stroke has several 'points' called anchors, 
					; each anchor describing a single locus on the path describing the bezier curve.
					; there can be more than one stroke on a single path, and we exploit that for this task
					; Example Results: Image 1; Vector 3; Stroke 1
					; PointsList: (0 18 #(156.0 356.0 156.0 356.0 156.0 356.0 574.0 171.0 574.0 171.0 574.0 171.0 
					;					406.0 358.0 406.0 358.0 406.0 358.0) 0)
					; PointsList breakdown:
					; (0 	- Always 0 at this time. Defines a BEZIER stroke, the only kind gimp uses.
					; 18 	- the total number of points defining the stroke. 
					;			Each point comprises six values, so this stroke has 18/6=3 points
					; #(	- A vector, a scheme language construct. Not to be confused with a gimp 'vectors' object
					; 156.0 356.0 156.0 356.0 156.0 356.0 - describes the bezier control points and position of the first anchor. 
					; First two are coordinates of the first grab handle.
					; Second two are the coordinates of the anchor on the path.
					; Third two are the coordinates of the second grab handle.
					; Three anchors in this example, so three sets of points.
					; ) 0) - Final item on its own defines whether the stroke is "Closed" or not. 0=Open, 1=Closed.
									; '(0 6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; car list = 0
									; cdr list = '(6 #(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; cadr list = 6
									; cddr list = '(#(123.0 123.0 130.0 160.0 90.0 190.0) 0)
									; caddr list = #(123.0 123.0 130.0 160.0 90.0 190.0)
									; cdddr list = 0			
			
					; get-strokes returns a list of all the strokes in the path. An example: (3 #(1 2 3))
					; The first value is the number of strokes in the path. The vector contains all the IDs of the strokes.
					; We will only accept two strokes, each with two points, so 
					; if numStrokes <> 2 then we fail the procedure with a warning.
					; Anything else would be ambiguous. 
					(set! numStrokes (car (gimp-vectors-get-strokes theVectorID)))
					(if (= numStrokes 2)
						(begin
							(set! theStrokeID (vector-ref (cadr (gimp-vectors-get-strokes theVectorID)) 0))
							(set! theStroke-list (gimp-vectors-stroke-get-points theVectorID theStrokeID))
							(display theStroke-list)
							(set! numPoints (cadr theStroke-list))

							; the number of points are six per anchor in the path. If there are 12 points, 
							; the stroke is valid for our purposes.
							; If the stroke does not have 12 points, it is unusable and we simply do nothing except send a warning message.

							(if (= numPoints 12)
								(begin
									(set! theControlPoints (caddr theStroke-list))

									; Extract the locations of the two reference points. (X0, Y0) (X1, Y1) in the 
									; reference layer's reference stroke.
									; The length of the line is sqrt (sqr(x1-x0) + sqr(y1-y0)) (Thanks, Pythagorus!)

										(set! theX0 (vector-ref theControlPoints 2))
										(set! theY0 (vector-ref theControlPoints 3))
										(set! theX1 (vector-ref theControlPoints 8))
										(set! theY1 (vector-ref theControlPoints 9))

										(set! SS-Length-0 (sqrt (+ (expt(- theX1 theX0) 2) (expt(- theY1 theY0) 2))))
								)
								(begin
									(gimp-message "Scale-by-Path failed: Each stroke on the path must have exactly two points")
								)
							)
							(set! theStrokeID (vector-ref (cadr (gimp-vectors-get-strokes theVectorID)) 1))
							(set! theStroke-list (gimp-vectors-stroke-get-points theVectorID theStrokeID))
							(display theStroke-list)
							(set! numPoints (cadr theStroke-list))

							; the number of points are six per anchor in the path. If there are 12 points, 
							; the stroke is valid for our purposes.
							; If the stroke does not have 12 points, it is unusable and we simply do nothing except send a warning message.

							(if (= numPoints 12)
								(begin
									(set! theControlPoints (caddr theStroke-list))

										; Extract the locations of the two reference points. (X0, Y0) (X1, Y1) in the 
										; target layer's reference stroke.
										; The length of the line is sqrt (sqr(x1-x0) + sqr(y1-y0)) (Thanks, Pythagorus!)

										(set! theX0 (vector-ref theControlPoints 2))
										(set! theY0 (vector-ref theControlPoints 3))
										(set! theX1 (vector-ref theControlPoints 8))
										(set! theY1 (vector-ref theControlPoints 9))
										
										(set! SS-Length-1 (sqrt (+ (expt(- theX1 theX0) 2) (expt(- theY1 theY0) 2))))
								)
								(begin
									(gimp-message "Scale-by-Path failed: Each stroke on the path must have exactly two points")
								)
							)
						)
						(begin
							(gimp-message "Scale-by-Path failed: There has to be two separate strokes with two points each._
												Use shift-click to start a second stroke")
						)
					)
					
					; We have two lines, the ratio of the lengths are to be the ratio to scale the layer by.

					(if (> SS-Length-1 0)   ; Why? Google "You divided by zero" to see why!
						(begin		
							(set! SS-scale-ratio (/ SS-Length-0 SS-Length-1))
						)
						(begin
							(gimp-message "One of the strokes has zero length!")
							(set! SS-scale-ratio 1)
						)
					)
					
					(gimp-drawable-width layer)
					(gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST) 
					; { TRANSFORM-RESIZE-ADJUST (0), TRANSFORM-RESIZE-CLIP (1), TRANSFORM-RESIZE-CROP (2), TRANSFORM-RESIZE-CROP-WITH-ASPECT (3) }
					
					(gimp-item-transform-scale layer 0 0 (* (car (gimp-drawable-width layer)) SS-scale-ratio) (* (car (gimp-drawable-height layer)) SS-scale-ratio))				
			
			)
			(begin
				(gimp-message "Scale-by-Path failed: There must be a path selected")
			)
		)

			
	(gimp-displays-flush)
	(gimp-image-undo-group-end img) 		
	(gimp-context-pop)
	)
)

;==========================================

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version. http://www.gnu.org/copyleft/gpl.html
;  
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;  



(script-fu-register 
			"script-fu-move-by-path"
			"<Image>/Layer/Transform/With Path.../Move by Path"
			"Move a layer by reference with a path. First anchor defines the active layer's reference point. Second anchor defines the point to move to. Only two anchors are allowed. A new 3-anchor path will be created ready for 'Rotate by Path'. The horizontal anchor must be moved onto the reference line, and the vertical anchor defines the point on the active layer that must be rotated onto the reference line. The corner anchor marks the centre of rotation and should not be moved."
			"savvysaffer.deviantart.com"
			"Copyright Mike Ochtman"
			"Ver 1.0 Aug 2012"
			"*"
			SF-IMAGE "Image" 0
			SF-DRAWABLE "Layer" 0
)




(script-fu-register 
			"script-fu-rotate-by-path"
			"<Image>/Layer/Transform/With Path.../Rotate by Path"
			"A three-point path with three anchor points describe the amount of rotation to apply to an image. The first point is the reference point, the second is the centre of rotation and the third is the point on the active layer that must be rotated into position. If Move-by-Path was used, the top anchor is the reference point and the rightmost anchor shows the point on the active layer that must be rotated to bring it into registration."
			"SavvySaffer.deviantart.com"
			"Copyright Mike Ochtman"
			"Ver 1.0 Aug 2012"
			"*" 
			SF-IMAGE "Image" 0
			SF-DRAWABLE "Layer" 0
)

(script-fu-register 
			"script-fu-ortho-by-path"
			"<Image>/Layer/Transform/With Path.../Ortho by Path"
			"Rotate image to orthagonal with path. First two anchors on the path mark a reference line on the active layer which must become vertical or horizontal, whichever requires the least rotation. The optional third anchor defines the centre of rotation. If no third anchor is present, the layer is rotated about the centre of the layer."
			"SavvySaffer.deviantart.com"
			"Copyright Mike Ochtman"
			"Ver 1.0 Aug 2012"
			"*"
			SF-IMAGE "Image" 0
			SF-DRAWABLE "Layer" 0
)


(script-fu-register 
			"script-fu-scale-by-path"
			"<Image>/Layer/Transform/With Path.../Scale by Path"
			"Description of function."
			"savvysaffer.deviantart.com"
			"Copyright Mike Ochtman"
			"Ver 0.1 Aug 2012"
			"*" ; image type "", "*", "RGB"...
			SF-IMAGE "Image" 0
			SF-DRAWABLE "Layer" 0
)



; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version. http://www.gnu.org/copyleft/gpl.html
;  
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;  

