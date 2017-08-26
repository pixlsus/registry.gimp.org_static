;;  ***************************************************************************
;;  *   Copyright (C) 2011 by James Sambrook                                  *
;;  *   sambrook@va.metrocast.net                                             *
;;  *                                                                         *
;;  *   This program is free software; you can redistribute it and/or modify  *
;;  *   it under the terms of the GNU General Public License as published by  *
;;  *   the Free Software Foundation; either version 2 of the License, or     *
;;  *   (at your option) any later version.                                   *
;;  *                                                                         *
;;  *   This program is distributed in the hope that it will be useful,       *
;;  *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
;;  *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
;;  *   GNU General Public License for more details.                          *
;;  *                                                                         *
;;  *   You should have received a copy of the GNU General Public License     *
;;  *   along with this program; if not, write to the                         *
;;  *   Free Software Foundation, Inc.,                                       *
;;  *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
;;  ***************************************************************************

(define (drawing-hex-shapes image x1 y1 x2 y2 x3 y3 x4 aliasYN)

	(let* (
		(hexArray (make-vector 14 0.0))
		)

		(gimp-selection-none image)
		(vector-set! hexArray 0 x1)
		(vector-set! hexArray 1 y1)
		(vector-set! hexArray 2 x2)
		(vector-set! hexArray 3 y2)
		(vector-set! hexArray 4 x3)
		(vector-set! hexArray 5 y2)
		(vector-set! hexArray 6 x4)
		(vector-set! hexArray 7 y1)
		(vector-set! hexArray 8 x3)
		(vector-set! hexArray 9 y3)
		(vector-set! hexArray 10 x2)
		(vector-set! hexArray 11 y3)
		(vector-set! hexArray 12 x1)
		(vector-set! hexArray 13 y1)
		(gimp-free-select image 14 hexArray 0 aliasYN FALSE 0)
	)
)
; ------------------------------------------------------------------------------
(define (drawing-oct-shapes image x1 y1 x2 y2 x3 y3 x4 y4 aliasYN)

	(let* (
		(octArray (make-vector 18 0.0))
		)

		(gimp-selection-none image)
		(vector-set! octArray 0 x1)
		(vector-set! octArray 1 y2)
		(vector-set! octArray 2 x2)
		(vector-set! octArray 3 y1)
		(vector-set! octArray 4 x3)
		(vector-set! octArray 5 y1)
		(vector-set! octArray 6 x4)
		(vector-set! octArray 7 y2)
		(vector-set! octArray 8 x4)
		(vector-set! octArray 9 y3)
		(vector-set! octArray 10 x3)
		(vector-set! octArray 11 y4)
		(vector-set! octArray 12 x2)
		(vector-set! octArray 13 y4)
		(vector-set! octArray 14 x1)
		(vector-set! octArray 15 y3)
		(vector-set! octArray 16 x1)
		(vector-set! octArray 17 y2)
		(gimp-free-select image 18 octArray 0 aliasYN FALSE 0)
	)
)
; ------------------------------------------------------------------------------
(define (script-fu-CircleGrid2
	shape		; Circle (shape=0)
				; Square (shape=1)
				; Hexagon (shape=2)
				; Octagon (shape=3)
	gType		; Grid type (0=Rectangular, 1=Hexagonal)
	bigDiam		; Outer diameter for circles
	smallDiam	; Inner diameter for circles
	xCirc		; Number of circles in the X direction
	yCirc		; Number of circles in the Y direction
	oBorder		; Outer border around image
	gapSpace	; Space between the circles
	outerColor1	; Outer color for the circles in the first row/column
	innerColor1	; Inner color for the circles in the first row/column
	outerColor2	; Outer color for the circles in the second row/column
	innerColor2	; Inner color for the circles in the second row/column
	constColor	; Constant colors along the rows or columns (0=Rows, 1=Columns)
	randColor	; Completely random colors for the circles?
	bgColor		; Background color for entire image
	aliasing	; Anti-aliasing for the circles?
	layered		; Separate layers for the inner and outer circles
	)

	(let* (

; Square roots
		(s3 (sqrt 3))
		(s2 (sqrt 2))

; Circle Diameters
		(bigcircDiam (max bigDiam smallDiam))
		(smallcircDiam (min bigDiam smallDiam))
		(shrinkDiff (/ (- bigcircDiam smallcircDiam) 2))

; make calculations a bit easier by defining the radius and total border space
		(circRad (/ bigcircDiam 2.0))
		(smallcircRad (/ smallcircDiam 2.0))
		(tBorder (* 2 oBorder))

		(inWidth 0)
		(inHeight 0)
		(temp 0)
		(xGap 0)
		(yGap 0)

; Add in the image, and the layer (or layers, if the shapes are on different layers)
		(theImage 0)
		(baseLayer 0)
		(innerLayer 0)
		(outerLayer 0)

; Variables for calculating the position of each shape
		(xFlag 0)
		(yFlag 0)
		(xStart 0.0)
		(yStart 0.0)
		(rowCheck 0.0)

		(xCenter 0.0)
		(yCenter 0.0)
		(x1 0.0)
		(x2 0.0)
		(x3 0.0)
		(x4 0.0)
		(y1 0.0)
		(y2 0.0)
		(y3 0.0)
		(y4 0.0)

; Hexagonal coordinates
		(bigHexX (* s3 0.25 circRad))
		(bigHexY (* s3 0.5 circRad))
		(smallHexX (* s3 0.25 smallcircRad))
		(smallHexY (* s3 0.5 smallcircRad))

; Octagonal coordinates		
		(bigoctSide (/ bigcircDiam (+ 1.0 s2)))
		(smalloctSide (/ smallcircDiam (+ 1.0 s2)))

		)

; If the colors are constant along rows, then switch the X and Y flags
		(if (= constColor 0)
			(begin
				(set! temp xCirc)
				(set! xCirc yCirc)
				(set! yCirc temp)
			)
		)

; Determine the dimensions of the image based on the grid type and number of circles
		(cond
			((= gType 0)	; Rectangular Grid
				(set! inWidth (+ tBorder (* bigcircDiam xCirc) (* gapSpace (- xCirc 1))))
				(set! inHeight (+ tBorder (* bigcircDiam yCirc) (* gapSpace (- yCirc 1))))
			)
			((= gType 1)	; Hexagonal Grid
				(cond
					((= shape 0)	; Circles
						(set! xGap (* s3 (+ circRad (* 0.5 gapSpace))))
						(set! inWidth (+ tBorder bigcircDiam (* (- xCirc 1) xGap)))
						(set! yGap (* gapSpace (- yCirc 1)))
						(set! inHeight (+ tBorder circRad yGap (* gapSpace 0.5) (* bigcircDiam yCirc)))
					)
					((= shape 1)	; Rectangles
						(set! xGap (* gapSpace (- xCirc 1)))
						(set! inWidth (+ tBorder (* bigcircDiam xCirc) xGap))
						(set! yGap (* gapSpace (- yCirc 1)))
						(set! inHeight (+ tBorder circRad yGap (* gapSpace 0.5) (* bigcircDiam yCirc)))
					)
					((= shape 2)	; Hexagons
						(set! xGap (* s3 (+ circRad (* 0.5 gapSpace))))
						(set! inWidth (+ tBorder bigcircDiam (* (- xCirc 1) xGap)))
						(set! yGap (* gapSpace (- yCirc 1)))
						(set! inHeight (+ tBorder circRad yGap (* gapSpace 0.5) (* bigcircDiam yCirc)))
					)
					((= shape 3)	; Octagons
						(set! xGap (* s3 (+ circRad (* 0.5 gapSpace))))
						(set! inWidth (+ tBorder bigcircDiam (* (- xCirc 1) xGap)))
						(set! yGap (* gapSpace (- yCirc 1)))
						(set! inHeight (+ tBorder circRad yGap (* gapSpace 0.5) (* bigcircDiam yCirc)))
					)
				)
			)
		)

; Define the image and layer properties based on what was calculated above
		(cond
			((= constColor 0)	; Constant Color in Rows
				(set! theImage (car (gimp-image-new inHeight inWidth RGB)))
				(set! baseLayer (car (gimp-layer-new theImage inHeight inWidth RGBA-IMAGE "Constant Rows" 100 NORMAL-MODE)))
				(set! innerLayer (car (gimp-layer-new theImage inHeight inWidth RGBA-IMAGE "Inner Shapes" 100 NORMAL-MODE)))
				(set! outerLayer (car (gimp-layer-new theImage inHeight inWidth RGBA-IMAGE "Outer Shapes" 100 NORMAL-MODE)))
			)
			((= constColor 1)	; Constant Color in Rows
				(set! theImage (car (gimp-image-new inWidth inHeight RGB)))
				(set! baseLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Constant Columns" 100 NORMAL-MODE)))
				(set! innerLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Inner Shapes" 100 NORMAL-MODE)))
				(set! outerLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Outer Shapes" 100 NORMAL-MODE)))
			)
		)

; Add the base layer, and fill it up with the background color
		(gimp-image-add-layer theImage baseLayer 0)
		(gimp-context-set-background bgColor)
		(gimp-drawable-fill baseLayer BACKGROUND-FILL)

; Add any extra layers, if needed
		(if (= layered TRUE)
			(begin
				(gimp-image-add-layer theImage outerLayer 0)
				(if (> shrinkDiff 0)
					(gimp-image-add-layer theImage innerLayer 0)
				)
			)
		)
		
; Start drawing circles, and figure out what the X-coordinate is for this row
		(while (< xFlag xCirc)
			(cond
				((= gType 0)
					(set! xStart (+ oBorder (* xFlag (+ bigcircDiam gapSpace))))
				)
				((= gType 1)
					(if (= (fmod xFlag 2) 1)
						(set! rowCheck (+ (* gapSpace 0.5) circRad))
					)
					(cond
						((= shape 0)
							(set! xStart (+ oBorder (* s3 (+ circRad (* 0.5 gapSpace)) xFlag)))
						)
						((= shape 1)
							(set! xStart (+ oBorder (* bigcircDiam xFlag) (* xFlag gapSpace)))
						)
						((= shape 2)
							(set! xStart (+ oBorder (* s3 (+ circRad (* 0.5 gapSpace)) xFlag)))
						)
						((= shape 3)
							(set! xStart (+ oBorder (* s3 (+ circRad (* 0.5 gapSpace)) xFlag)))
						)

					)
				)
			)

; Determine which set of colors is asked for in this row/column of circles.
			(if (= (fmod xFlag 2) 1)
				(begin
					(gimp-context-set-foreground outerColor1)
					(gimp-context-set-background innerColor1)
				)
				(begin
					(gimp-context-set-foreground outerColor2)
					(gimp-context-set-background innerColor2)
				)
			)
			
; Now start doing the columns
			(while (< yFlag yCirc)
				(set! yStart (+ rowCheck oBorder (* gapSpace yFlag) (* bigcircDiam yFlag)))

; For completely random colors, set the foreground and background colors to random values
				(if (= randColor TRUE)
					(begin
						(gimp-context-set-foreground (list (rand 256) (rand 256) (rand 256)))
						(gimp-context-set-background (list (rand 256) (rand 256) (rand 256)))
					)
				)

; If constant color is in rows, then flip the X and Y coordinates to draw the circle
				(cond
					((= shape 0)
						(if (= constColor 0)
							(gimp-ellipse-select theImage yStart xStart bigcircDiam bigcircDiam CHANNEL-OP-REPLACE aliasing FALSE 0)
							(gimp-ellipse-select theImage xStart yStart bigcircDiam bigcircDiam CHANNEL-OP-REPLACE aliasing FALSE 0)
						)
					)
					((= shape 1)
						(if (= constColor 0)
							(gimp-rect-select theImage yStart xStart bigcircDiam bigcircDiam CHANNEL-OP-REPLACE FALSE 0)
							(gimp-rect-select theImage xStart yStart bigcircDiam bigcircDiam CHANNEL-OP-REPLACE FALSE 0)
						)
					)
					((= shape 2)
						(if (= constColor 0)
							(begin
								(set! xCenter (+ yStart circRad))
								(set! yCenter (+ xStart circRad))
							)
							(begin
								(set! xCenter (+ xStart circRad))
								(set! yCenter (+ yStart circRad))
							)
						)
						(set! x1 (- xCenter circRad))
						(set! x2 (- xCenter bigHexX))
						(set! x3 (+ xCenter bigHexX))
						(set! x4 (+ xCenter circRad))
						(set! y1 yCenter)
						(set! y2 (- yCenter bigHexY))
						(set! y3 (+ yCenter bigHexY))
						(drawing-hex-shapes theImage x1 y1 x2 y2 x3 y3 x4 aliasing)
					)
					((= shape 3)
						(if (= constColor 0)
							(begin
								(set! xCenter (+ yStart circRad))
								(set! yCenter (+ xStart circRad))
							)
							(begin
								(set! xCenter (+ xStart circRad))
								(set! yCenter (+ yStart circRad))
							)
						)
						(set! x1 (- xCenter circRad))
						(set! x2 (- xCenter (/ bigoctSide 2.0)))
						(set! x3 (+ xCenter (/ bigoctSide 2.0)))
						(set! x4 (+ xCenter circRad))
						(set! y1 (- yCenter circRad))
						(set! y2 (- yCenter (/ bigoctSide 2.0)))
						(set! y3 (+ yCenter (/ bigoctSide 2.0)))
						(set! y4 (+ yCenter circRad))
						(drawing-oct-shapes theImage x1 y1 x2 y2 x3 y3 x4 y4 aliasing)
					)
				)

; Fill the selection, and then fill the inner circle with the proper color
				(if (= layered TRUE)
					(gimp-edit-bucket-fill outerLayer FG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
					(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
				)

				(if (> shrinkDiff 0)
					(begin
						(cond
							((= shape 0)
								(gimp-selection-shrink theImage shrinkDiff)
							)
							((= shape 1)
								(gimp-selection-shrink theImage shrinkDiff)
							)
							((= shape 2)
								(begin
									(set! x1 (- xCenter smallcircRad))
									(set! x2 (- xCenter smallHexX))
									(set! x3 (+ xCenter smallHexX))
									(set! x4 (+ xCenter smallcircRad))
									(set! y1 yCenter)
									(set! y2 (- yCenter smallHexY))
									(set! y3 (+ yCenter smallHexY))
									(drawing-hex-shapes theImage x1 y1 x2 y2 x3 y3 x4 aliasing)
								)
							)

							((= shape 3)
								(if (= constColor 0)
									(begin
										(set! xCenter (+ yStart circRad))
										(set! yCenter (+ xStart circRad))
									)
									(begin
										(set! xCenter (+ xStart circRad))
										(set! yCenter (+ yStart circRad))
									)
								)
								(set! x1 (- xCenter smallcircRad))
								(set! x2 (- xCenter (/ smalloctSide 2.0)))
								(set! x3 (+ xCenter (/ smalloctSide 2.0)))
								(set! x4 (+ xCenter smallcircRad))
								(set! y1 (- yCenter smallcircRad))
								(set! y2 (- yCenter (/ smalloctSide 2.0)))
								(set! y3 (+ yCenter (/ smalloctSide 2.0)))
								(set! y4 (+ yCenter smallcircRad))
								(drawing-oct-shapes theImage x1 y1 x2 y2 x3 y3 x4 y4 aliasing)
							)
						)

						(if (= layered TRUE)
							(gimp-edit-bucket-fill innerLayer BG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
							(gimp-edit-bucket-fill baseLayer BG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
						)
					)
				)

; Move on to the next circle
				(set! yFlag (+ 1 yFlag))
			)

; Move on to the next row, and reset the other counting flags
			(set! xFlag (+ xFlag 1))
			(set! yFlag 0)
			(set! rowCheck 0.0)
		)

; Get rid of the selection, and then show the final image.
		(gimp-selection-none theImage)
		(gimp-display-new theImage)
	)
)

(script-fu-register
	"script-fu-CircleGrid2"
	_"_Grid - Two-toned shapes..."
	_"Creates a grid of X by Y two-toned shapes, either in rectangular or hexagonal packing."
	"James Sambrook"
	"22 April 2011"
	"James Sambrook.  King George, VA, USA"
	""
	SF-OPTION     _"Shape"						'(_"Circle"
												  _"Square"
												  _"Hexagon"
												  _"Octagon")
	SF-OPTION     _"Grid Type"					'(_"Rectangular"
												  _"Hexagonal")
	SF-ADJUSTMENT _"Outer Shape Diameter"		'(40 10 200 1 5 0 0)
	SF-ADJUSTMENT _"Inner Shape Diameter"		'(30 10 200 1 5 0 0)
	SF-ADJUSTMENT _"Shapes in X Direction" 		'(10 1 100 1 5 0 0)
	SF-ADJUSTMENT _"Shapes in Y Direction" 		'(11 1 100 1 5 0 0)
	SF-ADJUSTMENT _"Outer Border Around Shapes"	'(12 1 100 1 5 0 0)
	SF-ADJUSTMENT _"Gap Between the Shapes"		'(13 1 100 1 5 0 0)
	SF-COLOR      _"Outer shape color #1"		'(0 0 0)
	SF-COLOR      _"Inner shape color #1"		'(127 127 127)
	SF-COLOR      _"Outer shape color #2"		'(127 127 127)
	SF-COLOR      _"Inner shape color #2"		'(0 0 0)
	SF-OPTION     _"Constant colors along rows or columns?"		'(_"Rows"
																  _"Columns")
	SF-TOGGLE     _"Completely random colors?"	FALSE
	SF-COLOR      _"Background Color"			'(255 255 255)
	SF-TOGGLE     _"Anti-aliasing?"				FALSE
	SF-TOGGLE     _"Inner and outer shapes on different layers?"	TRUE
)

(script-fu-menu-register "script-fu-CircleGrid2"
	"<Image>/Filters/SambrookJM/Grid/")