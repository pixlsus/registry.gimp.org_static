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

(define (drawing-diamond-shapes image x1 y1 x2 y2 x3 y3 aliasYN)

	(let* (
		(dArray (make-vector 10 0.0))
		)

; Get rid of any other selections
		(gimp-selection-none image)

; Choose the coordinates for the diamond
		(vector-set! dArray 0 x1)
		(vector-set! dArray 1 y2)
		(vector-set! dArray 2 x2)
		(vector-set! dArray 3 y3)
		(vector-set! dArray 4 x3)
		(vector-set! dArray 5 y2)
		(vector-set! dArray 6 x2)
		(vector-set! dArray 7 y1)
		(vector-set! dArray 8 x1)
		(vector-set! dArray 9 y2)

; Select these points using free-select
		(gimp-free-select image 10 dArray 0 aliasYN FALSE 0)
	)
)
;;  ***************************************************************************
(define (set-diamond-color isRand constColor xFlag yFlag Color1 Color2 Layer)

; Pick the color that you're going to paint the diamond, and paint it

	(if (= isRand TRUE)

; Completely random?
		(gimp-context-set-foreground (list (rand 256) (rand 256) (rand 256)))

; Constant colors along the rows?
		(if (= constColor 0)
			(if (= (fmod yFlag 2) 0)
				(gimp-context-set-foreground Color1)	; Color 1 for even rows
				(gimp-context-set-foreground Color2)	; Color 2 for odd rows
			)

; Constant color along columns
			(if (= (fmod xFlag 2) 0)
				(gimp-context-set-foreground Color1)	; Color 1 for even columns
				(gimp-context-set-foreground Color2)	; Color 2 for even columns
			)
		)
	)

; Fill the selection with the selected color
	(gimp-edit-bucket-fill Layer FG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)

)
;;  ***************************************************************************

(define (script-fu-DiamondGrid
	dHeight		; Height of one set of diamonds
	dWidth		; Width of one set of diamonds
	dHeightInn	; Height of second set of diamonds
	dWidthInn	; Width of second set of diamonds
	gType		; Grid type: Rectangular (gType=0) or Hexagonal (gType=1)
	vGap		; Vertical gap between diamonds
	hGap		; Horizontal gap between diamonds
	vShapes		; Number of shapes per column
	hShapes		; Number of shapes per row
	vBorder		; Border at top and bottom of image
	hBorder		; Border on left and right sides of image
	outerColor1 ; Outer color of one set of diamonds
	innerColor1 ; Inner color of one set of diamonds
	outerColor2 ; Outer color of second set of diamonds
	innerColor2 ; Inner color of second set of diamonds
	constColor	; Colors constant along rows or columns?
	randColor	; Completely random colors?
	bgColor		; Background color for image
	aliased		; Are the diamonds anti-aliasied?
	flatYN		; Is the image layered, or flat?
	seed		; Random seed for making random colors
	)

	(let* (

; Total vertical and horizontal borders for the image
		(tvBorder (* 2.0 vBorder))
		(thBorder (* 2.0 hBorder))

; Half of the dimensions for each of the diamond shapes
		(halfH (/ dHeight 2.0))
		(halfW (/ dWidth 2.0))
		(halfHInn (/ dHeightInn 2.0))
		(halfWInn (/ dWidthInn 2.0))

; Biggest sides in Height and Width
		(maxH (max dHeight dHeightInn))
		(maxW (max dWidth dWidthInn))
;		(minH (min dHeight dHeightInn))
;		(minW (min dWidth dWidthInn))
		(maxhalfH (max halfH halfHInn))
		(maxhalfW (max halfW halfWInn))

; Vertical and horizontal dimensions of diamonds plus gap
		(vGapTot (+ maxH vGap))
		(hGapTot (+ maxW hGap))

; For hexagonal grid purposes
		(add2row (/ vGapTot 2.0))

; Loop control variables
		(xFlag 0)
		(yFlag 0)
		(rowCheck 0)

; The image as well as layers in it.
		(theImage 0)
		(inWidth 0)
		(inHeight 0)
		(baseLayer 0)
		(innerLayer 0)
		(outerLayer 0)

; Coordinates for calculating the diamond locations
		(xCenter 0.0)
		(yCenter 0.0)
		(x1 0.0)
		(x2 0.0)
		(x3 0.0)
		(x4 0.0)
		(x5 0.0)
		(y1 0.0)
		(y2 0.0)
		(y3 0.0)
		(y4 0.0)
		(y5 0.0)

; Generic string for renaming layer, if needed
		(frameName "")

		)

; Set the image dimensions based on which type of grid is being used
		(cond
			((= gType 0)  ; Rectangular grid
				(begin
					(set! inWidth (+ thBorder maxW (* (- hShapes 1) hGapTot)))
					(set! inHeight (+ tvBorder maxH (* (- vShapes 1) vGapTot)))
				)
			)
			((= gType 1)  ; Hexagonal Grid
				(begin
					(set! inWidth (+ thBorder maxW (* (- hShapes 1) (+ hGap maxhalfW))))
					(set! inHeight (+ tvBorder maxH add2row (* (- vShapes 1) vGapTot)))
				)
			)
		)
		
; Create the image and the three layers needed to draw the diamonds
		(set! theImage (car (gimp-image-new inWidth inHeight RGB)))
		(set! baseLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Background Layer" 100 NORMAL-MODE)))
		(set! innerLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Inner Shapes" 100 NORMAL-MODE)))
		(set! outerLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Outer Shapes" 100 NORMAL-MODE)))

; Add the layer to the image
		(gimp-image-add-layer theImage baseLayer 0)
		(gimp-image-add-layer theImage outerLayer 0)
		(gimp-image-add-layer theImage innerLayer 0)

; If the second set of diamond is small in both dimensions than the first, change the layer order
		(if (> dWidthInn dWidth)
			(if (> dHeightInn dHeight)
				(gimp-image-raise-layer-to-top theImage outerLayer)
			)
		)

; Get the random seed
		(set! seed (if (number? seed) seed (realtime)))
		(srand seed)

; Fill up the image with the background color
		(gimp-context-set-foreground '(0 0 0))
		(gimp-context-set-background bgColor)
		(gimp-drawable-fill baseLayer BACKGROUND-FILL)

; Do the shapes in the X direction
		(while (< xFlag hShapes)
			(begin

; Set the center of the diamond in the x-direction for this row, depending on the grid type
				(if (= gType 0)
					(set! xCenter (+ hBorder maxhalfW (* xFlag hGapTot)))
					(set! xCenter (+ hBorder maxhalfW (* xFlag (+ maxhalfW hGap))))
				)

; Set the x-coordinates for the inner and outer diamonds
; x1, x2, x3 are for the outer one; x4, x2, x5 are the inner ones
				(set! x1 (- xCenter halfW))
				(set! x2 xCenter)
				(set! x3 (+ xCenter halfW))
				(set! x4 (-  xCenter halfWInn))
				(set! x5 (+  xCenter halfWInn))

; If we're in a hex grid, we may need to add add2row to the Y-coordinates
				(set! rowCheck (* add2row (fmod xFlag 2) gType))

; Do the shapes in the Y direction
				(while (< yFlag vShapes)
					(begin

; Set the center of the diamond in the y-direction for this diamond
						(set! yCenter (+ vBorder maxhalfH (* yFlag vGapTot) rowCheck))

; Set the Y-coordinates for the inner and outer diamonds
; y1, y2, y3 are for the outer one; y4, y2, y5 are the inner ones

						(set! y1 (- yCenter halfH))
						(set! y2 yCenter)
						(set! y3 (+ yCenter halfH))
						(set! y4 (-  yCenter halfHInn))
						(set! y5 (+  yCenter halfHInn))

; Draw the outer diamond
						(drawing-diamond-shapes theImage x1 y1 x2 y2 x3 y3 aliased)
						(set-diamond-color randColor constColor xFlag yFlag outerColor1 outerColor2 outerLayer)

; Draw the inner diamond
						(drawing-diamond-shapes theImage x4 y4 x2 y2 x5 y5 aliased)
						(set-diamond-color randColor constColor xFlag yFlag innerColor1 innerColor2 innerLayer)

; Increment the yFlag variable
						(set! yFlag (+ 1 yFlag))
					)
				)

; Reset the yFlag and increment the xFlag variable
				(set! yFlag 0)
				(set! xFlag (+ 1 xFlag))
			)
		)

; if the grid is random colored, be sure to make note of the seed used to generate it.
		(if (= randColor TRUE)
			(begin
				(set! frameName (string-append "Diamond Grid " (number->string seed)))
				(gimp-drawable-set-name baseLayer frameName)
			)
		)

; Flatten the image, if requested
		(if (= flatYN TRUE)
			(gimp-image-flatten theImage)
		)

; Get rid of the selection, and then show the final image.
		(gimp-selection-none theImage)
		(gimp-display-new theImage)
	)
)

(script-fu-register
	"script-fu-DiamondGrid"
	_"_Grid - Diamonds..."
	_"Creates a grid of X by Y diamonds, either in rectangular or hexagonal packing."
	"James Sambrook"
	"22 April 2011"
	"James Sambrook.  King George, VA, USA"
	""
	SF-ADJUSTMENT _"Outer Diamond Height"		'(60 10 200 1 5 0 0)
	SF-ADJUSTMENT _"Outer Diamond Width"		'(20 10 200 1 5 0 0)
	SF-ADJUSTMENT _"Inner Diamond Height"		'(30 10 200 1 5 0 0)
	SF-ADJUSTMENT _"Inner Diamond Width"		'(50 10 200 1 5 0 0)
	SF-OPTION     _"Grid Type"					'(_"Rectangular"
												  _"Hexagonal")
	SF-ADJUSTMENT _"Vertical Gap Between Diamonds"		'(15 0 100 1 5 0 0)
	SF-ADJUSTMENT _"Horizontal Gap Between Diamonds"	'(15 0 100 1 5 0 0)
	SF-ADJUSTMENT _"Diamonds in Vertical Dimension" 	'(12 1 100 1 5 0 0)
	SF-ADJUSTMENT _"Diamonds in Horizontal Dimension" 	'(12 1 100 1 5 0 0)
	SF-ADJUSTMENT _"Border at top and bottom of image"	'(15 0 100 1 5 0 0)
	SF-ADJUSTMENT _"Border on left and right of image"	'(15 0 100 1 5 0 0)
	SF-COLOR      _"Outer Diamond color #1"		'(0 0 0)
	SF-COLOR      _"Inner Diamond color #1"		'(80 80 80)
	SF-COLOR      _"Outer Diamond color #2"		'(160 160 160)
	SF-COLOR      _"Inner Diamond color #2"		'(240 240 240)
	SF-OPTION     _"Constant colors along rows or columns?"		'(_"Rows"
																  _"Columns")
	SF-TOGGLE     _"Completely random colors?"	FALSE
	SF-COLOR      _"Background Color"			'(255 255 255)
	SF-TOGGLE     _"Anti-aliasing?"				FALSE
	SF-TOGGLE     _"Flatten Image?"	FALSE
	SF-VALUE      "Random Seed"		"random"
)

(script-fu-menu-register "script-fu-DiamondGrid"
	"<Image>/Filters/SambrookJM/Grid/")