(define (script-fu-HexorTri
	Shape		 ; Hexagon (Shape=0) or Triangle (Shape=1)
	Width		 ; Width of image (subject to change if a tiled image is selected)
	Height		 ; Height of image (subject to change if a tiled image is selected)
	hexSize		 ; Length of one side
	Brush_Rad	 ; Radius of the brush
	hexColor	 ; Color of the lines that create the shapes
	tileAR		 ; Set aspect ratio of the image to tile the shapes?
	coloredHex	 ; Are colored shapes requested?
	inColor1	 ; First random color
	inColorPerc1 ; How often should this color show up?
	inColor2	 ; Second random color
	inColorPerc2 ; How often should this color show up?
	inColor3	 ; Third random color
	inColorPerc3 ; How often should this color show up?
	inColor4	 ; Fourth random color
	inColorPerc4 ; How often should this color show up?
	inColor5	 ; Fifth random color
	inColorPerc5 ; How often should this color show up?
	inColor6	 ; Sixth random color
	inColorPerc6 ; How often should this color show up?
	inNumColors	 ; How many colors to use? (0 means any color)
	seed		 ; Random seed used to generate the grid
	)

	(let* (
		(s3 (sqrt 3))
		(row 0)
		(hex 1)
		(point 1)
		(xcenter 0)
		(ycenter 0)
		(x_coord1 0.0)
		(x_coord2 0.0)
		(x_coord3 0.0)
		(x_coord4 0.0)
		(y_coord1 0.0)
		(y_coord2 0.0)
		(selectx 0.0)
		(selecty 0.0)
		(colorCheck '(0 0 0))
		(colorCheck2 '(0 0 0))

		(x1 (/ hexSize 2.0))
		(y1 (* hexSize s3 0.5))

		(inWidth
			(cond
				((= tileAR TRUE) (* 3 hexSize (trunc (/ Width (* 3 hexSize)))))
				((= tileAR FALSE) Width)
			)
		)

		(inHeight
			(cond
				((= tileAR TRUE) (* s3 hexSize (trunc (/ Height (* s3 hexSize)))))
				((= tileAR FALSE) Height)
			)
		)

		(theImage (car (gimp-image-new inWidth inHeight RGB)))
		(baseLayer (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Hex Grid" 100 NORMAL-MODE)))
		(brush (car (gimp-brush-new "Hexagon Brush")))

		(inColor '(0 0 0))
		(bgColor '(255 255 255))

		(halfX (/ inWidth 2.0))
		(halfY (/ inHeight 2.0))

		(flag 0)
		(triflag (* 5 (- 1 Shape)))
		(lineArray (make-vector 8 0.0))
		(innerArray (make-vector 4 0.0))
		(xArray (make-vector 6 0.0))
		(yArray (make-vector 6 0.0))

		(total_sum 0)
		(inColors (list inColor1 inColor2 inColor3 inColor4 inColor5 inColor6))

		(inColorPercents 
			(list 0 
			(+ inColorPerc1) 
			(+ inColorPerc1 inColorPerc2) 
			(+ inColorPerc1 inColorPerc2 inColorPerc3) 
			(+ inColorPerc1 inColorPerc2 inColorPerc3 inColorPerc4) 
			(+ inColorPerc1 inColorPerc2 inColorPerc3 inColorPerc4 inColorPerc5) 
			(+ inColorPerc1 inColorPerc2 inColorPerc3 inColorPerc4 inColorPerc5 inColorPerc6) 
			)
		)

	(thePercent 0)
	(frameName "")
	)

; Get the random seed
	(set! seed (if (number? seed) seed (realtime)))
	(srand seed)

	(set! total_sum (list-ref inColorPercents inNumColors))

; Create the image
	(gimp-image-add-layer theImage baseLayer 0)

; If the lines are supposed to be white, change the background to black
	(gimp-context-set-foreground hexColor)
	(if (equal? hexColor bgColor)
		(set! bgColor '(0 0 0))
	)

; Fill the bse layer with the background color
	(gimp-context-set-background bgColor)
	(gimp-drawable-fill baseLayer BACKGROUND-FILL)

; Generate the brush to be Brush_Rad pixels
	(gimp-brush-set-shape brush BRUSH-GENERATED-CIRCLE)
	(gimp-brush-set-radius brush Brush_Rad)
	(gimp-brush-set-spikes brush 2)
	(gimp-brush-set-hardness brush 1)
	(gimp-brush-set-aspect-ratio brush 1)
	(gimp-brush-set-angle brush 0)
	(gimp-brush-set-spacing brush 20)
	(gimp-context-set-brush brush)

; Start going across the image
	(while (< xcenter inWidth)

; If we're on an even numbered row, shift everything over half a hex height
		(if(= (fmod row 2) 1)
			(set! ycenter y1)
			(set! ycenter 0.0)
		)

; get the X-coordinates for the center and points on the outer hexagon
		(set! xcenter (* 1.5 hexSize row))
		(set! x_coord1 (- xcenter hexSize))
		(set! x_coord2 (- xcenter x1))
		(set! x_coord3 (+ xcenter x1))
		(set! x_coord4 (+ xcenter hexSize))

; Set up the X-coordinates in the vector
		(vector-set! lineArray 0 x_coord1)
		(vector-set! lineArray 2 x_coord2)
		(vector-set! lineArray 4 x_coord3)
		(vector-set! lineArray 6 x_coord4)

; Now go up and down the image
		(while (< (- ycenter (* s3 hexSize)) inHeight)

; Get the Y-coordinates for the points of the outer hexagon
			(set! y_coord1 (+ ycenter y1))
			(set! y_coord2 (- ycenter y1))

; Top half of the hexagon
			(vector-set! lineArray 1 ycenter)
			(vector-set! lineArray 3 y_coord1)
			(vector-set! lineArray 5 y_coord1)
			(vector-set! lineArray 7 ycenter)
			(gimp-pencil baseLayer 8 lineArray)

; Bottom half of the hexagon
			(vector-set! lineArray 3 y_coord2)
			(vector-set! lineArray 5 y_coord2)
			(gimp-pencil baseLayer 8 lineArray)

; If we're doing a triangle, add in the interior lines
			(if (= Shape 1)
				(begin

; Line across the center of the hex
					(vector-set! innerArray 0 x_coord1)
					(vector-set! innerArray 1 ycenter)
					(vector-set! innerArray 2 x_coord4)
					(vector-set! innerArray 3 ycenter)
					(gimp-pencil baseLayer 4 innerArray)

; Bottom left to upper right
					(vector-set! innerArray 0 x_coord2)
					(vector-set! innerArray 1 y_coord1)
					(vector-set! innerArray 2 x_coord3)
					(vector-set! innerArray 3 y_coord2)
					(gimp-pencil baseLayer 4 innerArray)

; Bottom right to upper left
					(vector-set! innerArray 0 x_coord3)
					(vector-set! innerArray 2 x_coord2)
					(gimp-pencil baseLayer 4 innerArray)

; Set up most of the array of X and Y coordinates for the center of the triangles
					(vector-set! xArray 0 (/ (+ x_coord1 x_coord2 x_coord3) 3))
					(vector-set! xArray 1 (/ (+ x_coord3 x_coord2) 2))
					(vector-set! xArray 2 (/ (+ x_coord2 x_coord3 x_coord4) 3))
					(vector-set! xArray 3 (/ (+ x_coord1 x_coord2 x_coord3) 3))
					(vector-set! xArray 4 (/ (+ x_coord3 x_coord2) 2))

					(vector-set! yArray 0 (/ (+ ycenter y_coord1) 2))
					(vector-set! yArray 1 (/ (+ ycenter y_coord1) 2))
					(vector-set! yArray 2 (/ (+ ycenter y_coord1) 2))
					(vector-set! yArray 3 (/ (+ ycenter y_coord2) 2))
					(vector-set! yArray 4 (/ (+ ycenter y_coord2) 2))
				)
			) ; end Shape=1 loop

; Set these up separately, since they are needed even for a hexagonal grid
			(vector-set! xArray 5 (/ (+ x_coord2 x_coord3 x_coord4) 3))
			(vector-set! yArray 5 (/ (+ ycenter y_coord2) 2))

; Are the hexagons colored in?
			(if (= coloredHex 0)
				(begin
					(while (> 6 triflag)

;						(set! frameName (string-append "triflag " (number->string triflag)))
;						(gimp-drawable-set-name baseLayer frameName)

; Select the area inside each triangle or hexagon
						(set! selectx (min (max 0 (vector-ref xArray triflag)) (- inWidth 1)))
						(set! selecty (min (max 0 (vector-ref yArray triflag)) (- inHeight 1)))
						(set! triflag (+ 1 triflag))

; Get the color at the selection point
						(set! colorCheck2 (car (gimp-image-pick-color theImage baseLayer selectx selecty 0 0 0)))

; If the color matches the line color, do nothing.
						(if (equal? hexColor colorCheck2)
							(set! inColor hexColor)
							(begin
								(gimp-fuzzy-select baseLayer selectx selecty 0 CHANNEL-OP-ADD 0 0 0 0)

; Use a completely random color if numColors=0
								(if (= inNumColors 0)
									(set! inColor (list (rand 256) (rand 256) (rand 256)))
									(begin
										(set! thePercent (rand total_sum))

; Otherwise, select one of the chosen colors
										(if (>= inNumColors 1)
											(if (>= thePercent (list-ref inColorPercents 0))
												(set! inColor (list-ref inColors 0))
											)
										)

										(if (>= inNumColors 2)
											(if (>= thePercent (list-ref inColorPercents 1))
												(set! inColor (list-ref inColors 1))
											)
										)

										(if (>= inNumColors 3)
											(if (>= thePercent (list-ref inColorPercents 2))
												(set! inColor (list-ref inColors 2))
											)
										)

										(if (>= inNumColors 4)
											(if (>= thePercent (list-ref inColorPercents 3))
												(set! inColor (list-ref inColors 3))
											)
										)

										(if (>= inNumColors 5)
											(if (>= thePercent (list-ref inColorPercents 4))
												(set! inColor (list-ref inColors 4))
											)
										)

										(if (>= inNumColors 6)
											(if (>= thePercent (list-ref inColorPercents 5))
												(set! inColor (list-ref inColors 5))
											)
										)
									) ; end begin
								) ; end numcolors loop

								(if (= (car (gimp-selection-is-empty theImage)) FALSE)
									(begin
										(gimp-context-set-background inColor)
										(gimp-edit-bucket-fill baseLayer BG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
										(gimp-selection-none theImage)
									)
								) ; end selection checking

							) ; end begin loop
						) ; end if hexColor=colorCheck2 loop

					) ; end while 6>triflag loop

; Reset triflag to the proper value
				(set! triflag (* 5 (- 1 Shape)))
				) ; end begin loop

			) ; end coloredHex=0 loop

			(set! ycenter (+ ycenter (* s3 hexSize)))
		) ; end (while (< (- ycenter (* s3 hexSize)) inHeight) loop

		(set! row (+ 1 row))
	) ; end (while (< xcenter inWidth) loop

	(gimp-brush-delete brush)
	
	(if (= tileAR TRUE)
		(begin
			(set! row 0)
			(if (= coloredHex 0)
				(begin
					(set! xcenter 0)
; ycenter is 0 for hexagons, or a finction for the triangles
					(set! ycenter (* 0.25 s3 hexSize Shape))

					(while (< (- ycenter (* s3 hexSize)) inHeight)
						(begin
							(set! selecty (min (- inHeight 1) ycenter))
							(gimp-fuzzy-select baseLayer 0 selecty 0 CHANNEL-OP-REPLACE 0 0 0 0)
							(gimp-fuzzy-select baseLayer (- inWidth 1) selecty 0 CHANNEL-OP-ADD 0 0 0 0)
							(set! colorCheck (car (gimp-image-pick-color theImage baseLayer xcenter selecty 0 0 0)))
							(gimp-context-set-foreground colorCheck)
							(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
							(set! ycenter (+ ycenter (* s3 hexSize (- 1.0 (* Shape 0.5)))))
						)
					)

; Only do the top and bottom if we're using hexagons
					(if (= Shape 0)
						(while (< xcenter inWidth)
							(set! xcenter (* 1.5 hexSize row))
							(set! selectx (min (max 0 (- inWidth 1)) xcenter))
							(gimp-fuzzy-select baseLayer selectx 0 0 CHANNEL-OP-REPLACE 0 0 0 0)
							(gimp-fuzzy-select baseLayer selectx (- inHeight 1) 0 CHANNEL-OP-ADD 0 0 0 0)
							(set! colorCheck2 (car (gimp-image-pick-color theImage baseLayer selectx 0 0 0 0)))
							(gimp-context-set-foreground colorCheck2)
							(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
							(set! row (+ 2 row))
						)
					) ; end Shape=0 loop
				)
			) ; End coloredHex=0 loop

			(gimp-selection-none theImage)
;			(set! frameName (string-append "Offset Hex Grid " (number->string seed)))
		)
	) ; end tileAR=true loop

	(if (= coloredHex 2)
		(begin
			(gimp-layer-add-alpha baseLayer)
			(plug-in-colortoalpha RUN-NONINTERACTIVE theImage baseLayer bgColor)
		)
	)

	(if (= Shape 0)
		(set! frameName (string-append "Hex Grid " (number->string seed)))
		(set! frameName (string-append "Triangular Grid " (number->string seed)))
	)

	(gimp-drawable-set-name baseLayer frameName)
	(gimp-display-new theImage)

	)
)

(script-fu-register
	"script-fu-HexorTri"
	"<Image>/Filters/SambrookJM/Hexagon or Triangle Grid..."  ; edit this line to change where it shows up
	"Creates a grid of regular hexagons.  They can be colored in with random colors, tiled, or both."
	"James Sambrook"
	"James Sambrook"
	"May 2011"
	""
	SF-OPTION      "Shape?"				'(_"Hexagon"
										  _"Triangle")
	SF-ADJUSTMENT _"Image width"		'(640 300 3000 1 50 0 0)
	SF-ADJUSTMENT _"Image height"		'(480 300 3000 1 50 0 0)
	SF-ADJUSTMENT _"Side Length"		'(50 20 200 1 10 0 0)
	SF-ADJUSTMENT _"Brush Radius"		'(1 1 10 1 5 0 0)
	SF-COLOR	  _"Shape Color"		'(0 0 0)
    SF-TOGGLE     _"Tileable Array?"	FALSE
	SF-OPTION     _"Hex color?"		 '(_"Random"
									_"White"
									_"Transparent")
    SF-COLOR	  _"Color 1:"			'(255 0 0)				;color variable
	SF-ADJUSTMENT _"Color 1 Ratio:"		'(20 1 100 1 5 0 0)		;color percent
	SF-COLOR	  _"Color 2:"			'(0 255 0)				;color variable
	SF-ADJUSTMENT _"Color 2 Ratio:"		'(20 1 100 1 5 0 0)		;color percent
	SF-COLOR	  _"Color 3:"			'(0 0 255)				;color variable
	SF-ADJUSTMENT _"Color 3 Ratio:"		'(20 1 100 1 5 0 0)		;color percent
	SF-COLOR	  _"Color 4:"			'(0 255 255)				;color variable
	SF-ADJUSTMENT _"Color 4 Ratio:"		'(20 1 100 1 5 0 0)		;color percent
	SF-COLOR	  _"Color 5:"			'(255 0 255)				;color variable
	SF-ADJUSTMENT _"Color 5 Ratio:"		'(20 1 100 1 5 0 0)		;color percent
	SF-COLOR	  _"Color 6:"			'(255 255 0)				;color variable
	SF-ADJUSTMENT _"Color 6 Ratio:"		'(20 1 100 1 5 0 0)		;color percent
	SF-ADJUSTMENT _"Number of Colors (0 for completely random)" '(6 0 6 1 1 0 0)
	SF-VALUE	_"Random seed?"  "random"
)
