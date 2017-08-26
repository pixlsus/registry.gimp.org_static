(define (fg/bg-selection image drawable)
	(gimp-edit-bucket-fill drawable FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
	(gimp-selection-invert image)
	(gimp-edit-bucket-fill drawable BG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
)

(define (get-blending-mode-galaxy mode)
  (let* ((modenumbers #(0 1 3 15 4 5 16 17 18 19 20 21 6 7 8 9 10 11 12 13 14)))
    (vector-ref modenumbers mode)
  )
)

(define (math-round-galaxy input)
  (floor (+ input 0.5))
)

(define (get-layer-pos-galaxy img layer)
  (let* ((layerdata (gimp-image-get-layers img))
	 (numlayers (car layerdata))
	 (layerarray (cadr layerdata))
	 (i 0)
	 (pos -1)
	)
    (while (< i numlayers)
      (if (= layer (vector-ref layerarray i))
	(begin
	  (set! pos i)
	  (set! i numlayers)
	)
	(set! i (+ i 1))
      )
    )
    pos
  )
)

(define (add-under-layer-galaxy img newlayer oldlayer)
  (gimp-image-add-layer img newlayer (+ (get-layer-pos-galaxy img oldlayer) 1))
)

(define (draw-blurshape-galaxy img drawable size initgrowth sel invert)
  (let* ((k initgrowth)
	 (currshade 0)
	 (i 0))
    (while (< i size)
      (if (> k 0)
	(gimp-selection-grow img k)
	(if (< k 0)
	  (gimp-selection-shrink img (abs k))
	)
      )
      (if (= invert 1)
	(set! currshade (math-round-galaxy (* (/ (- size (+ i 1)) size) 255)))
	(set! currshade (math-round-galaxy (* (/ (+ i 1) size) 255)))
      )
      (gimp-palette-set-foreground (list currshade currshade currshade))
      (if (= (car (gimp-selection-is-empty img)) 0)
	(gimp-edit-fill drawable 0)
      )
      (gimp-selection-load sel)
      (set! k (- k 1))
      (set! i (+ i 1))
    )
  )
)

(define (apply-contour-galaxy drawable channel contour)
  (let* ((contourtypes #(0 0 0 0 0 0 0 0 0 1 1))
	 (contourlengths #(6 6 10 14 18 10 18 18 10 256 256))
	 (contours #(#(0 0 127 255 255 0)
#(0 255 127 0 255 255)
#(0 64 94 74 150 115 179 179 191 255)
#(0 0 5 125 6 125 48 148 79 179 107 217 130 255)
#(0 0 33 8 64 38 97 102 128 166 158 209 191 235 222 247 255 255)
#(0 0 28 71 87 166 194 240 255 255)
#(0 0 33 110 64 237 97 240 128 138 158 33 191 5 222 99 255 255)
#(0 0 33 74 64 219 97 186 128 0 158 176 191 201 222 3 255 255)
#(3 255 54 99 97 107 179 153 252 0)
#(0 5 9 13 16 19 22 25 27 29 30 32 33 34 35 36 38 39 40 41 43 44 46 47 48 49 50 51 52 53 54 55 55 56 56 57 57 58 58 59 59 59 60 60 60 61 61 61 61 62 62 62 62 62 63 63 63 63 63 63 64 64 64 64 64 71 75 78 81 84 86 89 91 93 95 96 98 99 101 102 103 104 105 107 107 108 110 111 112 113 114 115 116 117 118 119 119 120 121 121 122 123 123 123 124 124 124 125 125 125 125 125 125 125 126 126 126 126 126 126 126 125 125 125 125 125 125 125 125 130 134 137 141 145 148 151 153 156 158 160 162 163 165 166 167 168 170 171 171 172 173 174 175 176 177 178 178 179 180 181 181 182 183 183 184 184 185 185 186 186 187 187 188 188 189 189 189 189 190 190 190 190 191 191 191 191 191 191 191 191 191 191 193 194 196 197 198 200 201 203 204 205 207 208 209 211 212 213 214 215 217 218 219 220 220 221 222 222 223 223 224 224 224 224 224 223 223 222 222 221 221 220 219 218 217 216 215 214 213 212 211 210 209 208 206 205 204 203 202 200 199 198 197 196 194 194)
#(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 127 125 123 121 119 117 115 113 111 109 107 105 103 101 99 97 95 93 91 89 87 85 83 81 79 77 75 73 71 69 67 65 63 61 59 57 55 53 51 49 47 45 43 41 39 37 35 33 31 29 27 25 23 21 19 17 15 13 11 9 7 5 3 1 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99 101 103 105 107 109 111 113 115 117 119 121 123 125 127 128 126 124 122 120 118 116 114 112 110 108 106 104 102 100 98 96 94 92 90 88 86 84 82 80 78 76 74 72 70 68 66 64 62 60 58 56 54 52 50 48 46 44 42 40 38 36 34 32 30 28 26 24 22 20 18 16 14 12 10 8 6 4 2))))
    (if (= (vector-ref contourtypes (- contour 1)) 0)
      (gimp-curves-spline drawable channel (vector-ref contourlengths (- contour 1)) (vector-ref contours (- contour 1)))
      (gimp-curves-explicit drawable channel (vector-ref contourlengths (- contour 1)) (vector-ref contours (- contour 1)))
    )
  )
)

(define (apply-noise-galaxy img drawable srclayer noise)
  (let* ((drwwidth (car (gimp-drawable-width srclayer)))
	 (drwheight (car (gimp-drawable-height srclayer)))
	 (layername (car (gimp-drawable-get-name drawable)))
	 (drwoffsets (gimp-drawable-offsets srclayer))
	 (srcmask (car (gimp-layer-get-mask srclayer)))
	 (noiselayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-noise") 100 0)))
	 (blanklayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-noise") 100 0))))
    (add-over-layer img noiselayer srclayer)
    (add-over-layer img blanklayer noiselayer)
    (gimp-layer-set-offsets noiselayer (car drwoffsets) (cadr drwoffsets))
    (gimp-layer-set-offsets blanklayer (car drwoffsets) (cadr drwoffsets))
    (gimp-selection-all img)
    (gimp-palette-set-foreground '(0 0 0))
    (gimp-edit-fill noiselayer 0)
    (gimp-edit-fill blanklayer 0)
    (gimp-palette-set-foreground '(255 255 255))
    (gimp-selection-load srcmask)
    (gimp-edit-fill blanklayer 0)
    (plug-in-hsv-noise 1 img noiselayer 1 0 0 255)
    (gimp-layer-set-mode blanklayer 5)
    (gimp-layer-set-opacity blanklayer noise)
    (set! noiselayer (car (gimp-image-merge-down img blanklayer 0)))
    (set! blanklayer (car (gimp-layer-create-mask noiselayer 5)))
    (gimp-channel-combine-masks srcmask blanklayer 2 0 0)
    (gimp-image-remove-layer img noiselayer)
  )
)

(define (script-fu-layerfx-outer-glow-galaxy img
				      drawable
				      color
				      opacity
				      contour
				      noise
				      mode
				      spread
				      size
				      knockout
				      merge)

(let* ((origfgcolor (car (gimp-palette-get-foreground)))
	 (origselection (car (gimp-selection-save img)))
	 (drwwidth (car (gimp-drawable-width drawable)))
	 (drwheight (car (gimp-drawable-height drawable)))
	 (drwoffsets (gimp-drawable-offsets drawable))
	 (layername (car (gimp-drawable-get-name drawable)))
	 (lyrgrowamt (math-round-galaxy (* size 1.2)))
	 (glowlayer (car (gimp-layer-new img (+ drwwidth (* lyrgrowamt 2)) (+ drwheight (* lyrgrowamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-outerglow") opacity (get-blending-mode-galaxy mode))))
	 (glowmask 0)
	 (alphaSel 0)
	 (growamt (* (/ spread 100) size))
	 (steps (- size growamt))
	 (origmask 0)
	)
    (add-under-layer-galaxy img glowlayer drawable)
    (gimp-layer-set-offsets glowlayer (- (car drwoffsets) lyrgrowamt) (- (cadr drwoffsets) lyrgrowamt))
    (gimp-selection-all img)
    (gimp-palette-set-foreground color)
    (gimp-edit-fill glowlayer 0)
    (gimp-selection-none img)
    (set! glowmask (car (gimp-layer-create-mask glowlayer 1)))
    (gimp-layer-add-mask glowlayer glowmask)
    (gimp-selection-layer-alpha drawable)
    (if (> (car (gimp-layer-get-mask drawable)) -1)
      (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
    )
    (set! alphaSel (car (gimp-selection-save img)))
    (draw-blurshape-galaxy img glowmask steps size alphaSel 0)
    (gimp-selection-none img)
    (if (> contour 0)
      (begin
	(apply-contour-galaxy glowmask 0 contour)
	(gimp-selection-load alphaSel)
	(gimp-selection-grow img size)
	(gimp-selection-invert img)
	(gimp-palette-set-foreground '(0 0 0))
	(gimp-edit-fill glowmask 0)
	(gimp-selection-none img)
      )
    )
    (if (> noise 0)
      (apply-noise-galaxy img drawable glowlayer noise)
    )
    (if (= knockout 1)
      (begin
	(gimp-palette-set-foreground '(0 0 0))
	(gimp-selection-layer-alpha drawable)
	(gimp-edit-fill glowmask 0)
      )
    )
    (gimp-layer-remove-mask glowlayer 0)
    (gimp-selection-none img)
    (if (= merge 1)
      (begin
	(set! origmask (car (gimp-layer-get-mask drawable)))
	(if (> origmask -1)
	  (gimp-layer-remove-mask drawable 0)
	)
	(set! glowlayer (car (gimp-image-merge-down img drawable 0)))
	(gimp-drawable-set-name glowlayer layername)
      )
    )
    (gimp-palette-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-displays-flush)

	(gimp-selection-layer-alpha glowlayer)
  )
)

(define
	(script-fu-galaxy
		numArms
		holeSize
		innerRad
		armSize
		startAngle
		ArmCurve
		NumStars
		maxStar
		hSkew
		vSkew
		radSkew
		cloudColor
		seed
	)

	(let*
		(
		(imageSize (* 2 (+ innerRad armSize)))
		(centerPoint (/ imageSize 2.0))
		(theImage (car (gimp-image-new imageSize imageSize RGB)))
		(baseLayer (car (gimp-layer-new theImage imageSize imageSize RGBA-IMAGE "Galaxy" 100 SCREEN-MODE)))
		(cloudLayer (car (gimp-layer-new theImage imageSize imageSize RGBA-IMAGE "Plasma" 100 OVERLAY-MODE)))
		(starLayer (car (gimp-layer-new theImage imageSize imageSize RGBA-IMAGE "Stars" 100 NORMAL-MODE)))
		(scratchLayer 0)
		(novaLayer 0)
		(area 0)
		(blackLayer 0)
		(testLayer 0)

		(diameter (* 2.0 holeSize))
		(flag (* -1 ArmCurve))
		(toothWidth (/ imageSize numArms))
		(outerEdge (- (* 2.0 innerRad) diameter))
		(centerX 0)
		(centerY 0)
		(rotAngle (* startAngle (/ *pi* 180.0)))

		(selection-bounds 0)
		(select-offset-x 0)
		(select-offset-y 0)
		(selection-width 0)
		(selection-height 0)
		(selArea 0.0)

		(x0 0)
		(x1 0)
		(x2 0)
		(x3 0)
		(y1 0)
		(y2 0)
		(pointArray (make-vector 8 0.0))

		(starSize 0)
		(Star_X 0)
		(Star_Y 0)
		(squared 0)
		(starAngle 0.0)
		(starRad 0.0)

		(oldHeight 0)
		(oldWidth 0)
		(Wprime 0)
		(Hprime 0)
		(newHeight 0)
		(newWidth 0)
		(radAngle (* radSkew (/ *pi* 180.0)))
		(tanTheta (tan radAngle))
		(size 0)
		
		(frameName "")
		(frameNum 0)

		)

		(set! seed (if (number? seed) seed (realtime)))
		(srand seed)

		(gimp-image-add-layer theImage baseLayer 0)
		(gimp-context-set-foreground cloudColor)
		(gimp-context-set-background '(255 255 255))
		(gimp-drawable-fill baseLayer BACKGROUND-FILL)

		(gimp-rect-select theImage 0 diameter imageSize outerEdge CHANNEL-OP-ADD FALSE 0)

		(while (< flag numArms)
			(begin
				(vector-set! pointArray 0 (* flag toothWidth))
				(vector-set! pointArray 1 (+ outerEdge diameter))
				(vector-set! pointArray 2 (* (+ ArmCurve 0.5 flag) toothWidth))
				(vector-set! pointArray 3 (+ diameter outerEdge armSize))
				(vector-set! pointArray 4 (* (+ 1 flag) toothWidth))
				(vector-set! pointArray 5 (+ outerEdge diameter))
				(vector-set! pointArray 6 (* flag toothWidth))
				(vector-set! pointArray 7 (+ outerEdge diameter))
				(gimp-free-select theImage 8 pointArray CHANNEL-OP-ADD 0 0 0)
				(set! flag (+ 1 flag))
			)
		)

		(set! flag 0)

		(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE 0 0)

		(gimp-selection-invert theImage)
		(plug-in-colortoalpha RUN-NONINTERACTIVE theImage baseLayer '(255 255 255))
		(gimp-selection-clear theImage)

		(plug-in-polar-coords RUN-NONINTERACTIVE theImage baseLayer 100 0 FALSE TRUE TRUE)

		(if (> startAngle 0)
			(gimp-drawable-transform-rotate-default baseLayer rotAngle TRUE centerPoint centerPoint TRUE TRANSFORM-RESIZE-ADJUST)
		)

		(gimp-selection-layer-alpha baseLayer)
		(gimp-selection-grow theImage 2)
		(gimp-edit-clear baseLayer)

		(script-fu-distress-selection theImage baseLayer 127 8 4 2 0 0)
		(script-fu-distress-selection theImage baseLayer 127 8 4 2 0 0)
		(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE 0 0)

		(set! selection-bounds (gimp-selection-bounds theImage))
		(set! select-offset-x (cadr selection-bounds))
		(set! select-offset-y (caddr selection-bounds))
		(set! selection-width (- (cadr (cddr selection-bounds)) select-offset-x))
		(set! selection-height (- (caddr (cddr selection-bounds)) select-offset-y))

		(set! selArea (* 0.25 *pi* selection-width selection-height))

		(set! scratchLayer (car (gimp-layer-copy baseLayer FALSE)))
		(gimp-image-add-layer theImage scratchLayer 0)
		
		(plug-in-autocrop-layer RUN-NONINTERACTIVE theImage scratchLayer)
		(gimp-context-set-background '(0 0 0))
		(gimp-context-set-foreground '(255 0 0))
		(fg/bg-selection theImage scratchLayer)
		(gimp-selection-none theImage)

		(let (
			(histogram (gimp-histogram scratchLayer HISTOGRAM-RED 128 255)))
			(set! area (/ (list-ref histogram 4) selArea))
		)
		(gimp-selection-none theImage)
		(gimp-image-remove-layer theImage scratchLayer)

		(set! blackLayer (car (gimp-layer-copy baseLayer FALSE)))
		(gimp-image-add-layer theImage blackLayer 0)
		(gimp-layer-set-mode blackLayer NORMAL-MODE)
		(gimp-context-set-background '(0 0 0))
		(gimp-drawable-fill blackLayer BACKGROUND-FILL)
		(gimp-drawable-set-name blackLayer "Blackness of Space")
		(gimp-image-lower-layer-to-bottom theImage blackLayer)

		(set! size (min selection-width selection-height))
		(set! squared (* size size))

		(gimp-image-add-layer theImage starLayer 0)
		(gimp-drawable-set-name starLayer "Stars")
		
		(while (< flag (/ NumStars area))
			(begin
				(set! starSize (+ 1 (rand maxStar)))
				(set! starRad (rand squared))
				(set! starAngle (/ (* *pi* (rand 3600.0)) 1800.0))
				(set! Star_X (+ select-offset-x (/ size 2.0) (/ (+ size (- (* starRad (sin starAngle)) starSize)) (* 2.0 size))))
				(set! Star_Y (+ select-offset-x (/ size 2.0) (/ (+ size (- (* starRad (cos starAngle)) starSize)) (* 2.0 size))))
				(gimp-rect-select theImage Star_X Star_Y starSize starSize CHANNEL-OP-REPLACE FALSE 0)
				(gimp-context-set-foreground (list (rand 256) (rand 256) (rand 256)))
				(gimp-edit-bucket-fill starLayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE 0 0)
				(set! flag (+ 1 flag))
			)
		)
		(gimp-selection-none theImage)

		(gimp-image-add-layer theImage cloudLayer 0)

		(plug-in-plasma RUN-NONINTERACTIVE theImage cloudLayer (rand 1000000000) 7)
		(gimp-desaturate cloudLayer)
		(plug-in-whirl-pinch RUN-NONINTERACTIVE theImage cloudLayer (* 45.0 ArmCurve) 0 1.0)

		(gimp-image-crop theImage (+ 40 selection-width) (+ 40 selection-height) (- select-offset-x 20) (- select-offset-y 20))
		(plug-in-cubism RUN-NONINTERACTIVE theImage baseLayer (/ selection-height 100.0) 1.5 0)
		(plug-in-gauss-rle2 RUN-NONINTERACTIVE theImage baseLayer (/ selection-width 100.0) (/ selection-height 100.0))

		(gimp-selection-layer-alpha baseLayer)
		(gimp-selection-invert theImage)
		(gimp-edit-clear starLayer)
		(gimp-selection-none theImage)

		(gimp-image-flatten theImage)
		(set! testLayer (car (gimp-image-get-active-layer theImage)))
		
		(set! oldHeight (car (gimp-drawable-height testLayer)))
		(set! oldWidth (car (gimp-drawable-width testLayer)))
		(set! centerX (/ oldWidth 2.0))
		(set! centerY (/ oldHeight 2.0))
		(set! newHeight (* vSkew oldHeight))
		(set! newWidth (* hSkew oldWidth))

		(if (= radSkew 0)
			(gimp-image-scale theImage newWidth newHeight)
			(begin
				(set! Wprime newWidth)
				(set! Hprime newHeight)
				(set! x0 (- centerX (* 0.5 (+ newWidth (/ newHeight tanTheta)))))
				(set! x1 (+ centerX (* 0.5 (- newWidth (/ newHeight tanTheta)))))
				(set! x2 (- centerX (* 0.5 (- newWidth (/ newHeight tanTheta)))))
				(set! x3 (+ centerX (* 0.5 (+ newWidth (/ newHeight tanTheta)))))
				(set! y1 (- centerY (* 0.5 oldHeight)))
				(set! y2 (+ centerY (* 0.5 oldHeight)))
				(gimp-drawable-transform-perspective-default testLayer x0 y1 x1 y1 x2 y2 x3 y2 TRUE TRANSFORM-RESIZE-ADJUST)
				(gimp-image-resize-to-layers theImage)
			)
		)

		(gimp-image-flatten theImage)
		(set! testLayer (car (gimp-image-get-active-layer theImage)))
		(plug-in-zealouscrop RUN-NONINTERACTIVE theImage testLayer)
		(set! oldHeight (car (gimp-drawable-height testLayer)))
		(set! oldWidth (car (gimp-drawable-width testLayer)))

		(set! novaLayer (car (gimp-layer-copy testLayer FALSE)))
		(gimp-image-add-layer theImage novaLayer 0)
		(gimp-drawable-set-name novaLayer "Nova")
		(gimp-layer-set-mode novaLayer NORMAL-MODE)
		(gimp-context-set-background '(0 0 0))
		(gimp-drawable-fill novaLayer BACKGROUND-FILL)

		(plug-in-nova RUN-NONINTERACTIVE theImage novaLayer (/ oldWidth 2) (/ oldHeight 2) cloudColor (min 100 (min (/ oldWidth 10) (/ oldHeight 10))) 1 0)
		(plug-in-colortoalpha RUN-NONINTERACTIVE theImage novaLayer '(0 0 0))
		(gimp-selection-layer-alpha novaLayer)
		(gimp-selection-invert theImage)
		(gimp-edit-clear novaLayer)
		(gimp-selection-none theImage)
		(plug-in-gauss-rle2 RUN-NONINTERACTIVE theImage novaLayer 100 100.0)
		(gimp-image-flatten theImage)

		(set! testLayer (car (gimp-image-get-active-layer theImage)))
		(set! scratchLayer (car (gimp-layer-copy testLayer FALSE)))
		(gimp-image-add-layer theImage scratchLayer 0)
		(gimp-drawable-fill scratchLayer BACKGROUND-FILL)
		(gimp-image-lower-layer-to-bottom theImage scratchLayer)

		(plug-in-colortoalpha RUN-NONINTERACTIVE theImage testLayer '(0 0 0))
		(gimp-selection-layer-alpha testLayer)
		(script-fu-layerfx-outer-glow-galaxy theImage testLayer cloudColor 50 0 0 0 0 5.0 FALSE FALSE)
		(set! novaLayer (car (gimp-image-get-active-layer theImage)))

		(plug-in-hsv-noise RUN-NONINTERACTIVE theImage scratchLayer 1 180 255 255)
		(gimp-selection-invert theImage)
		(gimp-edit-clear novaLayer)
		(gimp-edit-clear scratchLayer)
		(gimp-selection-none theImage)

		(gimp-image-flatten theImage)
		(set! baseLayer (car (gimp-image-get-active-layer theImage)))
		(set! frameName (string-append "Galaxy " (number->string seed frameNum)))
		(gimp-drawable-set-name baseLayer frameName)

		(gimp-display-new theImage)
	)
)

(script-fu-register
	"script-fu-galaxy"
	_"_Galaxy..."
	_"Creates a picture of a spiral galaxy..."
	"James Sambrook"
	"23 February 2011"
	""
	""
	SF-ADJUSTMENT _"Number of arms"	'(8 4 36 1 5 0 0)
	SF-ADJUSTMENT _"Hole Size"		'(25 0 100 1 10 0 0)
	SF-ADJUSTMENT _"Inner Radius"	'(100 10 1000 1 10 0 0)
	SF-ADJUSTMENT _"Arm Size"		'(500 10 1000 1 50 0 0)
	SF-ADJUSTMENT _"Angle first arm starts at"	'(0 0 360 1 30 0 0)
	SF-ADJUSTMENT _"Arm Curvature"	'(6 0 10 0.1 1 1 0)
	SF-ADJUSTMENT _"Number of Stars"	'(200 0 3000 100 500 0 0)
	SF-ADJUSTMENT _"Maximum star size (in pixels)"	'(3 1 10 1 1 0 0)
	SF-ADJUSTMENT _"Horizontal skew"	'(1 0.1 5 0.1 1 1 0)
	SF-ADJUSTMENT _"Vertical skew"	'(1 0.1 5 0.1 1 1 0)
	SF-ADJUSTMENT _"Skew Angle"		'(60 -89 89 1 10 0 0)
	SF-COLOR      _"Galaxy color"	'(100 149 237)
	SF-VALUE      _"Random seed"    "random"

)

(script-fu-menu-register "script-fu-galaxy"
	"<Image>/Filters/SambrookJM/")