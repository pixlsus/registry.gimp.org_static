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

; Courtesy of Jonathan Stipe <JonStipe@prodigy.net> and his layer effects scripts
(define (math-round-wall input)
  (floor (+ input 0.5))
)

; Courtesy of Jonathan Stipe <JonStipe@prodigy.net> and his layer effects scripts
(define (math-ceil-wall input)
  (if (= input (floor input))
    input
    (+ (floor input) 1)
  )
)

; Courtesy of Jonathan Stipe <JonStipe@prodigy.net> and his layer effects scripts
(define (get-blending-mode-wall mode)
  (let* ((modenumbers #(0 1 3 15 4 5 16 17 18 19 20 21 6 7 8 9 10 11 12 13 14)))
    (vector-ref modenumbers mode)
  )
)

; Courtesy of Jonathan Stipe <JonStipe@prodigy.net> and his layer effects scripts
(define (add-over-layer-wall img newlayer oldlayer)
  (gimp-image-add-layer img newlayer (get-layer-pos-wall img oldlayer))
)

; Courtesy of Jonathan Stipe <JonStipe@prodigy.net> and his layer effects scripts
(define (draw-blurshape-wall img drawable size initgrowth sel invert)
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
	(set! currshade (math-round-wall (* (/ (- size (+ i 1)) size) 255)))
	(set! currshade (math-round-wall (* (/ (+ i 1) size) 255)))
      )
      (gimp-context-set-foreground (list currshade currshade currshade))
      (if (= (car (gimp-selection-is-empty img)) 0)
	(gimp-edit-fill drawable 0)
      )
      (gimp-selection-load sel)
      (set! k (- k 1))
      (set! i (+ i 1))
    )
  )
)

; Courtesy of Jonathan Stipe <JonStipe@prodigy.net> and his layer effects scripts
(define (get-layer-pos-wall img layer)
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

; Courtesy of Jonathan Stipe <JonStipe@prodigy.net> and his layer effects scripts
(define (script-fu-layerfx-wall-bevel-emboss img
					drawable
					style
					depth
					direction
					size
					soften
					angle
					altitude
					glosscontour
					highlightcolor
					highlightmode
					highlightopacity
					shadowcolor
					shadowmode
					shadowopacity
					surfacecontour
					invert
					merge)
  (gimp-image-undo-group-start img)
  (let* ((origfgcolor (car (gimp-palette-get-foreground)))
	 (origselection (car (gimp-selection-save img)))
	 (drwwidth (car (gimp-drawable-width drawable)))
	 (drwheight (car (gimp-drawable-height drawable)))
	 (drwoffsets (gimp-drawable-offsets drawable))
	 (layername (car (gimp-drawable-get-name drawable)))
	 (imgtype (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)))
	 (lyrgrowamt (math-round-wall (* size 1.2)))
	 (bumpmaplayer 0)
	 (highlightlayer 0)
	 (highlightmask 0)
	 (shadowlayer 0)
	 (shadowmask 0)
	 (layersize 0)
	 (alphaSel 0)
	 (halfsizef 0)
	 (halfsizec 0)
	 (origmask 0)
	 (alphamask 0)
	)
    (cond
      ((= style 0)
	(begin
	  (set! layersize (list
	    (+ drwwidth (* lyrgrowamt 2))
	    (+ drwheight (* lyrgrowamt 2))
	    (- (car drwoffsets) lyrgrowamt)
	    (- (cadr drwoffsets) lyrgrowamt)
	  ))
	)
      )
      ((= style 1)
	(begin
	  (set! layersize (list
	    drwwidth
	    drwheight
	    (car drwoffsets)
	    (cadr drwoffsets)
	  ))
	)
      )
      ((= style 2)
	(begin
	  (set! layersize (list
	    (+ drwwidth lyrgrowamt)
	    (+ drwheight lyrgrowamt)
	    (- (car drwoffsets) (floor (/ lyrgrowamt 2)))
	    (- (cadr drwoffsets) (floor (/ lyrgrowamt 2)))
	  ))
	)
      )
      (
	(begin
	  (set! layersize (list
	    (+ drwwidth lyrgrowamt)
	    (+ drwheight lyrgrowamt)
	    (- (car drwoffsets) (floor (/ lyrgrowamt 2)))
	    (- (cadr drwoffsets) (floor (/ lyrgrowamt 2)))
	  ))
	)
      )
    )
    (set! bumpmaplayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-bumpmap") 100 0)))
    (set! highlightlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-highlight") highlightopacity (get-blending-mode-wall highlightmode))))
    (set! shadowlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-shadow") shadowopacity (get-blending-mode-wall shadowmode))))
    (add-over-layer-wall img bumpmaplayer drawable)
    (add-over-layer-wall img shadowlayer bumpmaplayer)
    (add-over-layer-wall img highlightlayer shadowlayer)
    (gimp-layer-set-offsets bumpmaplayer (caddr layersize) (cadddr layersize))
    (gimp-layer-set-offsets shadowlayer (caddr layersize) (cadddr layersize))
    (gimp-layer-set-offsets highlightlayer (caddr layersize) (cadddr layersize))
    (gimp-selection-all img)
    (gimp-context-set-foreground highlightcolor)
    (gimp-edit-fill highlightlayer 0)
    (gimp-context-set-foreground shadowcolor)
    (gimp-edit-fill shadowlayer 0)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill bumpmaplayer 0)
    (set! highlightmask (car (gimp-layer-create-mask highlightlayer 1)))
    (set! shadowmask (car (gimp-layer-create-mask shadowlayer 1)))
    (gimp-layer-add-mask highlightlayer highlightmask)
    (gimp-layer-add-mask shadowlayer shadowmask)
    (gimp-selection-layer-alpha drawable)
    (if (> (car (gimp-layer-get-mask drawable)) -1)
       (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
    )
    (set! alphaSel (car (gimp-selection-save img)))
    (cond
      ((= style 0)
	(draw-blurshape-wall img bumpmaplayer size size alphaSel 0)
      )
      ((= style 1)
	(draw-blurshape-wall img bumpmaplayer size 0 alphaSel 0)
      )
      ((= style 2)
	(begin
	  (set! halfsizec (math-ceil-wall (/ size 2)))
	  (draw-blurshape-wall img bumpmaplayer size halfsizec alphaSel 0)
	)
      )
      (
	(begin
	  (set! halfsizef (floor (/ size 2)))
	  (set! halfsizec (- size halfsizef))
	  (gimp-selection-all img)
	  (gimp-context-set-foreground '(255 255 255))
	  (gimp-edit-fill bumpmaplayer 0)
	  (draw-blurshape-wall img bumpmaplayer halfsizec halfsizec alphaSel 1)
	  (draw-blurshape-wall img bumpmaplayer halfsizef 0 alphaSel 0)
	)
      )
    )
    (gimp-selection-all img)
    (gimp-context-set-foreground '(127 127 127))
    (gimp-edit-fill highlightmask 0)
    (gimp-selection-none img)
    (if (> surfacecontour 0)
      (apply-contour bumpmaplayer 0 surfacecontour)
    )
    (if (< angle 0)
      (set! angle (+ angle 360))
    )
    (plug-in-bump-map 1 img highlightmask bumpmaplayer angle altitude depth 0 0 0 0 1 direction 0)
    (if (> glosscontour 0)
      (apply-contour highlightmask 0 glosscontour)
    )
    (if (> soften 0)
      (plug-in-gauss-rle 1 img highlightmask soften 1 1)
    )
    (if (> invert 0)
      (gimp-invert highlightmask)
    )
    (gimp-channel-combine-masks shadowmask highlightmask 2 0 0)
    (gimp-levels highlightmask 0 127 255 1.0 0 255)
    (gimp-levels shadowmask 0 0 127 1.0 255 0)
    (gimp-selection-load alphaSel)
    (if (= style 0)
      (gimp-selection-grow img size)
      (if (or (= style 2) (= style 3))
	(gimp-selection-grow img halfsizec)
      )
    )
    (gimp-selection-invert img)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill shadowmask 0)
    (gimp-selection-none img)
    (gimp-image-remove-layer img bumpmaplayer)
    (if (= merge 1)
      (if (= style 1)
	(begin
	  (set! origmask (car (gimp-layer-get-mask drawable)))
	  (if (> origmask -1)
	    (begin
	      (set! origmask (car (gimp-channel-copy origmask)))
	      (gimp-layer-remove-mask drawable 1)
	    )
	  )
	  (set! alphamask (car (gimp-layer-create-mask drawable 3)))
	  (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
	  (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
	  (gimp-drawable-set-name highlightlayer layername)
	  (gimp-layer-add-mask highlightlayer alphamask)
	  (gimp-layer-remove-mask highlightlayer 0)
	  (if (> origmask -1)
	    (gimp-layer-add-mask highlightlayer origmask)
	  )
	)
	(begin
	  (set! origmask (car (gimp-layer-get-mask drawable)))
	  (if (> origmask -1)
	    (gimp-layer-remove-mask drawable 0)
	  )
	  (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
	  (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
	  (gimp-drawable-set-name highlightlayer layername)
	)
      )
    )
    (gimp-context-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

; Finally, the actual brick wall script begins here.
(define (script-fu-brickwall
	inWidth			; Width of the original image
	inHeight		; Height of the original image
	brickWidth		; Width of the bricks in the wall
	brickHeight		; Height of the bricks in the wall
	brickColor		; Base color of the bricks
	diffColor		; Amount the RGB values can differ from the base color of the bricks
	RGBnoise		; amount of RGB noise added to the final brick layers
	mortarThick		; Thickness of the mortar in the wall
	mortarColor		; Color of the mortar
	fuzzyMortar		; Mortar fuzziness due to the cubism filter (thanks to mahvin for this tip)
	mortar-size		; size of tiles for the mortar
	mortar-saturation	; saturation of the mortar
	beMortar		; beveled and embossed mortar layer  (thanks to zarnad for this tip)
	flattenYN		; flatten the image
	blMode			; Mode for the brick layer
	seed			; random seed for generating the wall
	)

	(let* (
		(theImage (car (gimp-image-new inWidth inHeight RGB)))
		(brickLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Bricks" 100 blMode)))
		(mortarLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Mortar" 100 NORMAL-MODE)))
		(noiseLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Noise" 100 NORMAL-MODE)))
		(brushRad (/ mortarThick 2.0))
		(mortarbrush (car (gimp-brush-new "Mortar Brush")))

; Range of red coloration for the bricks
		(brickRed (car brickColor))
		(redStart (max (- brickRed diffColor) 0))
		(redEnd (min 255 (+ brickRed diffColor)))
		(redDiff (max 1 (- redEnd redStart)))
		(red 0)

; Range of green coloration for the bricks
		(brickGreen (cadr brickColor))
		(greenStart (max (- brickGreen diffColor) 0))
		(greenEnd (min 255 (+ brickGreen diffColor)))
		(greenDiff (max 1 (- greenEnd greenStart)))
		(green 0)

; Range of blue coloration for the bricks
		(brickBlue (caddr brickColor))
		(blueStart (max (- brickBlue diffColor) 0))
		(blueEnd (min 255 (+ brickBlue diffColor)))
		(blueDiff (max 1 (- blueEnd blueStart)))
		(blue 0)

; Get the random seed number into the layer names
		(frameName "")
		(frameNum 0)

; Get the amount every other row needs to be offset
		(rowOffset (/ brickHeight 2.0))

; Checks for which row is being drawn
		(rowflag 0)
		(rowCheck 0)

; Coordinates for the brick		
		(x1 0)
		(y1 0)
		(x2 inWidth)
		(y2 0)
		(lineArray (make-vector 4 0.0))
		)

; Set the random seed for the wall
		(set! seed (if (number? seed) seed (realtime)))
		(srand seed)

; Add in the layers
		(gimp-image-add-layer theImage noiseLayer 0)
		(gimp-image-add-layer theImage brickLayer 0)
		(gimp-image-add-layer theImage mortarLayer 0)

; Set the colors
		(gimp-context-set-foreground mortarColor)
		(gimp-context-set-background brickColor)

; Generate the brush used to create the mortar stripes
		(gimp-brush-set-shape mortarbrush BRUSH-GENERATED-CIRCLE)
		(gimp-brush-set-radius mortarbrush brushRad)
		(gimp-brush-set-spikes mortarbrush 2)
		(gimp-brush-set-hardness mortarbrush 1)
		(gimp-brush-set-aspect-ratio mortarbrush 1)
		(gimp-brush-set-angle mortarbrush 0)
		(gimp-brush-set-spacing mortarbrush 20)
		(gimp-context-set-brush mortarbrush)

; Start the main loop
		(while (> inHeight y1)
			(begin

; Draw the horizontal line for the mortar
				(gimp-context-set-foreground mortarColor)
				(vector-set! lineArray 0 x1)
				(vector-set! lineArray 1 (+ brickHeight y1))
				(vector-set! lineArray 2 x2)
				(vector-set! lineArray 3 (+ brickHeight y2))
				(gimp-pencil mortarLayer 4 lineArray)

; If we're on an even row, run through the following loop to draw the half-brick on the left side of the picture
				(if (= (fmod rowflag 2) 1)
					(begin
						(set! rowCheck (* brickWidth 0.5))
						(gimp-rect-select theImage 0 y1 rowCheck brickHeight CHANNEL-OP-ADD 0 0)
						(if (= (car (gimp-selection-is-empty theImage)) FALSE)
							(begin
								(set! red (+ redStart (rand redDiff)))
								(set! blue (+ blueStart (rand blueDiff)))
								(set! green (+ greenStart (rand greenDiff)))
								(gimp-context-set-background (list red green blue))
;								(gimp-edit-bucket-fill brickLayer BG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
;								(script-fu-distress-selection theImage brickLayer 127 8 4 2 0 0)
								(gimp-edit-bucket-fill brickLayer BG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
							)
						)
						(gimp-selection-none theImage)
					)
				)

				(vector-set! lineArray 1 y1)

				(while (> inWidth x1)
					(begin
						(vector-set! lineArray 0 (+ x1 rowCheck))
						(vector-set! lineArray 2 (+ x1 rowCheck))
						(gimp-pencil mortarLayer 4 lineArray)
						(gimp-rect-select theImage (+ x1 rowCheck) y1 brickWidth brickHeight CHANNEL-OP-ADD 0 0)
						(if (= (car (gimp-selection-is-empty theImage)) FALSE)
							(begin
								(set! red (+ redStart (rand redDiff)))
								(set! blue (+ blueStart (rand blueDiff)))
								(set! green (+ greenStart (rand greenDiff)))
								(gimp-context-set-background (list red green blue))
;								(gimp-edit-bucket-fill brickLayer BG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
;								(script-fu-distress-selection theImage brickLayer 127 8 4 2 0 0)
								(gimp-edit-bucket-fill brickLayer BG-BUCKET-FILL NORMAL-MODE 100 255 0 0 0)
							)
						)
						(set! x1 (+ x1 brickWidth))
						(gimp-selection-none theImage)
					)
				)

				(set! y1 (+ y1 brickHeight))
				(set! rowCheck 0.0)
				(set! rowflag (+ 1 rowflag))
				(set! x1 0)
				(set! x2 inWidth)
				(set! y2 y1)
			)
		)

		(gimp-brush-delete mortarbrush)

		(plug-in-solid-noise RUN-NONINTERACTIVE theImage noiseLayer 1 0 (rand 1000000000) 15 2.0 2.0)
		(plug-in-bump-map RUN-NONINTERACTIVE theImage brickLayer noiseLayer 0 45.00 20 0 0 0 0 1 0 0)
		(plug-in-bump-map RUN-NONINTERACTIVE theImage brickLayer noiseLayer 0 45.00 20 0 0 0 0 1 0 0)

		(if (> RGBnoise 0.0)
			(plug-in-rgb-noise RUN-NONINTERACTIVE theImage brickLayer 0 0 RGBnoise RGBnoise RGBnoise 0.0)
		)

		(plug-in-solid-noise RUN-NONINTERACTIVE theImage noiseLayer 1 0 (rand 1000000000) 15 2.0 2.0)

		(plug-in-bump-map RUN-NONINTERACTIVE theImage mortarLayer noiseLayer 0 45.00 20 0 0 0 0 1 0 0)

		(if (= fuzzyMortar TRUE)
			(plug-in-cubism RUN-NONINTERACTIVE theImage mortarLayer mortar-size mortar-saturation 1)
		)

		(if (= beMortar TRUE)
			(script-fu-layerfx-wall-bevel-emboss theImage mortarLayer 1 3 1 5 0 120 30 0 '(255 255 255) 0 75 '(0 0 0) 0 75 0 0 0)
		)

		(set! frameName (string-append "Wall " (number->string seed frameNum)))
		(gimp-drawable-set-name noiseLayer frameName)

		(if (= flattenYN TRUE)
			(gimp-image-flatten theImage)
		)

		(gimp-display-new theImage)
	)
)

(script-fu-register
	"script-fu-brickwall"
	_"_Brick Wall"
	_"Making a wall, one brick at a time.  Thanks to mahvin, zarnad, and froGgy factory for some additions to the script."
	"James Sambrook"
	"GNU General Public License"
	"4 January 2011"
	""
	SF-ADJUSTMENT _"Image Width"		'(500 30 3000 1 50 0 0)
	SF-ADJUSTMENT _"Image Height"		'(500 30 3000 1 50 0 0)
	SF-ADJUSTMENT _"Brick Width"		'(80 3 100 1 5 0 0)
	SF-ADJUSTMENT _"Brick Height"		'(40 3 100 1 5 0 0)
	SF-COLOR      _"Brick Color"		'(178 33 33)
	SF-ADJUSTMENT _"Color Variations"	'(30 0 50 1 5 0 0)
	SF-ADJUSTMENT _"RGB brick noise?"   '(0.10 0 1 0.01 0.1 2 0)
	SF-ADJUSTMENT _"Mortar Thickness"	'(5 1 15 1 5 0 0)
	SF-COLOR      _"Mortar Color"		'(200 200 200)
	SF-TOGGLE     _"Fuzzy mortar?"		TRUE
	SF-ADJUSTMENT _"Mortar Tile Size?"  '(1.0 0.1 100.0 0.1 1 1 0)
	SF-ADJUSTMENT _"Mortar Saturation?" '(2.5 0.1 10.0 0.1 1 1 0)
	SF-TOGGLE     _"Beveled / Embossed Mortar?"	TRUE
	SF-TOGGLE     _"Flatten image?"		FALSE
	SF-OPTION     _"Brick Layer Mode?"	'("Normal"
										  "Dissolve"
										  "Behind"
										  "Multiply"
										  "Screen"
										  "Overlay"
										  "Difference"
										  "Addition"
										  "Subtract"
										  "Darken Only"
										  "Lighten Only"
										  "Hue"
										  "Saturation"
										  "Color"
										  "Value"
										  "Divide"
										  "Dodge"
										  "Burn"
										  "Hard Light"
										  "Soft Light"
										  "Grain Extract"
										  "Grain Merge"
										  "Color Erase"
										  "Erase Mode"
										  "Replace"
										  "Anti-Erase")
	SF-VALUE      _"Random Seed"		"random"
)

(script-fu-menu-register "script-fu-brickwall"
	"<Image>/Filters/SambrookJM/")