 (script-fu-register
                  "script-fu-scale-legend"                        ;func name
                  "Map Scale Legend"                                  ;menu label
                  "Creates a scale legend for a map. v1.0"              ;description
                  "Alejandro Grijalba sud_NOSPAM@latinsud.com"                             ;author
                  "copyright 2009, Alejandro Grijalba"             ;copyright notice
                  "June 6, 2009"                          ;date created
                  ""                     ;image type that the script works on
                                                              ;a spin-button
                  SF-ADJUSTMENT  "Height"        '(14 1 200 1 5 0 SF-SLIDER)     ;color variable
                  SF-ADJUSTMENT  "Width"        '(200 1 4096 1 20 0 SF-SLIDER)     ;color variable
                  SF-ADJUSTMENT  "Step(px)"        '(50 1 1024 1 10 0 SF-SLIDER)     ;color variable
                  SF-ADJUSTMENT  "Step(unit)"        '(1 1 1024 1 10 0 SF-SLIDER)     ;color variable
                  SF-STRING      "Unit name"         "km"     ;color variable
                  SF-TOGGLE	 "Split bar"        1     ;color variable
                  SF-ADJUSTMENT  "Border Width"        '(1 0 100 1 5 0 SF-SLIDER)     ;color variable
                  SF-COLOR       "Border Color"        '(0 0 0)     ;color variable
                  SF-COLOR       "Color 1"        '(255 255 255)     ;color variable
                  SF-COLOR       "Color 2"        '(0 0 0)     ;color variable
                  SF-FONT        "Font"         "Charter"    ;a font variable
                  SF-ADJUSTMENT  "Font Size"     '(10 1 1000 1 10 0 1)
                  SF-COLOR       "Font Color"        '(0 0 0)     ;color variable
                  SF-COLOR       "Font Shadow Color"        '(255 255 255)     ;color variable
        )

        (script-fu-menu-register "script-fu-scale-legend" "<Image>/Filters/Render")


        (define (script-fu-scale-legend inH inW inStep inSU inU inDouble inB inBC inC1 inC2 inFont inFontSize inFC inFSC)
          (let*
            (
              ; define our local variables
              ; create a new image:
              (theImage
                        (car
                            (gimp-image-new
                              (+ inW inB inB)
                              (+ inH inB inB)
                              RGB
                            )
                        )
              )
		(fondo
                        (car
                            (gimp-layer-new
                              theImage
                              (+ inW inB inB)
                              (+ inH inB inB) 
                              RGB-IMAGE
                              "Frame"
                              100
                              NORMAL
                            )
                        )
              )

		(theLayer
                        (car
                            (gimp-layer-new
                              theImage
                              inW
                              inH
                              RGBA-IMAGE
                              "Zigzag"
                              100
                              NORMAL
                            )
                        )
              )

		(textLayer (car (gimp-layer-new theImage 10 10 RGBA-IMAGE "text" 100 NORMAL) ) )

		(tw)
		(th)
		(oldBG (car (gimp-context-get-foreground)))
		(oldFG (car (gimp-context-get-background)))

	)

	(gimp-image-add-layer theImage fondo -1)
	(gimp-image-add-layer theImage theLayer -1)
	(gimp-image-add-layer theImage textLayer -1)
	(gimp-layer-set-offsets theLayer inB inB)

	(gimp-context-set-foreground inC1)


	; borde
	(gimp-context-set-background inBC)
	(gimp-drawable-fill fondo BACKGROUND-FILL)


	; marcos
	(gimp-context-set-foreground inC1)
	(gimp-context-set-background inC2)
	(marcos1 theImage theLayer 0 (- inW inStep) inStep inB inH inDouble inU inSU 0 inFontSize inFont inFC)

	; add shadow to text layer
	(set! tw (car  (gimp-drawable-width (car (gimp-image-get-active-layer theImage) ) )) )
	(set! th (car  (gimp-drawable-height (car (gimp-image-get-active-layer theImage) ) )) )
	(gimp-layer-resize (car (gimp-image-get-active-layer theImage) ) (+ tw 2) (+ th 2) 1 1)
	(gimp-context-set-foreground inFSC) 
	(let* (
		(shadowLayer (car (gimp-layer-copy (car (gimp-image-get-active-layer theImage) ) TRUE) ))
		)
		(gimp-image-add-layer theImage shadowLayer -1)
		(gimp-image-lower-layer theImage shadowLayer)
		(gimp-image-resize-to-layers theImage)
		(gimp-selection-layer-alpha shadowLayer)
		(gimp-selection-grow theImage 1)
		(gimp-edit-fill shadowLayer FOREGROUND-FILL)
	) 


	; fin
	(gimp-image-resize-to-layers theImage)
	(gimp-image-merge-visible-layers theImage EXPAND-AS-NECESSARY)
	(gimp-context-set-background oldBG)
	(gimp-context-set-foreground oldFG)
	(gimp-selection-none theImage)
        (gimp-display-new theImage)
	(gimp-image-clean-all theImage)
        (list theImage)
          )
        )

; white frame
(define (marcos1 theImage theLayer x inW inStep inB inH inDouble inU inSU steps inFontSize inFont inFC)
	(gimp-rect-select theImage (+ x inB) inB inStep inH CHANNEL-OP-REPLACE FALSE 0)
	(gimp-edit-fill theLayer FOREGROUND-FILL)
	(if (= inDouble 1) 
		(list
		(gimp-rect-select theImage (+ x inB) inB inStep (/ (+ inH 1) 2) CHANNEL-OP-REPLACE FALSE 0)
 		(gimp-edit-fill theLayer BACKGROUND-FILL)
		)
	)

	; add text to the frame
	(let* ((text (car
		(gimp-text-fontname theImage -1 (- (+ x inB) 1) (+ inH inB inB) (string-append (number->string steps) inU ) 0 FALSE inFontSize PIXELS inFont)		)))
		(gimp-text-layer-set-color text inFC)
		(gimp-image-merge-down theImage text EXPAND-AS-NECESSARY)	
	)

	(if (<= x inW)
		(marcos2 theImage theLayer (+ x inStep) inW inStep inB inH inDouble inU inSU (+ steps inSU) inFontSize inFont inFC)
	)
) 


; black frame
(define (marcos2 theImage theLayer x inW inStep inB inH inDouble inU inSU steps inFontSize inFont inFC)
	(gimp-rect-select theImage (+ x inB) inB inStep inH CHANNEL-OP-REPLACE FALSE 0)
 	(gimp-edit-fill theLayer BACKGROUND-FILL)
	(if (= inDouble 1) 
		(list
		(gimp-rect-select theImage (+ x inB) inB inStep (/ (+ inH 1) 2) CHANNEL-OP-REPLACE FALSE 0)
 		(gimp-edit-fill theLayer FOREGROUND-FILL)
		)
	)

	; add text to the frame
	(let* ((text (car
		(gimp-text-fontname theImage -1 (- (+ x inB) 1) (+ inH inB inB) (string-append (number->string steps) inU ) 0 FALSE inFontSize PIXELS inFont)		)))
		(gimp-text-layer-set-color text inFC)
		(gimp-image-merge-down theImage text EXPAND-AS-NECESSARY)	
	)

	(if (<= x inW)
		(marcos1 theImage theLayer (+ x inStep) inW inStep inB inH inDouble inU inSU (+ steps inSU) inFontSize inFont inFC)
	)
)

