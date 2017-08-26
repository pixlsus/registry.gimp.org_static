; Copyright 2009-2011 Kristian Järventaus
; version 2.0 of my paper texture script, renamed Parchment Texture Script
; Licensed under Creative Commons Attribution Share-alike license 3.0
; Disclaimer: I've studied programming since I was a little kid, mostly reading books and maybe doing a small program in whatever flavour of language I was interested in...
; The disclaimer part is this: even though I'd study a language (ranging from Basic to Pascal in school to Inform 6 which was a C-syntax-y Z-engine language, and I'm Pythoning at the moment)
; I never used them to actually program, so I have very, very little actual experience. The results can be seen below. I know, I know. If I could bother with it, I'd encapsulate
; all the steps into separate functions, but eh.
; If you find a bug (and you will), please contact me.



(define (script-fu-parchment-texture inImage inLayer inFlatten inContrast inColour inCartoon inCartoonStr inBiteStrength inBiteGamma inLava)
	(let		((theImage 0) ; the target image
			(originalLayer 0) ; original layer, a copy because i couldn't be bothere to change all the code that uses "theLayer" (which is Plugin>Cartoon'd)
			(blurredLayer 0) ; blurred original layer
			(theLayer 0) ; the base layer, the original image's copy
			(theScrunch 0) ; embossed noise layer used to make poxy texture
			(theBite 0) ; white spot layer used to make printing faults
			(biteMask 0)
			(multiYella 0) ; yellowing (or any colour) layer with multiply
			(colorYella 0) ; yellowing layer with coloring
			(theVertical 0) ; vertical fibres
			(theHorizontal 0) ; horizontal fibres
			(thePlasma 0) ; badgering layer on top
			(theGrungeLayer 0) ; aging layer

			(lavaLayer 0) ; layer with squiggly lines
			(edgeMask 0) ; layer for the the edge grunge layer
			(secondGrungeLayer 0) ; light grunge through the whole image
			(theGrungeMask 0) ; mask of the secondary grunge layer filled with plasma, and used as the base of the grunge layer drawing
			(speckleLayer 0) ; paper or skin speckles you see on old parchments. I think they're birth marks of the animal.
			(theWidth 0) ; width of the image
			(theHeight 0) ; height of the image
			(theContrast 4.1) ; i don't remember what this was for, so it can't be that important
			(thatDamnFloater 0) ; sounds like this one has some sort of history but i can't remember what it was; comment added at a much later update
			(theIterator 0) ; should just have used local blocks with i
			(perimeter 0) ; this is the perimeter of the image
			(area 0) ; area of the image
			(inFibreOpacity 15)
			(randomPoints (cons-array 2 'double))) ; should have use a local (let block for these, an old-style array ("vector" in moderner scheme dialects) for x,y coordinates
			; note the un-scheme-like variable names. should just have use dashes.




		
		(gimp-image-undo-group-start inImage) ; the undo block
		(set! theImage (car (gimp-image-duplicate inImage)))
		(set! theLayer (car (gimp-image-flatten theImage)))
		(if (not (= (car (gimp-image-base-type theImage)) 0))
			(gimp-image-convert-rgb theImage))
		(gimp-layer-set-name theLayer "Cartooned layer copy")
		(set! theWidth (car (gimp-image-width theImage)))
		(set! theHeight (car (gimp-image-height theImage)))
		(set! area (* theWidth theHeight))
		(set! perimeter (* 2 (+ theWidth theHeight)))	
		
		
		(set! originalLayer (car (gimp-layer-copy theLayer FALSE)))
		(gimp-layer-set-name originalLayer "Original background layer")
		(set! blurredLayer (car (gimp-layer-copy theLayer FALSE)))
		(gimp-layer-set-name blurredLayer "Blurred layer copy")
		(gimp-image-add-layer theImage originalLayer -1)
		(gimp-layer-set-opacity originalLayer 70)
		
		(gimp-image-add-layer theImage blurredLayer -1)
		(gimp-layer-set-opacity blurredLayer 40)		
		(plug-in-blur 1 theImage blurredLayer)
		

		(if (= inCartoon TRUE) ; the "enlivening" procress, toggle widget
			(begin
			  (plug-in-cartoon 1 theImage theLayer inCartoonStr 0.0)      ; cartoon makes for a nice "print" look
			  (plug-in-sharpen 1 theImage theLayer 30)        ; sharpening takes just a teensy bit off the edges of letter forms
  			(gimp-image-set-active-layer theImage blurredLayer)	          
			  (set! theBite (car 	                    ; the white splotches, representing sloppy printing and wear-and-tear
				         (gimp-layer-new
				          theImage
				          theWidth
				          theHeight
				          RGB-IMAGE
				          "White bite (control with Color > Level)"
				          60       ;"Bite opacity", the layer opacity
				          ADDITION-MODE)))
			  
			  (gimp-image-add-layer theImage theBite -1)
			  (gimp-context-set-background '(0 0 0))
			  (gimp-edit-bucket-fill theBite 1 0 100 0 FALSE TRUE 0)      ; is there a simpler way to do this?
			  (plug-in-hsv-noise 1 theImage theBite 1 0 0 255)    ; white noise
			  (plug-in-blur 1 theImage theBite)       ; blur so that the Levels tool has something to work with
			  (gimp-levels    theBite
				          0 ;channel: value
				          0 ;black input
				          (- 160 inBiteStrength)          ; white input, "Bite strength". the dialog lets you choose between 0 and 20, which is then here adjusted with MATHS
				          (/ (+ 20 inBiteGamma) 100)          ; gamma, "Bite spread", also in the dialog.
				          0 ; intensity black, default
				          255)
		          (set! biteMask (car(gimp-layer-create-mask theBite 0)))
			  (gimp-layer-add-mask theBite biteMask)
			  (plug-in-plasma 1 theImage biteMask (random 10000) 7.0)  ))


		(gimp-image-set-active-layer theImage blurredLayer)
		(gimp-image-set-active-layer theImage theBite)	          
          	(set! speckleLayer (car (gimp-layer-new theImage
          						theWidth
          						theHeight
          						RGB-IMAGE
          						"Parchment speckles (remove excess)"
          						60
          						NORMAL-MODE)))
		(gimp-drawable-fill speckleLayer WHITE-FILL)
		(gimp-image-add-layer theImage speckleLayer -1)
		
		(let* ((speckler 0)
			(i 0))
			
			(if (= (car (gimp-brushes-get-list "^speckler$")) 0)
				(set! speckler (car (gimp-brush-new "speckler")))
				(set! speckler "speckler"))
			
			(gimp-brush-set-hardness speckler 0.60)
			(gimp-brush-set-radius speckler 3.0)
			(gimp-context-set-foreground '(0 0 0))
			(gimp-context-set-brush speckler)
			(gimp-by-color-select originalLayer '(255 255 255) 15 0 TRUE FALSE 0 FALSE)
			(while (< i (/ area 50000))
				(gimp-paintbrush-default speckleLayer 2 (vector (random theWidth) (random theHeight)))
				(gimp-brush-set-radius speckler (/ (+ 10 (random 20)) 10))
				(set! i (+ i 1)))
			(gimp-selection-none theImage))
	    	
	    	(plug-in-colortoalpha 1 theImage speckleLayer '(255 255 255))
	    	
	    	(if (= inLava TRUE)
	    		(begin
			    	(set! lavaLayer (car (gimp-layer-new theImage
			    						theWidth
			    						theHeight
			    						RGB-IMAGE
			    						"Lava layer for squiggly lines"
			    						12
			    						NORMAL-MODE)))
				(gimp-image-add-layer theImage lavaLayer -1)
				;(print "before lava")
				(gimp-selection-all theImage)
				(script-fu-lava theImage lavaLayer (random 100000) 10 7 "Default" FALSE FALSE FALSE)
				(gimp-selection-none theImage)
				(gimp-invert lavaLayer)
				(plug-in-colortoalpha 1 theImage lavaLayer '(255 255 255))))
	    	
				          
		(set! theGrungeLayer (car (gimp-layer-new theImage
							theWidth
							theHeight
							RGB-IMAGE
							"Edge grunge (edit mask with Blend)"
							100
							GRAIN-MERGE-MODE)))
							
		(gimp-drawable-fill theGrungeLayer WHITE-FILL)
		(gimp-image-add-layer theImage theGrungeLayer -1)
		(set! edgeMask (car(gimp-layer-create-mask theGrungeLayer 0)))
		(gimp-layer-add-mask theGrungeLayer edgeMask)

		(gimp-context-set-foreground '(0 0 0))
		(gimp-context-set-background '(255 255 255))
		(gimp-context-set-opacity 100)
		(gimp-edit-blend edgeMask 0 0 2 100 60 0 FALSE FALSE 1 0 TRUE (/ theWidth 2) (/ theHeight 2) 0 0)
		
		(gimp-context-set-brush "Galaxy, Big")
		

	
		(if (and (>= theWidth 100) (>= theHeight 100))
			(begin
				(gimp-round-rect-select theImage 40 40 (- theWidth 80) (- theHeight 80) 200 200 0 FALSE TRUE 50 50)
		
				(let* 		((i1 0)
						(i2 0)
						(pointList '())
						(randomColor 0)
						(xy (make-vector 2 'double)))
			
					(while (< i1 (/ perimeter 5))
						(let* ((sector 0)
							(margin 70))
							(set! sector (random 4))
							;random xy-coordinates, but not in the center, faster this way than rejecting random coordinates from the whole area,
							;especially as the image becomes bigger
							(cond ((= sector 0) (begin
										(vector-set! xy 0 (random theWidth))
										(vector-set! xy 1 (random margin))))
								((= sector 1) (begin
										(vector-set! xy 0 (random theWidth))
										(vector-set! xy 1 (+ (random margin) (- theHeight margin)))))
								((= sector 2) (begin
										(vector-set! xy 0 (random margin))
										(vector-set! xy 1 (random theHeight))))
								((= sector 3) (begin
										(vector-set! xy 0 (+ (random margin) (- theWidth margin)))
										(vector-set! xy 1 (random theHeight))))))
					
					    	;(vector-set! xy 0 (random theWidth))
					    	;(vector-set! xy 1 (random theHeight))
					    	(if (< (car (gimp-selection-value theImage (vector-ref xy 0) (vector-ref xy 1))) (random 150))
					    		(begin
						    		(set! i1 (+ i1 1))
						    		;(print i1)
						    		(set! pointList (cons (vector (vector-ref xy 0) (vector-ref xy 1)) pointList)))))
					    	
					(gimp-selection-none theImage)
		
					;(print pointList)
		
				    	(while (< i2 (/ perimeter 10))
					    	(set! randomColor (random 128))
			    			(gimp-context-set-foreground (list randomColor randomColor randomColor))
					    	(gimp-paintbrush-default theGrungeLayer 2 (car pointList))
					    	(set! pointList (cdr pointList))
					    	(set! i2 (+ i2 1))))
					    	
				(gimp-round-rect-select theImage 20 20 (- theWidth 40) (- theHeight 40) 150 150 0 FALSE FALSE 0 0)
				(if (and (> theWidth 200) (> theHeight 200))
					(gimp-round-rect-select theImage 100 100 (- theWidth 200) (- theHeight 200) 150 150 1 FALSE FALSE 0 0))

		
		   		(let* ((i1 0)
					(i2 0)
					(pointList '())
					(xy (make-vector 2 'double)))
			

					(set! pointList (cons (vector 45 45) (cons (vector 45 (- theHeight 45)) (cons (vector (- theWidth 45) 45) (cons (vector (- theWidth 45) (- theHeight 45)) (vector 0) )))))			
				   	(while (< i1 (/ perimeter 20))
					    	(vector-set! xy 0 (random theWidth))
					    	(vector-set! xy 1 (random theHeight))
					    	(if (> (car (gimp-selection-value theImage (vector-ref xy 0) (vector-ref xy 1))) 0)
					    		(begin
						    		(set! i1 (+ i1 1))
						    		;(print i1)
						    		(set! pointList (cons (vector (vector-ref xy 0) (vector-ref xy 1)) pointList)))))
						    	
					(gimp-selection-none theImage)
				
					;(print pointList)
					(gimp-context-set-opacity 70)
					(gimp-context-set-foreground '(255 255 255))
					    (while (< i2 (+ (/ perimeter 20) 4)) ; (the perimeter constant is used to determine how much edge grunge there will be, and the + 4 is for
					    					 ; the set vectors above that are used to cut the corners in.
 					    	(gimp-paintbrush-default theGrungeLayer 2 (car pointList))
					    	(set! pointList (cdr pointList))
					    	(set! i2 (+ i2 1))))
			    	(plug-in-colortoalpha 1 theImage theGrungeLayer '(255 255 255))
			    	(gimp-context-set-opacity 100)))
		
		
		(set! secondGrungeLayer (car (gimp-layer-new 	theImage
								theWidth
								theHeight
								RGB-IMAGE
								"Light grunge (edit mask with Level)"
								20
								NORMAL-MODE)))			    			
		(gimp-drawable-fill secondGrungeLayer WHITE-FILL)
		(gimp-image-add-layer theImage secondGrungeLayer -1)
		(set! theGrungeMask (car(gimp-layer-create-mask secondGrungeLayer 0)))
		(gimp-layer-add-mask secondGrungeLayer theGrungeMask)			
		(plug-in-plasma 1 theImage theGrungeMask (random 10000) 5.0)
		
		(let* 		((i 0)
				(randomColor 0)
				(randomx 0)
				(randomy 0))
			
			(while (< i (/ area 4000))
				(set! randomx (random theWidth))
				(set! randomy (random theHeight))
				;(print (car (gimp-drawable-get-pixel theLayer randomx randomy)))
				;(print (car (gimp-drawable-get-pixel theGrungeMask randomx randomy)))
				;(print (cdr (gimp-drawable-get-pixel theGrungeMask randomx randomy)))
				(if (> (vector-ref (car (cdr (gimp-drawable-get-pixel theGrungeMask randomx randomy))) 0) (random 200))
				    	(begin
					    	(set! randomColor (random 255))
					    	(gimp-context-set-opacity (+ 50(random 50)))
			    			(gimp-context-set-foreground (list randomColor randomColor randomColor))
			    			(gimp-paintbrush-default secondGrungeLayer 2 (vector randomx randomy))
					    	(set! i (+ i 1))))))
	    	
	    	(plug-in-colortoalpha 1 theImage secondGrungeLayer '(255 255 255))
	    	(plug-in-blur 1 theImage theGrungeMask)
	    	
	    
    
		(set! theScrunch (car (gimp-layer-new   theImage        ;the embossed noise layer, poxy, porous look
                                            theWidth
                                            theHeight
                                            RGB-IMAGE
                                            "Pore texture"
                                            5
                                            VALUE-MODE)))
    
		(gimp-drawable-fill theScrunch WHITE-FILL)
		(gimp-image-add-layer theImage theScrunch -1)
		(plug-in-hsv-noise	1 theImage theScrunch 1 0 100 0)
		(plug-in-emboss 1 theImage theScrunch 15 50 5 1)

		(set! multiYella (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGB-IMAGE
                                        "Multiply with age (control with opacity)"
                                        3 ; how yella do ya want it
                                        MULTIPLY-MODE)))
    
		(gimp-image-add-layer theImage multiYella -1)
		(gimp-context-set-background inColour)      ; brown works too.
		(gimp-edit-bucket-fill multiYella 1 0 100 0 FALSE TRUE 0)

		(set! colorYella (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGB-IMAGE
                                        "Colourise with age (control with opacity)"
                                        33 ; 
                                       	COLOR-MODE)))
    
		(gimp-image-add-layer theImage colorYella -1)
		(gimp-context-set-background inColour)     
		(gimp-edit-bucket-fill colorYella 1 0 100 0 FALSE TRUE 0)

		(set! theHorizontal (car (gimp-layer-new
                              theImage
                              theWidth
                              theHeight
                              RGB-IMAGE
                              "Horizontal stripes"
                              inFibreOpacity
                              BURN-MODE)))
		(gimp-drawable-fill theHorizontal WHITE-FILL)
		(gimp-image-add-layer theImage theHorizontal -1)
		(plug-in-randomize-hurl 1 theImage theHorizontal 100 1 TRUE 10)
		(gimp-desaturate theHorizontal)
		(plug-in-wind 1 theImage theHorizontal 5 0 25 0 3)
		(plug-in-blur 1 theImage theHorizontal)
		(gimp-brightness-contrast theHorizontal 0 30)
    
		(gimp-image-rotate theImage 0) ; originally the method was just to duplicate the horizontal layer and rotate the duplicate, but that only works with square images. For ages I tried to figure out how to do rectangular ones, but my attempts at enlarging the images and layers were unsuccesful. And by unsuccessful, I mean buggy, because enlarging those images and layers seems to work, but no plugin works in the "outside" areas and the rotation is centered on the original area, not the new square one, etc. Then lightning struck me, and I felt really stupid, and just did this: rotate the fricking image, do the Vertical layer, then rotate it back.

		(set! theVertical (car (gimp-layer-new
                            theImage
                            theHeight
                            theWidth
                            RGB-IMAGE
                            "Vertical stripes"
                            inFibreOpacity
                            BURN-MODE)))
    
		(gimp-drawable-fill theVertical WHITE-FILL)
		(gimp-image-add-layer theImage theVertical -1)
		(plug-in-randomize-hurl 1 theImage theVertical 100 1 TRUE 10)
		(gimp-desaturate theVertical)
		(plug-in-wind 1 theImage theVertical 5 0 25 0 3)
		(plug-in-blur 1 theImage theVertical)
		(gimp-brightness-contrast theVertical 0 30)
    
		(gimp-image-rotate theImage 2)

		(set! thePlasma (car (gimp-layer-new ; Splotches of dark and light to create an aged look.
				theImage
				theWidth
				theHeight
				RGB-IMAGE
				"Plasma layer (control with opacity)"
				30
				OVERLAY-MODE)))
		(gimp-image-add-layer theImage thePlasma -1)
		;(set! theContrast 4.1)
		(plug-in-plasma 1 theImage thePlasma (random 100000) inContrast) 
		(gimp-desaturate thePlasma)

    
		(if (= inFlatten TRUE)
			(begin (gimp-image-flatten theImage)))
		(gimp-image-clean-all theImage)
		(gimp-display-new theImage)
		(gimp-selection-none inImage)
		(gimp-selection-none theImage)

		(gimp-displays-flush theImage)
		(gimp-image-undo-group-end inImage)))

(script-fu-register "script-fu-parchment-texture"
					_"_Parchment Texture"
					_"Make an image look like it's on old parchment"
					"Kristian Järventaus"
					"2011, Kristian Järventaus."
					"June 25th 2011"
					"RGB* GRAY* INDEX*"
					SF-IMAGE     	 "The image"    0
					SF-DRAWABLE   	"The layer"     0
					SF-TOGGLE		"Flatten"	FALSE
					SF-ADJUSTMENT	"Plasma wear turbulence"	'(4 1 7 1 3 1 0)
					SF-COLOR		" Aging color"	'(255 182 0)
					SF-TOGGLE		"Badger the original image"	TRUE
					SF-ADJUSTMENT	" Print smudge strength (Cartoon plugin)"	'(10 1 20 1 5 1 0)
					SF-ADJUSTMENT  	" Strength of bite"	'(10 0 20 1 5 0 0)
					SF-ADJUSTMENT  	"  Bite spread" '(10 0 20 1 5 0 0)
					SF-TOGGLE	"Squiggly lines? (the Lava plugin takes a lot of time)" FALSE)

(script-fu-menu-register 	"script-fu-parchment-texture"
							"<Image>/Filters/Decor/")
