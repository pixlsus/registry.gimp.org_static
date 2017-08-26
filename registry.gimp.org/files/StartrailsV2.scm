;Startrails V2.0 23.10.2011

;This script calculates frames to create startrail videos.
;
;Load a  star shot sequence into Gimp and run the script. It will calculate the frames by merging the single images using „lighten only“. Frame one will be a merge of the first two star shots, Frame two of the first three star shots and so on.
;A layer mask can be used to ignore the landscape. It has to be on top of the layer stack.
;There is also an option to reverse the order of the layer stack, if the top layer is the last shot of the sequence instead of the first. This option will call the script „script-fu-reverse-layers“.
;
;This is the first script I wrote. Please report me all bugs/errors you find.
;
;To export the layers to single images I recommend the script <a href="http://registry.gimp.org/node/25394">Export Layers</a>

(define (pk-startrails	Image 
						Draw
						single
						Alpha
						revOrder
						fileName
						fade
						TF
						fadePerc
						startOpac
						
		)
(if(= single TRUE)
(begin
(let*
(
	(layerCount (car	(gimp-image-get-layers Image)))
	(layerArray (cadr	(gimp-image-get-layers Image)))
	(layer)
	(x (- layerCount 1))
	(FF (floor(+ 0.9 (/ TF (/ 1 (/ fadePerc 100))))))
	(SF 0)
)
(gimp-image-undo-group-start Image)
(gimp-progress-set-text "Set Layer-Mode to Lighten Only..")
(gimp-progress-update 0.0)
(set! FF (- FF (floor(/ FF TF))))
(set! SF (- TF FF))

(while (>= x 0)
    (set! layer (aref layerArray x))
	(if (= fade TRUE)
		(begin
			(if (> x (- (- layerCount 1) TF))
				(gimp-layer-set-opacity layer 100)
				(gimp-layer-set-opacity layer (* (/ startOpac (- layerCount TF)) x))
			)
		)		
	)
	(gimp-drawable-set-visible layer TRUE)
	(gimp-layer-set-mode layer 10)
	(set! x(- x 1))
)
(set! layer (car(gimp-layer-new-from-visible Image Image "Final")))
(gimp-image-add-layer Image layer 0)

(set! layerCount (car	(gimp-image-get-layers Image)))
(set! layerArray (cadr	(gimp-image-get-layers Image)))
(set! x 1)
(while (< x layerCount)
    (set! layer (aref layerArray x))
	(gimp-drawable-set-visible layer FALSE)
	(gimp-layer-set-mode layer 0)
	(set! x(+ x 1))
			
)
(gimp-image-undo-group-end Image)
))

(begin


(let*
(
	(ImageOut 	(car	(gimp-image-duplicate Image)))
	(layerCount (car	(gimp-image-get-layers ImageOut)))
	(layerArray (cadr	(gimp-image-get-layers ImageOut)))
	(layer)
	(layer2)
	(layer3)
	(layerAlpha)
	(layerMask)
	(ArrayNr 0)
	(ArrayString)
	(x 0)
	(y 0)
	(z 0)
	(c 0)
	(FF (floor(+ 0.9 (/ TF (/ 1 (/ fadePerc 100))))))
	(SF 0)
)

;-----copy Image
(gimp-image-undo-group-start ImageOut)
(gimp-progress-set-text "Calculating...")
(gimp-progress-update 0.0)
(gimp-display-new ImageOut)
(set! FF (- FF (floor(/ FF TF))))
(set! SF (- TF FF))

;-------Copy Alpha mask to buffer and delete layer
(if (= Alpha TRUE)
	(begin
		(set! layerAlpha(aref layerArray 0))
		(gimp-selection-all ImageOut)
		(gimp-edit-copy layerAlpha)
		(gimp-selection-none ImageOut)
		(gimp-image-remove-layer ImageOut layerAlpha)
		(set! layerCount (car	(gimp-image-get-layers ImageOut)))
		(set! layerArray (cadr	(gimp-image-get-layers ImageOut)))
	)
)	
;-----Reverse order if desired
(if (= revOrder TRUE)
	(begin
		(script-fu-reverse-layers ImageOut Draw)
		(set! layerCount (car	(gimp-image-get-layers ImageOut)))
		(set! layerArray (cadr	(gimp-image-get-layers ImageOut)))
	)
)

;-----Set mode to lighten only/ make invisible-----	
(while (> layerCount 0)
    (set! layerCount (- layerCount 1))
    (set! layer (aref layerArray layerCount))
	(gimp-drawable-set-visible layer FALSE)
	(gimp-layer-set-mode layer 10)
)
(set! layerCount (car	(gimp-image-get-layers ImageOut)))

;-------------------------CODE HERE------------------------
(if (= fade TRUE)
	(begin
		(while (< x layerCount)
			(gimp-progress-update (/ x (- layerCount 1)))
			(if (< (- x FF) 0)	
				(begin
					(set! layer (aref layerArray x))
					(gimp-drawable-set-visible layer TRUE)
					(gimp-layer-set-opacity layer 100)
					(if (= Alpha TRUE)
						(if (> x 0)
							(begin
								(set! layer (aref layerArray (- x 1)))
								(set! layerMask(car(gimp-layer-create-mask layer ADD-WHITE-MASK)))
								(gimp-layer-add-mask layer layerMask)
								(gimp-floating-sel-anchor (car (gimp-edit-paste layerMask TRUE)))
							)
						)
					)
					(set! layer (car(gimp-layer-new-from-visible ImageOut ImageOut (string-append fileName(number->string x)))))
					(gimp-image-add-layer ImageOut layer (+ x layerCount))
					(gimp-drawable-set-visible layer FALSE)
					(set! x (+ x 1))
				)
				
				(begin
					;------------------------------------ 1
					(set! layer (aref layerArray x))
					(gimp-drawable-set-visible layer TRUE)
					(gimp-layer-set-opacity layer 100)
					;------------------------------------ 2
					(if (= Alpha TRUE)
						(if (> x 0)
							(begin
								(set! layer (aref layerArray (- x 1)))
								(set! layerMask(car(gimp-layer-create-mask layer ADD-WHITE-MASK)))
								(gimp-layer-add-mask layer layerMask)
								(gimp-floating-sel-anchor (car (gimp-edit-paste layerMask TRUE)))
							)
						)
					)
					;------------------------------------ 3
					(if (= x (- layerCount 1))
						(begin
							(set! y 0)
							(set! c (- TF 1))
						)
						(begin
							(set! y 0)
							(set! c 1)
						)
					)
					;------------------------------------ 4
					(while (> c y)
						(set! z FF)
						(while (< z TF)
							(if (>= (- x z) 0)
								(begin
									(if(< (+ (- x z) y) (- layerCount 1))
										(begin
											(set! ArrayString (number->string(+ (- x z) y)))
											(set! ArrayNr (string->number ArrayString))
											;(gimp-message-set-handler 0)
											;(gimp-message (string-append "x" (number->string x)))
											(set! layer (aref layerArray ArrayNr))
											(gimp-layer-set-opacity layer (* (/ startOpac SF) (-(- TF z) 1)))
											(if (= (* (/ startOpac SF) (-(- TF z) 1))0)
												(gimp-drawable-set-visible layer FALSE)
											)
										)
									)
								)
							)
							(set! z (+ z 1))						
						)
						(set! layer (car(gimp-layer-new-from-visible ImageOut ImageOut (string-append fileName(number->string (+ x y))))))
						(gimp-image-add-layer ImageOut layer (+ x (+ layerCount y)))
						(gimp-drawable-set-visible layer FALSE)
						(set! y (+ y 1))
					)
					(set! x (+ x 1))
				)
			) 
		)
		(set! x 0)
		(while (< x layerCount)
			(set! layer (aref layerArray x))
			(gimp-image-remove-layer ImageOut layer)
			(set! x(+ x 1))
		)
		(set! layerCount (car	(gimp-image-get-layers ImageOut)))
		(set! layerArray (cadr	(gimp-image-get-layers ImageOut)))
		(set! x 0)
		(while (< x layerCount)
			(set! layer (aref layerArray x))
			(gimp-drawable-set-visible layer TRUE)
			(set! x(+ x 1))
		)
	)
		
	(begin
		(set! layer (aref layerArray x))
		(gimp-drawable-set-visible layer TRUE)
		(gimp-layer-set-opacity layer 100)
		(set! layer2 (car(gimp-layer-new-from-visible ImageOut ImageOut (string-append fileName(number->string x)))))
		(gimp-image-add-layer ImageOut layer2 x)
		(gimp-layer-set-mode layer2 10)
		(set! x 1)
		(gimp-progress-set-text "Calculating...")
		(while (< x layerCount)
			(gimp-image-remove-layer ImageOut layer)
			(set! layer (aref layerArray x))
			(gimp-drawable-set-visible layer TRUE)
			(gimp-layer-set-opacity layer 100)
			
			(if (= Alpha TRUE)
				(begin
					(set! layerMask(car(gimp-layer-create-mask layer2 ADD-WHITE-MASK)))
					(gimp-layer-add-mask layer2 layerMask)
					(gimp-floating-sel-anchor (car (gimp-edit-paste layerMask TRUE)))
				)
			)
			
			(set! layer3 layer2)
			(set! layer2 (car(gimp-layer-new-from-visible ImageOut ImageOut (string-append fileName(number->string x)) )))
			(gimp-image-add-layer ImageOut layer2 x)
			(gimp-layer-set-mode layer2 10)
			(gimp-drawable-set-visible layer3 FALSE)
			(if (= Alpha TRUE)
				(gimp-layer-remove-mask layer3 MASK-DISCARD)
			)
			(set! x (+ x 1))
			(if (= revOrder TRUE)
				(gimp-progress-update (/ (+ 14(* 0.6 x)) (+ 14(* 0.6 layerCount))))
				(gimp-progress-update (/ (+ 6(* 0.6 x)) (+ 6(* 0.6 layerCount))))
			)
		)
		(gimp-image-remove-layer ImageOut layer)
		
		(set! layerCount (car	(gimp-image-get-layers ImageOut)))
		(set! layerArray (cadr	(gimp-image-get-layers ImageOut)))
		(while (> layerCount 0)
			(set! layerCount (- layerCount 1))
			(set! layer (aref layerArray layerCount))
			(gimp-drawable-set-visible layer TRUE)
			(gimp-layer-set-mode layer 0)
		)
	)
)



(gimp-image-undo-group-end ImageOut)
(gimp-displays-flush)
))
))
(script-fu-register
    "pk-startrails"           		             		
    "Startrails"                                  	
    "Creates frames for Startrail videos"				
    "Pascal Klingelhöfer"                         	
    "pascalk89@gmail.com"        						
    "October, 2011"             	             			
    "*"                     								
	SF-IMAGE       	"Input image"           0
    SF-DRAWABLE    	"Input drawable"        0
	SF-TOGGLE		"SingleImage"			TRUE			
	SF-TOGGLE		"Use Alpha Mask\n\t- Sky has to be white\n\t- Mask-Layer has to be on top"	FALSE
	SF-TOGGLE		"Reverse Layer-Order"	FALSE
	SF-STRING		"Basename"				""
	SF-TOGGLE		"Fadeout"				FALSE
	SF-VALUE		"Traillength"			"10"
	SF-ADJUSTMENT	"Fade percentage"		'(50 1 100 1 10 0 0)
	SF-ADJUSTMENT	"Start opacity"			'(100 0 100 1 10 0 0)
	
)
(script-fu-menu-register 	
	"pk-startrails" 
	"<Image>/Filters/Generic"
)