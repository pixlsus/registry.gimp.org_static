; time-lapse-0.1
; combine more time-lapse images in more layers into one image via mask
; by Fosfor, 2011, fosfor.software@seznam.cz
; tested with GIMP 2.6.4

(define (script-fu-time-lapse img drawable dithering dith-in-p keogram reverse-layers)
	(let* (
			(layers (gimp-image-get-layers img))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(i 0)
			(width (car (gimp-drawable-width drawable)))
			(height (car (gimp-drawable-height drawable)))
		)
		(gimp-image-undo-group-start img)
		(gimp-context-push)
		(if (= dith-in-p TRUE)
			(set! dithering (* (/ dithering 100) (/ width num-layers)))
		)
		(if (= keogram TRUE)
			(set! dithering 0)
		)
		(while (< i (- num-layers 1))
			(let* (
					(thislayer (aref layer-array i))
				)
				(if (= (car (gimp-layer-is-floating-sel thislayer)) FALSE)
					(let* (
							(masklayer (car (gimp-layer-create-mask thislayer ADD-WHITE-MASK)))
							(grad (* (/ width num-layers) (- num-layers i 1)))
							(grad-start (+ grad (/ dithering 2)))
							(grad-end (- grad (/ dithering 2)))
						)
						(if (= reverse-layers TRUE)
							(begin
								(set! grad (- width grad))
								(set! grad-start (- grad (/ dithering 2)))
								(set! grad-end (+ grad (/ dithering 2)))
							)
						)
						(gimp-layer-add-mask thislayer masklayer)
						(gimp-context-set-foreground '(255 255 255))
						(gimp-context-set-background '(0 0 0))
						(if (= dithering 0)
							(let* (
									(rect-x (if (= reverse-layers TRUE) grad 0))
									(rect-w (if (= reverse-layers TRUE) (- width grad) grad)
									)
								)
								(gimp-rect-select img
									rect-x 0 rect-w height
									CHANNEL-OP-REPLACE FALSE 0 
								)
								(gimp-edit-bucket-fill masklayer
									BG-BUCKET-FILL NORMAL-MODE 100
									0 FALSE 0 0
								)
								(gimp-selection-none img)
							)
							(gimp-edit-blend masklayer
								FG-BG-RGB-MODE NORMAL-MODE
								GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE
								FALSE 1 0 TRUE
								grad-start 0 grad-end 0
							)
						)
					)
					(gimp-layer-set-edit-mask thislayer FALSE)
				)
			)
			(set! i (+ i 1))
		)
		(gimp-context-pop)
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
	)
)

(if (defined? 'script-fu-menu-register (the-environment))
	(begin 
		(script-fu-register "script-fu-time-lapse"
			_"Make time-lapse..."
			_"Combine up layers into time-lapse image"
			"Fosfor"
			"Fosfor, (c) 2011"
			"Januar 2011"
			"*"
			SF-IMAGE       "Image"                    0
			SF-DRAWABLE    "Drawable"                 0
			SF-ADJUSTMENT _"Dithering (pixels)"       '(10 0 100 1 10 0 1)
			SF-TOGGLE     _"Dithering in % of stripe" FALSE
			SF-TOGGLE     _"Keogram (dithering = 0)"  FALSE
			SF-TOGGLE     _"Reverse layers"           FALSE
		)
		(script-fu-menu-register "script-fu-time-lapse" _"<Image>/Filters/Combine")
	)

	(script-fu-register "script-fu-time-lapse"
		_"<Image>/Filters/Combine/Make time-lapse..."
		_"Combine up layers into time-lapse image"
		"Fosfor"
		"Fosfor, (c) 2011"
		"Januar 2011"
		"*"
		SF-IMAGE       "Image"                    0
		SF-DRAWABLE    "Drawable"                 0
		SF-ADJUSTMENT _"Dithering (pixels)"       '(10 0 100 1 10 0 1)
		SF-TOGGLE     _"Dithering in % of stripe" FALSE
		SF-TOGGLE     _"Keogram (dithering = 0)"  FALSE
		SF-TOGGLE     _"Reverse layers"           FALSE
	)
)
 