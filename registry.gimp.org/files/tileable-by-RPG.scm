
(define (script-fu-tile img drawable blend-x blend-y overlap square)
	(let*
		(
			(sel-float1 0)
			(sel-float2 0)
			(sel-float3 0)
			(sel-float4 0)
			(high-pass-layer 0)
			(yoffset 0)
			(img-w (/ (car (gimp-image-width img)) 2))
			(img-h (/ (car (gimp-image-height img)) 2))
			;(newimg (car (gimp-image-new (- (car (gimp-image-width img)) blend-x) (- (car (gimp-image-height img)) blend-y) RGB)))
		)
		(if (= square 1)
			(set! yoffset (+ (- (* (- img-w img-h) 2) (- blend-x blend-y)) overlap))
			(set! yoffset overlap)
		)
		
		(gimp-undo-push-group-start img)
		
		(gimp-rect-select img 0 0 img-w img-h CHANNEL-OP-REPLACE FALSE 0)
		
		(gimp-edit-copy drawable)
		(set! sel-float1 (car (gimp-edit-paste drawable FALSE)))
		(gimp-floating-sel-to-layer sel-float1)
		(gimp-layer-translate sel-float1 (- (- img-w blend-x) yoffset) (- (- img-h blend-y) overlap))
		
		(gimp-rect-select img img-w 0 img-w img-h CHANNEL-OP-REPLACE FALSE 0)
		
		(gimp-edit-copy drawable)
		(set! sel-float2 (car (gimp-edit-paste drawable FALSE)))
		(gimp-floating-sel-to-layer sel-float2)
		(gimp-layer-translate sel-float2 (- 0 img-w) (- (- img-h blend-y) overlap))
		
		(gimp-rect-select img 0 img-h img-w img-h CHANNEL-OP-REPLACE FALSE 0)
		
		(gimp-edit-copy drawable)
		(set! sel-float3 (car (gimp-edit-paste drawable FALSE)))
		(gimp-floating-sel-to-layer sel-float3)
		(gimp-layer-translate sel-float3 (- (- img-w blend-x) yoffset) (- 0 img-h))
		
		(gimp-rect-select img img-w img-h img-w img-h CHANNEL-OP-REPLACE FALSE 0)
		
		(gimp-edit-copy drawable)
		(set! sel-float4 (car (gimp-edit-paste drawable FALSE)))
		(gimp-floating-sel-to-layer sel-float4)
		(gimp-layer-translate sel-float4 (- 0 img-w) (- 0 img-h))
		
		(gimp-rect-select img (- 0 blend-x) (- 0 blend-x) (+ img-w (/ blend-x 2)) (+ img-h (+ blend-x blend-x)) CHANNEL-OP-REPLACE TRUE (/ blend-x 1.68))
		(gimp-selection-invert img)
		(gimp-edit-clear sel-float4)
		
		(gimp-rect-select img (- 0 blend-y) (- 0 blend-y) (+ img-w (+ blend-y blend-y)) (+ img-h (/ blend-y 2)) CHANNEL-OP-REPLACE TRUE (/ blend-y 1.68))
		(gimp-selection-invert img)
		(gimp-edit-clear sel-float4)
		
		(gimp-rect-select img (- (- img-w (- blend-x blend-y)) yoffset) (- 0 blend-y) (+ img-w (+ blend-y blend-y)) (+ img-h (/ blend-y 2)) CHANNEL-OP-REPLACE TRUE (/ blend-y 1.68))
		(gimp-selection-invert img)
		(gimp-edit-clear sel-float3)
		
		(gimp-rect-select img (- 0 blend-x) (- (- img-h (+ blend-x blend-y)) overlap) (+ img-w (/ blend-x 2)) (+ img-h (+ blend-x blend-x)) CHANNEL-OP-REPLACE TRUE (/ blend-x 1.68))
		(gimp-selection-invert img)
		(gimp-edit-clear sel-float2)
		
		(gimp-selection-none img)
		
		;(gimp-layer-delete drawable)
		
		(gimp-image-crop img (- (- (* img-w 2) blend-x) yoffset) (- (- (* img-h 2) blend-y) overlap) 0 0)
		
			
		
		
		; Complete the undo group
		(gimp-undo-push-group-end img)

		; Flush output
		(gimp-displays-flush)
	)
)

 
 
(script-fu-register "script-fu-tile"
      "<Image>/Filters/Map/Tileable..."
      "Make seamless texture"
      "Pavel aka RPG Roshchin <rpg89@post.ru>"
      "Pavel aka RPG Roshchin"
      "2011"
      "RGB*, GRAY*"
       SF-IMAGE "Image" 0
       SF-DRAWABLE "Layer to blur" 0
       SF-ADJUSTMENT "Blend x" '(100 0 1000 1 10 0 0)
       SF-ADJUSTMENT "Blend y" '(100 0 1000 1 10 0 0)
       SF-ADJUSTMENT "Overlap" '(0 0 1000 1 10 0 0)
       SF-TOGGLE		"Make square texture"	TRUE
       ;SF-ADJUSTMENT "Homogenize brightness (%)" '(70 0 100 1 10 0 0)
       )