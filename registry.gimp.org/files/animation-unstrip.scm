; Animation-Unstrip
; Converts an animation strip or sheet into series of layers.

(define (script-fu-animation-unstrip img img-layer frames rows)
	(let* 
		(
			(width (car (gimp-drawable-width img-layer)))
			(height (car (gimp-drawable-height img-layer)))
			(animation (car (gimp-image-new (/ width frames) (/ height rows) RGB)))
			(layer 0)
			(flayer 0)
			(i 0)
			(j 0)
		)
		;(gimp-display-new animation)

		(gimp-selection-none img)
		(while (< j rows) (while (< i frames)
			(gimp-rect-select img (* (/ width frames) i) (* (/ height rows) j)
				(/ width frames) (/ height rows) CHANNEL-OP-ADD FALSE 0)
			(gimp-edit-copy img-layer)
			(gimp-selection-none img)
			
			(set! layer (car (gimp-layer-new animation (/ width frames) (/ height rows)
				RGBA-IMAGE "frame" 100 NORMAL-MODE)))
			(gimp-image-add-layer animation layer 0)
			(set! flayer (car (gimp-edit-paste layer TRUE)))
			(gimp-floating-sel-anchor flayer)
			(set! i (+ i 1))
		) (set! i 0) (set! j (+ j 1)) )

		(gimp-display-new animation)			
	)
)

(script-fu-register "script-fu-animation-unstrip"
	"Unstrip"
	"Generate layer sequence from animation strip."
	"K^2"
	"Public Domain"
	"2012"
	""

	SF-IMAGE 	"Image"		0
	SF-DRAWABLE	"Drawable"	0
	SF-ADJUSTMENT	"Columns"	'(10 1 500 1 10 0 1)
	SF-ADJUSTMENT	"Rows"		'(1  1 500 1 10 0 1)
)

(script-fu-menu-register "script-fu-animation-unstrip" "<Image>/Filters/Animation")
