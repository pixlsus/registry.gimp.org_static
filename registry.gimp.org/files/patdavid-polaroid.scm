; Expired Polaroid 600 Film Effect
; Created by Patrick David <patdavid@gmail.com>
;
; This file is licensed under CC-BY-SA
; Creative Commons by Attribution Share Alike
; Basically - do whatever you want with it.  Just keep my name in it somwewhere
; and whatever you make, share it with the same license.
;
; Oh, and if you are reading this - this is NOT a good script to build off of.
; I am insane, and my code shows it.   Sorry.
;
; Emulate the color and non-developed portions of a Polaroid 600 instant print
;

(define (script-fu-patdavid-expired-polaroid Image Drawable)

	(define (RandDim inHeight)
		(cond
			; Choose height to be up to 100% of image height
			( (eqv? inHeight 'height) (* (car(gimp-image-height Image)) (* (random 100) 0.01)))
			; Choose width to be up to 50% of image width
			( else (* (car(gimp-image-width Image)) (* (random 50) 0.01)  ))
		)
	)

(let
	(
		(oImage Image)
		(oDrawable Drawable)
	)
		; This is entirely because I am too lazy to clean this code up - so swapping Image with a new one...
		(gimp-context-push)
		(gimp-edit-copy Drawable)
		(set! Image (car(gimp-edit-paste-as-new)) )
		(set! Drawable (car(gimp-image-get-active-drawable Image)) )
		;(gimp-display-new Image) ; don't use this except for debugging - no reason to display...

	(let* 
		( 	; define variables
							;(gimp-edit-copy Drawable)
							;(set! Image (car(gimp-edit-paste-as-new)) )
							;(set! Drawable (car(gimp-image-get-active-drawable Image)) )
							;(gimp-display-new Image)
			
			(MaxWidth (car (gimp-image-width Image))) ; Image width
			(MaxHeight (car (gimp-image-height Image))) ; Image height
			(Colored (car (gimp-layer-new-from-drawable Drawable Image)) ) ; Polaroid color tint layer
			(Run (car (gimp-layer-new-from-drawable Drawable Image)) ) ; Polaroid extra-washed out color layer
			; Create layers for the overbright washed out section, the underlying film color (DarkCorner), and the edge
			(BlownCorner (car (gimp-layer-new Image (car(gimp-image-width Image)) 
													(car(gimp-image-height Image)) 
													RGBA-IMAGE "BlownCorner" 100 ADDITION-MODE) ) )
			(DarkCorner (car (gimp-layer-new Image (car(gimp-image-width Image)) 
													(car(gimp-image-height Image)) 
													RGBA-IMAGE "DarkCorner" 100 NORMAL-MODE) ) )
			(EdgeCorner (car (gimp-layer-new Image (car(gimp-image-width Image)) 
													(car(gimp-image-height Image)) 
													RGBA-IMAGE "EdgeCorner" 100 NORMAL-MODE) ) )

			; Polaroid Color Curve
			(RedCurve #(0 112 31 139 63 153 95 166 191 205 223 219 255 235))
			(GreenCurve #(0 77 31 101 63 115 95 128 159 154 191 167 223 181 255 199))
			(BlueCurve #(0 0 191 124 223 145 255 172))

			; Polaroid washed out color curves
			(RedCurveRun #(0 136 31 150 63 162 95 174 127 186 223 223 255 236))
			(GreenCurveRun #(0 92 31 109 63 124 95 139 127 154 159 170 191 185 223 200 255 217)) ;Polaroid run Color Curve
			(BlueCurveRun #(0 5 31 34 63 58 95 83 127 107 159 131 191 156 223 180 255 208))

			; Left & Right corner selections
			(LeftCorner (vector 0 0 (RandDim 'width) 0 0 (RandDim 'height) ) )
			(RightCorner (vector MaxWidth 0 (- MaxWidth (RandDim 'width)) 0 MaxWidth (RandDim 'height) ))
			
			; Streak of color in the middle selection
			(StreakLeft (* MaxWidth (+ 0.25 (* (random 20) 0.01))))
			(StreakRight (- MaxWidth (* MaxWidth (+ 0.25 (* (random 20) 0.01)))))
			(Streak (vector (* StreakLeft (+ 1 (* (random 11) 0.01))) 0 StreakLeft MaxHeight StreakRight  MaxHeight (* StreakRight (+ 0.9 (* (random 11) 0.01))) 0))
			(Corners)
			(StreakMask (car(gimp-layer-create-mask Colored 1)) )
		)
			(gimp-image-undo-group-start oImage)
			(gimp-image-undo-disable Image)
			(gimp-selection-none Image)
	
			; Copy base layer and adjust color to Polaroid Run Color
			(gimp-drawable-set-name Run "Polaroid Color Run")
			(gimp-image-add-layer Image Run -1)
			(gimp-curves-spline Run HISTOGRAM-RED 14 RedCurveRun)
			(gimp-curves-spline Run HISTOGRAM-GREEN 18 GreenCurveRun)
			(gimp-curves-spline Run HISTOGRAM-BLUE 18 BlueCurveRun)

			; Copy base layer and adjust color to Polaroid Color
			(gimp-drawable-set-name Colored "Polaroid Color")
			(gimp-image-add-layer Image Colored -1)
			(gimp-curves-spline Colored HISTOGRAM-RED 14 RedCurve)
			(gimp-curves-spline Colored HISTOGRAM-GREEN 16 GreenCurve)
			(gimp-curves-spline Colored HISTOGRAM-BLUE 8 BlueCurve)

			; Random streak of color (mask for Color/Run Color)
			(gimp-layer-add-mask Colored StreakMask )
			(gimp-free-select Image 8 Streak CHANNEL-OP-REPLACE TRUE FALSE 0)
			(gimp-selection-feather Image (* MaxWidth 0.02) ) ; Feather by 2% of image width
			(gimp-context-set-foreground '(255 255 255))
			(gimp-bucket-fill StreakMask FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
			(gimp-image-merge-down Image Colored 0) ; MERGE DOWN the color layers

			; Select the corners of the image
			(gimp-free-select Image 6 LeftCorner CHANNEL-OP-REPLACE TRUE FALSE 0)
			(gimp-free-select Image 6 RightCorner CHANNEL-OP-ADD TRUE FALSE 0)

			; Distress corner selections for variability and shape
			(script-fu-distress-selection Image Drawable 66 60 25 17 1 1)
			(gimp-selection-grow Image 150) ; Grow selection to add blown corners fill
			(set! Corners (gimp-selection-save Image))
			(gimp-selection-grow Image (* 1 MaxWidth 0.01) ) ; grow selection 2% of width
									 ; actually, testing 1%
			(gimp-selection-feather Image (* 3 (* MaxWidth 0.01))) ; feather selection 3% of width

			; Add the blown corner layer and fill with white
			(gimp-image-add-layer Image BlownCorner -1)
			(gimp-context-set-foreground '(255 255 255))
			(gimp-edit-bucket-fill BlownCorner FG-BUCKET-FILL NORMAL-MODE 40 0 FALSE 0 0)

			; Add dark corner layer and fill with black
			(gimp-image-add-layer Image DarkCorner -1)
			(gimp-selection-load (vector-ref (cadr(gimp-image-get-channels Image)) 0)) ; load the top channel in our list
			(gimp-context-set-foreground '(0 0 0))
			(gimp-edit-bucket-fill DarkCorner FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)

			; Apply polaroid colors to the dark corners
			(gimp-curves-spline DarkCorner HISTOGRAM-RED 14 RedCurve)
			(gimp-curves-spline DarkCorner HISTOGRAM-GREEN 16 GreenCurve)
			(gimp-curves-spline DarkCorner HISTOGRAM-BLUE 8 BlueCurve)
			(gimp-image-merge-down Image DarkCorner 0)

			; Add dark edge corner layer
			(gimp-image-add-layer Image EdgeCorner -1)
			(if (> MaxWidth 2500)
				(gimp-context-set-brush "Circle Fuzzy (05)")
				(gimp-context-set-brush "Circle Fuzzy (03)") ; Should adjust this to function of image size
			)
			;(gimp-context-set-brush "Circle Fuzzy (05)") ; Should adjust this to function of image size
			(gimp-context-set-opacity 100)
			(gimp-edit-stroke EdgeCorner)
			;(plug-in-gauss-rle2 1 Image EdgeCorner 1 1)

			(gimp-context-set-foreground '(255 255 255))
			(if (> MaxWidth 2500)
				(gimp-context-set-brush "Circle Fuzzy (05)")
				(gimp-context-set-brush "Circle Fuzzy (03)")
			)
			(gimp-context-set-opacity 95)
			(gimp-selection-grow Image 1)
			(gimp-edit-stroke EdgeCorner)

			(if (> MaxWidth 2500)
				(gimp-context-set-brush "Circle Fuzzy (07)")
				(gimp-context-set-brush "Circle Fuzzy (05)")
			)
			(gimp-context-set-opacity 30)
			(gimp-selection-grow Image 2)
			(gimp-edit-stroke EdgeCorner)
			(gimp-image-merge-down Image EdgeCorner 0)

			(gimp-context-set-opacity 100)

	
			
	(gimp-selection-all Image)
	;(gimp-image-add-layer oImage (car(gimp-layer-new-from-drawable (car(gimp-image-merge-visible-layers Image 0)) oImage )) -1)
	(gimp-edit-copy-visible Image)
	(gimp-floating-sel-to-layer (car(gimp-edit-paste oDrawable TRUE)) )
	(gimp-drawable-set-name (car(gimp-image-get-active-layer oImage)) "Expired Polaroid")
			
			(gimp-displays-flush)
	
			(gimp-image-undo-enable Image)
			(gimp-image-undo-group-end oImage)

	;(gimp-display-delete Image) ; Only makes sense if I display the scratch image
	(gimp-image-delete Image) ; Only works if I haven't made a display...

	)

(gimp-context-pop)
) ; TESTING - because i am lazy, wrapping another let so this will work with selections in base image...
)

; Finally register our script with script-fu.
(script-fu-register "script-fu-patdavid-expired-polaroid"
                    "Polaroid 600 Expired..."
                    "Will color tone for expired Polaroid 600 instant film, and add non-developed bleed corners"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2011-06-11"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
			;SF-OPTION "Mute Method" '("Luminosity Layers" "Solid Layers with Masks" "Desaturated Overlay" )
)

(script-fu-menu-register "script-fu-patdavid-expired-polaroid" "<Image>/Colors")
