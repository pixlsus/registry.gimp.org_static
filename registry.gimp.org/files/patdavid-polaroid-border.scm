; Polaroid 600 Border
; Created by Patrick David <patdavid@gmail.com>
; http://blog.patdavid.net
;
; Create a facsimile of a Polaroid 600 print border
; Including the texture on 600 prints

(define (script-fu-patdavid-polaroid-border Image Drawable)

	(let*
		( ;define variables
			; Actual image dimensions
			(imWidth (gimp-image-width Image))
			(imHeight (gimp-image-height Image))

			; new Polaroid border dimensions based on 600 film
			(LR-BW (* (car imWidth) 0.072)) ; left & right border width based on 0.072 of img width
			(T-BW (* LR-BW 1.06)) ; top border width based on LR-BW
			(B-BW (* LR-BW 4)) ; bottom border width based on LR-BW
			
			; new dimensions for the image
			(newWidth (+ (car imWidth) (* 2 LR-BW)) )
			(newHeight (+ (car imHeight) (+ T-BW B-BW) ) )

			; Layer to paint the texture of the border on
			(theLayer (car (gimp-layer-new Image newWidth newHeight RGBA-IMAGE "Border" 100 NORMAL-MODE) ) )
			(underLayer (car (gimp-layer-new Image newWidth newHeight RGBA-IMAGE "Border Under" 100 NORMAL-MODE) ) )
			(blackBorderLayer (car (gimp-layer-new Image newWidth newHeight RGBA-IMAGE "Border Shadow" 100 NORMAL-MODE) ) )
			
			; Note: Polaroid 600 dimensions 453 (w) X 467 (h)
			; original image polaroid 600 height based on width...
			(imHeightCropped (* (car imWidth) 1.03))

			; Keep the currently selected brush in memory...
			(oldBrush (car(gimp-context-get-brush)))
			
			; reference image width for scaling patterns
			(refWidth 1818)
			
			; all factors of image width...
			; my test image had width of 1818 px
			;diamond factor: 0.0083
			;diamond spacing factor: 0.088 -- Won't need this, spacing is a % of brush width
			;diamond dy factor: 0.007
			;row x offset factor: 0.01265
			(yOffset (inexact->exact (round (* (car imWidth) 0.007))) )
			(xOffset (inexact->exact (round (* (car imWidth) 0.01265))) )
			(radius (inexact->exact (round (* (car imWidth) 0.0083))) )

			
		)

		(gimp-image-undo-group-start Image)
		; First, resize the image to match polaroid 600 dimensions
		; original layer will still be available, but will be visibly cropped to dims
		(gimp-image-resize Image newWidth (+ imHeightCropped (+ T-BW B-BW)) LR-BW T-BW)

		; setup the brush
		(gimp-brush-new "Polaroid Diamond")
		(gimp-brush-set-shape "Polaroid Diamond" BRUSH-GENERATED-DIAMOND)
		(gimp-brush-set-radius "Polaroid Diamond" radius) ; originally 15
		(gimp-brush-set-spacing "Polaroid Diamond" 160) ; originally 160
		(gimp-brush-set-aspect-ratio "Polaroid Diamond" 2)
		(gimp-brush-set-hardness "Polaroid Diamond" 0.8)

		; setup the layer
		(gimp-image-add-layer Image blackBorderLayer -1)
		(gimp-image-add-layer Image underLayer -1)
		(gimp-image-add-layer Image theLayer -1)
		(gimp-drawable-fill theLayer WHITE-FILL)
		(gimp-drawable-fill underLayer WHITE-FILL)
		(gimp-selection-none Image)
		;(gimp-context-set-foreground '(255 255 255))
		(gimp-selection-all Image)
		;(gimp-bucket-fill theLayer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)

		(gimp-context-set-foreground '(0 0 0) )
		(gimp-context-set-brush "Polaroid Diamond")
		(gimp-image-undo-freeze Image)
		; This function will fill with lines offset every other for diamond pattern.
		(define (doLine y ymax dy)
			(if (<= y ymax)
				(begin
					;======
					(if (even? y)
						(gimp-paintbrush theLayer 0 4 (vector xOffset y newWidth y) PAINT-CONSTANT 0)
						(gimp-paintbrush theLayer 0 4 (vector 0 y newWidth y) PAINT-CONSTANT 0)
					)
					;======
					(doLine (+ y dy) ymax dy)
				)
			)
		)

		(if (even? yOffset)
			  (set! yOffset (+ yOffset 1))
		)

		;(doLine 0 newHeight 13) ; yOffset value was originally 13
		(doLine 0 newHeight yOffset)

		(gimp-image-undo-thaw Image)

		; Select all of the dots with a given threshold and feather
		(gimp-by-color-select theLayer '(0 0 0) 15 2 TRUE TRUE 5 FALSE)
	
		; now fill those centers with white
		(gimp-context-set-foreground '(255 255 255) )
		(gimp-selection-translate Image 0 1)
		(gimp-edit-bucket-fill theLayer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
		; set the 'recessed' pattern for another fill
		(gimp-context-set-pattern "recessed")
		(gimp-selection-all Image)
		(gimp-edit-bucket-fill theLayer PATTERN-BUCKET-FILL NORMAL-MODE 20 0 FALSE 0 0)
		(gimp-layer-set-opacity theLayer 10) ; Set this line to fade the dimples!

		; erase borders in image section
		(gimp-rect-select Image LR-BW T-BW (car imWidth) imHeightCropped CHANNEL-OP-REPLACE FALSE 0)
		(gimp-context-set-foreground '(0 0 0))
		(gimp-edit-bucket-fill theLayer FG-BUCKET-FILL 23 100 0 FALSE 0 0)
		(gimp-edit-bucket-fill underLayer FG-BUCKET-FILL 23 100 0 FALSE 0 0)

		; set border shadow layer active, and stroke with 07 fuzzy brush to simulate
		; drop shadow effect
		(gimp-image-set-active-layer Image blackBorderLayer)
		(gimp-context-set-brush "Circle Fuzzy (07)")
		(gimp-edit-stroke blackBorderLayer)

		; now fade the border pattern on the edges of the image
		(gimp-selection-invert Image)
		(gimp-image-set-active-layer Image theLayer)
		(gimp-context-set-foreground '(255 255 255))
		; setup brush parameters
		(gimp-context-set-brush "Polaroid Diamond")
		(gimp-brush-set-radius "Polaroid Diamond" (* (car imWidth) 0.055) )
		(gimp-brush-set-aspect-ratio "Polaroid Diamond" 2)
		(gimp-brush-set-spacing "Polaroid Diamond" (* (car imWidth) 0.0137) )
		(gimp-brush-set-hardness "Polaroid Diamond" 0.5)
		; stroke border selection
		(gimp-edit-stroke theLayer)
		; setup new brush params
		(gimp-brush-set-radius "Polaroid Diamond" (* (car imWidth) 0.11) )
		(gimp-brush-set-aspect-ratio "Polaroid Diamond" 1)
		(gimp-context-set-opacity 50)
		(gimp-brush-set-spacing "Polaroid Diamond" (* (car imWidth) 0.022) )
		(gimp-brush-set-hardness "Polaroid Diamond" 0.5)
		; stroke border again...
		(gimp-edit-stroke theLayer)

		(gimp-layer-add-mask theLayer (car(gimp-layer-create-mask theLayer ADD-WHITE-MASK)))
		(gimp-selection-invert Image)
		(gimp-context-set-foreground '(0 0 0))
		(gimp-edit-fill (car(gimp-layer-get-mask theLayer)) FOREGROUND-FILL)
		
		; clean up
		(gimp-context-set-brush oldBrush)
		;(gimp-brush-delete "Polaroid Diamond")
		(gimp-displays-flush) ; Flush all changes to the display
		(gimp-image-undo-group-end Image)

	)
)

; Finally register our script with script-fu.
(script-fu-register "script-fu-patdavid-polaroid-border"
                    "Polaroid Border"
                    "Create a polaroid border based on image width"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2011-06-17"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
			;SF-OPTION "Mute Method" '("Luminosity Layers" "Solid Layers with Masks" "Desaturated Overlay" )
)

(script-fu-menu-register "script-fu-patdavid-polaroid-border" "<Image>/Colors")
