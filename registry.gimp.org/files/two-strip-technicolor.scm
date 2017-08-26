; Two-Strip Technicolor effect
; From www.gimpforphotos.com
; Adapted to script-fu by Patrick David <patdavid@gmail.com>
;
;

(define (script-fu-two-strip-technicolor Image Drawable MergeDown Normalize)


	(let* 
		( 	; define variables
			(RedColor '(255 0 0))
			(CyanColor '(0 255 255))
			(RedLayer (car (gimp-layer-copy Drawable TRUE)) )
			(CyanLayer (car (gimp-layer-copy Drawable TRUE)) )
			(RedFill (car (gimp-layer-copy Drawable TRUE)) )
			(CyanFill (car (gimp-layer-copy Drawable TRUE)) )
			(Final)
		)
			(gimp-image-undo-group-start Image)

			(gimp-image-add-layer Image CyanLayer -1)
			(gimp-context-set-foreground CyanColor)
			(gimp-drawable-fill CyanFill FOREGROUND-FILL)
			(gimp-layer-set-mode CyanFill MULTIPLY-MODE)
			(gimp-image-add-layer Image CyanFill -1)
			(set! CyanLayer (car (gimp-image-merge-down Image CyanFill 0)) )
			(gimp-desaturate-full CyanLayer DESATURATE-LIGHTNESS)
			(gimp-colorize CyanLayer 180 50 0)

			(gimp-image-add-layer Image RedLayer -1)
			(gimp-context-set-foreground RedColor)
			(gimp-drawable-fill RedFill FOREGROUND-FILL)
			(gimp-layer-set-mode RedFill MULTIPLY-MODE)
			(gimp-image-add-layer Image RedFill -1)
			(set! RedLayer (car (gimp-image-merge-down Image RedFill 0)) )
			(gimp-layer-set-mode RedLayer DIFFERENCE-MODE)

			(if (= MergeDown TRUE)
				(begin 
					(gimp-drawable-set-visible Drawable FALSE)
					(set! Final (car (gimp-image-merge-visible-layers Image 0)) )
					(gimp-drawable-set-name Final "Two-strip Technicolor")
					(if (= Normalize TRUE)
						(plug-in-normalize 1 Image Final)
					)
					(gimp-drawable-set-visible Drawable TRUE)
				)
			)
			
			
			(gimp-displays-flush)
	
			(gimp-image-undo-group-end Image)
	)
)

; Finally register our script with script-fu.
(script-fu-register "script-fu-two-strip-technicolor"
                    "Two-strip Technicolor..."
                    "Apply technicolor effect from gimpforphotos.com"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2011-06-11"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
			;SF-OPTION "Mute Method" '("Luminosity Layers" "Solid Layers with Masks" "Desaturated Overlay" )
			SF-TOGGLE "Merge Down" TRUE
			SF-TOGGLE "Normalize when done (only applicable when merged down)" TRUE
)

(script-fu-menu-register "script-fu-two-strip-technicolor" "<Image>/Colors")
