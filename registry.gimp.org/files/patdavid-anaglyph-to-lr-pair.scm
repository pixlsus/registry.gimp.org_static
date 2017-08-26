; Create Luminosity Masks for an image
; Created by Patrick David <patdavid@gmail.com>
;
; Will isolate light, mid, and dark tones in an image as channel masks
; Adapted from tutorial by Tony Kuyper (originally for PS)
; http://goodlight.us/writing/luminositymasks/luminositymasks-1.html 

(define (script-fu-patdavid-anaglyph-to-pair Image Drawable)

	(let*
		( ; define variables
			(RedLayer (car (gimp-layer-new-from-drawable Drawable Image)) )
			(CyanLayer (car (gimp-layer-new-from-drawable Drawable Image)) )
            (Red (car (gimp-layer-new-from-drawable Drawable Image)) )
            (Cyan (car (gimp-layer-new-from-drawable Drawable Image)) )
            (iWidth)
		)

		(gimp-image-undo-group-start Image)

        ; Get the image width...
        (set! iWidth (car(gimp-drawable-width Drawable)) )

        (gimp-drawable-set-name RedLayer "RedFilter")
        (gimp-drawable-set-name Cyan "Cyan")

        (gimp-context-set-foreground '(255 0 0))
        (gimp-drawable-fill RedLayer 0)
        (gimp-layer-set-mode RedLayer SUBTRACT-MODE)

        (gimp-image-add-layer Image Cyan 0)
        (gimp-image-add-layer Image RedLayer 0)
        (set! Cyan (car (gimp-image-merge-down Image RedLayer 0)) )

        (gimp-desaturate Cyan)
        (plug-in-normalize 1 Image Cyan)


        (gimp-drawable-set-name CyanLayer "CyanFilter")
        (gimp-drawable-set-name Red "Red")

        (gimp-context-set-foreground '(0 255 255))
        (gimp-drawable-fill CyanLayer 0)
        (gimp-layer-set-mode CyanLayer SUBTRACT-MODE)

        (gimp-image-add-layer Image Red 0)
        (gimp-image-add-layer Image CyanLayer 0)
        (set! Red (car (gimp-image-merge-down Image CyanLayer 0)) )

        (gimp-desaturate Red)
        (plug-in-normalize 1 Image Red)

        (gimp-layer-translate Red iWidth 0)
        (gimp-image-resize-to-layers Image)


		(gimp-image-undo-group-end Image)
		(gimp-displays-flush)

	)

)

; Finally register our script with script-fu.
(script-fu-register "script-fu-patdavid-anaglyph-to-pair"
                    "Anaglyph to Stereo..."
                    "Create a LR Stereo Pair (Cross-Eyed) from an Anaglyph Image (Red/Cyan)"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2012-12-12"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
            ;SF-TOGGLE "Generate Cross-View?" TRUE
)

(script-fu-menu-register "script-fu-patdavid-anaglyph-to-pair" "<Image>/Filters/Generic")
