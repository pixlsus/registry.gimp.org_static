; Blue-Channel skin "Check Layer"
; Created by Patrick David <patdavid@gmail.com>
;
; No adjustment layers in GIMP, so add some solid color layers,
; subtract them from the image to isolate the blue channel
; automatically adjust the contrast to match the blue channel exactly.
;
; Then work on healing/cloning in the base layer, and the results
; will instantly show up in your "check layer"!
;

(define (script-fu-patdavid-check-layer Image Drawable)

    (let*
        ( ;define vars
          (Red (car (gimp-layer-new-from-drawable Drawable Image)))
          (Green (car (gimp-layer-new-from-drawable Drawable Image)))
		  (Yellow (car (gimp-layer-new-from-drawable Drawable Image)))
          (White (car (gimp-layer-new-from-drawable Drawable Image)))
          (Dodge (car (gimp-layer-new-from-drawable Drawable Image)))

          )

        (gimp-image-undo-group-start Image)

        ;(gimp-drawable-set-name Red "-Red")
        ;(gimp-context-set-foreground '(255 0 0))
        ;(gimp-drawable-fill Red FOREGROUND-FILL)
        ;(gimp-image-insert-layer Image Red 0 -1)
        ;(gimp-layer-set-mode Red SUBTRACT-MODE)

        ;(gimp-drawable-set-name Green "-Green")
        ;(gimp-context-set-foreground '(0 255 0))
        ;(gimp-drawable-fill Green FOREGROUND-FILL)
        ;(gimp-image-insert-layer Image Green 0 -1)
        ;(gimp-layer-set-mode Green SUBTRACT-MODE)
		
        (gimp-drawable-set-name Yellow "-Yellow")
        (gimp-context-set-foreground '(255 255 0))
        (gimp-drawable-fill Yellow FOREGROUND-FILL)
        (gimp-image-insert-layer Image Yellow 0 -1)
        (gimp-layer-set-mode Yellow SUBTRACT-MODE)

        (gimp-drawable-set-name White "-White")
        (gimp-context-set-foreground '(255 255 255))
        (gimp-drawable-fill White FOREGROUND-FILL)
        (gimp-image-insert-layer Image White 0 -1)
        (gimp-layer-set-mode White COLOR-MODE)

        (gimp-drawable-set-name Dodge "-Dodge")
        (gimp-context-set-foreground '(127 127 127))
        (gimp-drawable-fill Dodge FOREGROUND-FILL)
        (gimp-image-insert-layer Image Dodge 0 -1)
        (gimp-layer-set-mode Dodge DODGE-MODE)

        (gimp-image-set-active-layer Image Drawable)


        (gimp-displays-flush)
        (gimp-image-undo-group-end Image)


    )

)

; Finally register our script with script-fu.
(script-fu-register "script-fu-patdavid-check-layer"
                    "Skin Check Layer..."
                    "Generates a blue layer monochrome view for checking skin immperfections"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2013-03-28"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
)

(script-fu-menu-register "script-fu-patdavid-check-layer" "<Image>/Filters/Generic")
