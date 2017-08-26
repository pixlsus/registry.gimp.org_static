; Create Luminosity Masks for an image
; Created by Patrick David <patdavid@gmail.com>
;
; Will isolate light, mid, and dark tones in an image as channel masks
; Adapted from tutorial by Tony Kuyper (originally for PS)
; http://goodlight.us/writing/luminositymasks/luminositymasks-1.html 

(define (script-fu-patdavid-luminosity-masks-2 Image Drawable)

	(let*
		( ; define variables
			(LMask (car (gimp-layer-new-from-drawable Drawable Image)) )
			(L)
            (LL)
            (LLL)
            (D)
            (DD)
            (DDD)
			(iWidth)
			(iHeight)
		)

		;(gimp-image-undo-group-start Image)


		; Set image width and height
		(set! iWidth (car(gimp-drawable-width Drawable)) )
		(set! iHeight (car(gimp-drawable-height Drawable)) )

        ; Create a duplicate of target layer, and desaturate with luminosity
        (gimp-drawable-set-name LMask "Light")
        (gimp-image-add-layer Image LMask 0)
        (gimp-desaturate-full LMask DESATURATE-LUMINOSITY)

		
		; Setup the Channels
		; create them, add them, and make them non-visible
		(set! L (car(gimp-channel-new Image iWidth iHeight "L" 100 '(0 0 0))) )
		(gimp-image-add-channel Image L -1)
		(gimp-drawable-set-visible L FALSE)
				

        ; Transfer this first luminosity level into channel "L"
        (gimp-edit-copy LMask)
        (gimp-floating-sel-anchor (car(gimp-edit-paste L TRUE)))


        ; Select the entire image
        (gimp-selection-all Image)

        ; Subtract the "L" channel from the image (giving us "D")
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT L)
        (gimp-item-set-name (car(gimp-selection-save Image)) "D")
		
        ; Subtract again, DD
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT L)
        (gimp-item-set-name (car(gimp-selection-save Image)) "DD")

        ; Subtract again, DDD
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT L)
        (gimp-item-set-name (car(gimp-selection-save Image)) "DDD")

        (set! D (car(gimp-image-get-channel-by-name Image "D")))
        (set! DD (car(gimp-image-get-channel-by-name Image "DD")))
        (set! DDD (car(gimp-image-get-channel-by-name Image "DDD")))

        ; Activate "L" again
        (gimp-image-select-item Image CHANNEL-OP-REPLACE L)

        ; Subtract "D" from "L", giving us "LL"
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT D )
        (gimp-item-set-name (car(gimp-selection-save Image)) "LL")

        ; Subtract "D" from "LL", giving us "LLL"
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT D )
        (gimp-item-set-name (car(gimp-selection-save Image)) "LLL")

        (set! LL (car(gimp-image-get-channel-by-name Image "LL")))
        (set! LLL (car(gimp-image-get-channel-by-name Image "LLL")))


        ; Now build the mid tone masks
        ; Activate "L" mask first
        (gimp-image-select-item Image CHANNEL-OP-REPLACE L)

        ; And intersect the "D" mask, saving new selection as "M"
        (gimp-image-select-item Image CHANNEL-OP-INTERSECT D)
        (gimp-item-set-name (car(gimp-selection-save Image)) "M")

        ; Select the entire image
        (gimp-selection-all Image)

        ;Subtract "DD" and "LL" to get "MM"
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT LL)
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT DD)
        (gimp-item-set-name (car(gimp-selection-save Image)) "MM")

        ; Select the entire image
        (gimp-selection-all Image)

        ;Subtract "DDD" and "LLL" to get "MMM"
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT LLL)
        (gimp-image-select-item Image CHANNEL-OP-SUBTRACT DDD)
        (gimp-item-set-name (car(gimp-selection-save Image)) "MMM")

        ; Remove temp desat layer
        (gimp-image-remove-layer Image LMask)

        ; De-Select Everything
        (gimp-selection-none Image)

        ; Re-activate original layer
        (gimp-image-set-active-layer Image Drawable)


		;(gimp-image-undo-group-end Image)
		(gimp-displays-flush)

	)

)

; Finally register our script with script-fu.
(script-fu-register "script-fu-patdavid-luminosity-masks-2"
                    "Luminosity Masks (patdavid)..."
                    "Create Luminosity Masks of Layer.\nShould now follow Kuyper's original tutorial nomenclature better"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2013-10-23"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
)

(script-fu-menu-register "script-fu-patdavid-luminosity-masks-2" "<Image>/Filters/Generic")

