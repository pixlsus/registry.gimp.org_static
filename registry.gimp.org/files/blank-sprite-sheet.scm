; Blank Sprite Sheet

; For n sprites of height h and width w with spacing s between each.
; For guides for the above sprites.

; Graham Spiers  Feb 2014

(define (script-fu-sprite-guides image n w h s)
    ; image: the image to work on
    ; n: # sprites
    ; h: sprite height
    ; w: sprite width
    ; s: spacing between sprites
    (let* 
        (   (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (vg w)
            (hg h)
        )
        (gimp-image-undo-group-start image)
        ; Add hguides.
        (while (< hg height)
            (gimp-image-add-hguide image hg)
            (if (and (> s 0) (< (+ hg s) height))
                (gimp-image-add-hguide image (+ s hg))
            )
            (set! hg (+ hg s))
            (set! hg (+ hg h))
        )
        ; Add vguides.
        (while (< vg width)
            (gimp-image-add-vguide image vg)
            (if (and (> s 0) (< (+ vg s) width))
                (gimp-image-add-vguide image (+ s vg))
            )
            (set! vg (+ vg s))
            (set! vg (+ vg w))
        )
        (gimp-image-undo-group-end image)
    )
)

(define (script-fu-blank-sprite-sheet n w h s)
    ; n: # sprites
    ; h: sprite height
    ; w: sprite width
    ; s: spacing between sprites

    ; Figure out the nearest greater power of 2.
    (define (npot n)
        (expt 2 (ceiling (/ (log n) (log 2))))
    )

    ; Calulate a distance with spacing.
    (define (in_px n d s)
        ; n of distance d + (n - 1) of distance s.
        (+ (* n d) (* (- n 1) s))
    )

    ; Squarest rows and columns for our h and w that will fit n sprites.
    (define (squarest h w n s)
        (let*
            (   (r 1)
                (c 1)
            )
            (while (< (* r c) n)
                (if (> (in_px (+ r 1) h s) (in_px (+ c 1) w s))
                    (set! c (+ c 1))
                    (set! r (+ r 1))
                )
            )
            (list r c)
        )
    )

    (let* 
        (   (rc (squarest h w n s)) ;(rows columns)
            (r (car rc)) ; rows
            (c (cadr rc)) ; columns
            (wi (in_px c w s)) ; image width
            (hi (in_px r h s)) ; image height
            (px (npot (max wi hi))) ; length of a side
            (image (car (gimp-image-new px px RGB))) ; our square image
            (layer ; a layer for our image
                (car
                    (gimp-layer-new
                        image
                        px px
                        RGB-IMAGE
                        "Background"
                        100
                        NORMAL-MODE)))
        )
        (gimp-image-undo-disable image)
        (gimp-drawable-fill layer BG-IMAGE-FILL)
        (gimp-image-add-layer image layer 0)
        (script-fu-sprite-guides image n w h s)
        (gimp-display-new image)
        (gimp-image-undo-enable image)
    )
)

(script-fu-register
    "script-fu-sprite-guides"
    "<Image>/Image/Guides/Sprite Sheet Guides..."
    "Make guides for a sprite sheet."
    "Graham Spiers"
    "Graham Spiers"
    "February 2014"
    ""
    SF-IMAGE "Image" 0
    SF-ADJUSTMENT "# Sprites" '(16 0 9999 1 10 0 SF-SPINNER)
    SF-ADJUSTMENT "Width (px)" '(128 0 9999 1 10 0 SF-SPINNER)
    SF-ADJUSTMENT "Height (px)" '(128 0 9999 1 10 0 SF-SPINNER)
    SF-ADJUSTMENT "Spacing (px)" '(4 0 256 1 10 0 SF-SPINNER)
)

(script-fu-register
    "script-fu-blank-sprite-sheet"
    "<Image>/File/Create/Blank Sprite Sheet..."
    "Make a new image for n sprites of a given size."
    "Graham Spiers"
    "Graham Spiers"
    "February 2014"
    ""
    SF-ADJUSTMENT "# Sprites" '(16 0 9999 1 10 0 SF-SPINNER)
    SF-ADJUSTMENT "Width (px)" '(128 0 9999 1 10 0 SF-SPINNER)
    SF-ADJUSTMENT "Height (px)" '(128 0 9999 1 10 0 SF-SPINNER)
    SF-ADJUSTMENT "Spacing (px)" '(4 0 256 1 10 0 SF-SPINNER)
)
