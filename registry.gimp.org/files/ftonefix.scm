(define (script-fu-tone-fix theImage theLayer lowInt intInterval highInt)

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Auto level base image
    (gimp-levels-stretch theLayer)

    (let*
    (
        (originalImage (car (gimp-layer-copy theLayer 0)))
        (layerCopy)
        (layerCopyMask)
        (layerInt)
        (merged)
    )

    ;start with lowest intensity
    (set! layerInt (/ lowInt 100))
    (gimp-levels theLayer 0 0 255 layerInt 0 255)

    (set! lowInt (+ lowInt intInterval))
    (while (< lowInt highInt)
        ;process layer
        (set! layerCopy (car (gimp-layer-copy originalImage 0)))
        (gimp-image-add-layer theImage layerCopy -1)
        (gimp-levels-stretch layerCopy)
        (set! layerInt (/ lowInt 100))
        (gimp-levels layerCopy 0 0 255 layerInt 0 255)
        (set! layerCopyMask (car (gimp-layer-create-mask layerCopy ADD-COPY-MASK)))
        (gimp-layer-add-mask layerCopy layerCopyMask)
        (gimp-invert layerCopyMask)
        (set! lowInt (+ lowInt intInterval))
        (set! merged (car (gimp-image-merge-down theImage layerCopy 0)))
        (gimp-image-flatten theImage)
    )
    
    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)

    )
)

(script-fu-register "script-fu-tone-fix"
            _"_Tone Fix..."
            "Performs pseudo tone fixing"
            "Fidelito Fernandez"
            ""
            "20080129"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-ADJUSTMENT   _"Lowest Intensity"     '(20 10 100 1 10 1 0)
            SF-ADJUSTMENT   _"Intervals"            '(10 5 30 1 10 1 0)
            SF-ADJUSTMENT   _"Highest Intensity"    '(170 0 300 1 10 1 0))

(script-fu-menu-register "script-fu-tone-fix" "<Image>/Filters/Enhance")