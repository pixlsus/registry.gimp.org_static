(define (script-fu-pencil-drawing sfImage
                                  sfDrawable
                                  sfBlurRadius
                                  sfStrength
                                  sfMergeLayers)
    (let* (
            (varActiveLayer
                (car
                    (gimp-layer-copy sfDrawable
                                     FALSE) ; Don't add an alpha channel to the layer
                )
            )
            (varCounter 1)
          )

        (gimp-context-push)
        (gimp-image-undo-group-start sfImage)

        (gimp-image-add-layer sfImage
                              varActiveLayer
                              -1 ; Layer position
        )

        (gimp-hue-saturation varActiveLayer
                             0 ; Range of affected hues 0 = ALL-HUES
                             0 ; Hue offset in degrees
                             0 ; Lightness modification
                             -100) ; Saturation modification

        (set! varActiveLayer
            (car
                (gimp-layer-copy varActiveLayer
                                 FALSE)
            )
        )

        (gimp-image-add-layer sfImage
                              varActiveLayer
                              -1)

        (plug-in-gauss-rle2 1 ; non-interactive
                            sfImage
                            varActiveLayer
                            sfBlurRadius ; horizontal radius of gaussian blur
                            sfBlurRadius) ; vertical radius of gaussian blur

        (gimp-invert varActiveLayer)

        (gimp-layer-set-opacity varActiveLayer
                                50) ; opacity

        (set! varActiveLayer
            (car
                (gimp-image-merge-down sfImage
                                       varActiveLayer
                                       0) ; merge-type 0 = EXPAND-AS-NECESSARY
            )
        )

        (set! varActiveLayer
            (car
                (gimp-layer-copy varActiveLayer
                                 FALSE)
            )
        )

        (gimp-image-add-layer sfImage
                              varActiveLayer
                              -1)

        (gimp-layer-set-mode varActiveLayer
                             16) ; 16 = DODGE-MODE

        (set! varActiveLayer
            (car
                (gimp-image-merge-down sfImage
                                       varActiveLayer
                                       0)
            )
        )

        (while (< varCounter sfStrength)
            (set! varActiveLayer
                (car
                    (gimp-layer-copy varActiveLayer
                                     FALSE)
                )
            )

            (gimp-image-add-layer sfImage
                                  varActiveLayer
                                  -1)

           (gimp-layer-set-mode varActiveLayer
                                3) ; 3 = MULTIPLY-MODE)

            (set! varCounter
                (+ varCounter 1)
            )
        )

        (when (= sfMergeLayers TRUE)

            (set! varCounter 2)

            (while (< varCounter sfStrength)
                (set! varActiveLayer
                    (car
                        (gimp-image-merge-down sfImage
                                               varActiveLayer
                                               0)
                    )
                )

                (gimp-layer-set-mode varActiveLayer
                                     3) ; 3 = MULTIPLY-MODE)

                (set! varCounter
                    (+ varCounter 1)
                )
            )
        )
    )
    (gimp-context-pop)
    (gimp-displays-flush)
    (gimp-image-undo-group-end sfImage)
)

(script-fu-register "script-fu-pencil-drawing"
    "Pencil-Drawing"
    "Generate a pencil drawing from a photo."
    "Lukas Stahl"
    "Copyright 2009, Lukas Stahl based on an tutorial from gimpusers.de"
    "04.01.2010"
    "*"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-ADJUSTMENT "Blur Radius" '(4 0 5120 1 10 0 1)
    SF-ADJUSTMENT "Strength" '(1 1 50 1 10 0 0)
    SF-TOGGLE "Merge Layers" TRUE
)
(script-fu-menu-register "script-fu-pencil-drawing" "<Image>/Filters")