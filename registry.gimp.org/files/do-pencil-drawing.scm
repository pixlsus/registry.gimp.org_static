 (define (do-pencil-drawing filename
                            sfBlurRadius
                            sfStrength)

   (let* ((sfImage (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
          (sfDrawable (car (gimp-image-get-active-layer sfImage)))
          (sfMergeLayers 1))

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
            
            (gimp-image-flatten sfImage)
        )
    )
    (gimp-context-pop)
    (gimp-displays-flush)
    (gimp-image-undo-group-end sfImage)

	 (set! sfDrawable (car (gimp-image-get-active-layer sfImage)))
     (gimp-file-save RUN-NONINTERACTIVE sfImage sfDrawable filename filename)
     (gimp-image-delete sfImage)))
