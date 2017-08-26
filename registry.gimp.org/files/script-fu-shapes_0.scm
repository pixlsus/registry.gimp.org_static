(define (degToRad sfDeg)
    (let * (
             (varRadPerigon (* (* (atan 1.0) 4) 2)) ;(atan 1.0) * 4 = Pi; 2 * Pi = Vollwinkel im rad-Winkelsystem
             (varPartOfPerigon (/ 360 sfDeg))       ;Berechne das Verhältnis des angegebenen Winkels zu einem vollen Winkel
           )
           (/ varRadPerigon varPartOfPerigon)       ;Gibt den umgewandelten Winkel zurück
    )
)
(define (funcVectorShape image offsetX offsetY angle centerX centerY autocenter times startTimes)
    (when (= (car (gimp-image-get-active-vectors image)) -1)
        (gimp-message "There is no active path! Please add a path to the image!")
    )
    (gimp-image-add-vectors image
                           (car
                                (gimp-vectors-copy
                                    (car
                                        (gimp-image-get-active-vectors image)
                                    )
                                )
                            )
                           -1)
    (gimp-vectors-stroke-rotate (car(gimp-image-get-active-vectors image))
                                TRUE ; Stroke-ID
                                (if (= autocenter TRUE)
                                    (/
                                        (car
                                            (gimp-image-width image)
                                        )
                                    2)
                                    (centerX)
                                )
                                (if (= autocenter TRUE)
                                    (/
                                        (car
                                            (gimp-image-height image)
                                        )
                                    2)
                                    (centerY)
                                )
                                angle)
    (gimp-vectors-stroke-translate (car(gimp-image-get-active-vectors image))
                                   TRUE
                                   offsetX
                                   offsetY)
    (gimp-progress-update (/ (- startTimes times) startTimes))
    (gimp-progress-set-text (string-append (number->string (/ (- startTimes times) startTimes) " %")))
    (when (> times 1)
        (funcVectorShape image offsetX offsetY angle centerX centerY autocenter (- times 1) startTimes)
    )
)
(define (funcLayerShape image drawable offsetX offsetY angle centerX centerY autocenter times)
    (let* (
            (varActiveLayer drawable)
          )
        (set! varActiveLayer
            (car
                (gimp-layer-new-from-drawable varActiveLayer
                                              image
                )
            )
        )
        (gimp-image-add-layer image
                              varActiveLayer
                              -1)
        (when (<> angle 0)
            (gimp-drawable-transform-rotate-default varActiveLayer ;; The affected drawable
                                                    (degToRad angle) ;; The angle of rotation (radians)
                                                    FALSE ;; Whether to automatically rotate around the selection
                                                    (if (= autocenter TRUE)
                                                      (/
                                                          (car
                                                              (gimp-image-width image)
                                                          )
                                                      2)
                                                      (centerX)
                                                    )
                                                    (if (= autocenter TRUE)
                                                        (/
                                                            (car
                                                                (gimp-image-height image)
                                                            )
                                                        2)
                                                        (centerY)
                                                    )
                                                    TRUE
                                                    0 ;; How to clip results
            )
        )
        (gimp-layer-translate varActiveLayer
                              offsetX
                              offsetY
        )
        (when (> times 1)
            (funcLayerShape image varActiveLayer offsetX offsetY angle centerX centerY autocenter (- times 1))
        )
    )
)
(define (script-fu-shapes sfImage sfDrawable sfPathLayer sfOffsetX sfOffsetY sfAngle sfCenterX sfCenterY sfAutocenter sfTimes)
    (gimp-context-push)
    (gimp-image-undo-group-start sfImage)
    (if (= sfPathLayer 0)
        (funcVectorShape sfImage sfOffsetX sfOffsetY sfAngle sfCenterX sfCenterY sfAutocenter sfTimes sfTimes)
        (funcLayerShape sfImage sfDrawable sfOffsetX sfOffsetY sfAngle sfCenterX sfCenterY sfAutocenter sfTimes)
    )
    (gimp-context-pop)
    (gimp-displays-flush)
    (gimp-image-undo-group-end sfImage)
)
(script-fu-register "script-fu-shapes"
    "Shapes"
    "Duplicates a path/layer as often as you want and move and rotate the dublicated paths/layers."
    "Lukas Stahl"
    "Copyright 2010, Lukas Stahl"
    "08.01.2010"
    "*"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-OPTION "Path/Layer" '("Path" "Layer")
    SF-ADJUSTMENT "Offset X" '(3 -4096 4096 1 10 0 1)
    SF-ADJUSTMENT "Offset Y" '(3 -4096 4096 1 10 0 1)
    SF-ADJUSTMENT "Angle" '(0.5 -180 180 0.1 1 2 0)
    SF-ADJUSTMENT "Center X" '(500 -4096 4096 1 10 0 1)
    SF-ADJUSTMENT "Center Y" '(500 -4096 4096 1 10 0 1)
    SF-TOGGLE "Auto-center" TRUE
    SF-ADJUSTMENT "Times" '(20 1 1000 1 10 0 1)
)
(script-fu-menu-register "script-fu-shapes" "<Image>/Filters")