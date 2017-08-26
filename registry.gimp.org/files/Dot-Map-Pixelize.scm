(script-fu-register
            "script-fu-pixelraster"                        ;func name
            "Brush-Dot-Pixelize"                                  ;menu label
            "Creates a color-averaged Dot-Map with a selected Brush\
              and a matching masked copy of the active layer."              ;description
            "Zed Gecko"                             ;author
            "2011, as free as possible"        ;copyright notice
            "June, 2011"                          ;date created
            "RGB"                     ;image type that the script works on
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Layer" 0
            SF-BRUSH "Brush" '("Circle (03)" 100 44 0)
            SF-ADJUSTMENT  "Number of colums" '(10 2 500 1 1 0 0)
            SF-ADJUSTMENT  "Number of rows" '(10 2 500 1 1 0 0)
  )
  (script-fu-menu-register "script-fu-pixelraster" "<Image>/Filters/Pixelize")


(define (script-fu-pixelraster inImage inLayer inBrush inColums inRows)
  (let*
      (
      	(theLayerA
                  (car
                      (gimp-layer-new
                        inImage
                        (car (gimp-image-width inImage))
                        (car (gimp-image-height inImage))
                        RGB-IMAGE
                        "dot-layer"
                        60
                        NORMAL
                      )
                  )
        )
        (theLayerB 
                   (car
                       (gimp-layer-copy inLayer 0)
                   )
        )
        (theMaskB 
                 (car 
                     (gimp-layer-create-mask 
                        theLayerB 
                        1
                     )
                 ) 
        )
        (theXspacing
                    (/
                      (car (gimp-image-width inImage))
                      inColums
                    )
        )
        (theYspacing
                    (/
                      (car (gimp-image-height inImage))
                      inRows
                    )
        )
        (theXposition)
        (theYposition)
        (counterX 0)
        (counterY 0)

        
      ) ;----------Variablendeklaration Ende

      (gimp-image-undo-group-start inImage)
      (gimp-image-add-layer inImage theLayerB -1)
      (gimp-image-add-layer inImage theLayerA -1)
      (gimp-layer-add-mask theLayerB theMaskB)
      (gimp-layer-add-alpha theLayerA)
      (gimp-layer-add-alpha theLayerB)
      (gimp-edit-clear theLayerA)
      (gimp-drawable-set-name theLayerB "dot-BG-layer")
      (gimp-context-set-brush (car inBrush))

      (while (< counterX inColums)
        (while (< counterY inRows)
          (set! theXposition (+ (/ theXspacing 2) (* theXspacing counterX)))
          (set! theYposition (+ (/ theYspacing 2) (* theYspacing counterY)))
          
          (gimp-palette-set-foreground (car (gimp-image-pick-color inImage inLayer theXposition theYposition 0 1 (/ theXspacing 2))))
          (gimp-paintbrush-default theLayerA 2 (my-float-array theXposition theYposition))
          
          (gimp-palette-set-foreground '(255 255 255))
          (gimp-paintbrush-default theMaskB 2 (my-float-array theXposition theYposition))
          (set! counterY (+ counterY 1))
        )
        (set! counterX (+ counterX 1))
        (set! counterY 0)
        (gimp-progress-update (/ counterX inColums))
     )
     (gimp-drawable-update theLayerA 0 0 (car (gimp-image-width inImage))(car (gimp-image-height inImage)))
     (gimp-drawable-update theMaskB 0 0 (car (gimp-image-width inImage))(car (gimp-image-height inImage)))
     (gimp-drawable-update theLayerB 0 0 (car (gimp-image-width inImage))(car (gimp-image-height inImage)))
     (gimp-image-undo-group-end inImage)
  )
)

(define my-float-array
  (lambda stuff
    (letrec ((kernel (lambda (array pos remainder)
                        (if (null? remainder) array
                            (begin
                              (aset array pos (car remainder))
                              (kernel array (+ pos 1) (cdr remainder)))))))
      (kernel (cons-array (length stuff) 'double) 0 stuff))))