(define (script-fu-initial-image sfImage sfLayerName sfPercentOfTranslation sfNewLayerPosition)

    (gimp-context-push)
    (gimp-image-undo-group-start sfImage)
                          
    (case sfNewLayerPosition
        ((0) (gimp-image-add-layer sfImage ; The image
                                      (car
                                           (gimp-layer-new-from-visible sfImage ; The source image from where the content is copied
                                                                        sfImage ; The destination image to which to add the layer
                                                                        sfLayerName) ; The layer name
                                      )
                                   -1) ; The layer position
                          
            (gimp-layer-add-mask (car
                                     (gimp-image-get-active-layer sfImage)
                                 ) ; The layer to receive the mask
                                 (car (gimp-layer-create-mask (car (gimp-image-get-active-layer sfImage)) ;The layer to which to add the mask
                                                              0))) ; The type of mask
            (gimp-rect-select sfImage
                              0
                              0
                              (car (gimp-image-width sfImage))
                              (car (gimp-image-height sfImage))
                              2
                              FALSE
                              0)
            (gimp-selection-translate sfImage
                                      (* (/ sfPercentOfTranslation 100) (car (gimp-image-width sfImage)))
                                      0)
            (gimp-context-set-foreground "#000000")
            (gimp-edit-fill (car (gimp-layer-get-mask (car (gimp-image-get-active-layer sfImage))))
                            0) ; Foreground fill
            (gimp-selection-none sfImage)
        )
        ((1) (gimp-image-add-layer sfImage ; The image
                                      (car
                                           (gimp-layer-new-from-visible sfImage ; The source image from where the content is copied
                                                                        sfImage ; The destination image to which to add the layer
                                                                        sfLayerName) ; The layer name
                                      )
                                   -1) ; The layer position

            (gimp-layer-add-mask (car
                                     (gimp-image-get-active-layer sfImage)
                                 ) ; The layer to receive the mask
                                 (car (gimp-layer-create-mask (car (gimp-image-get-active-layer sfImage)) ;The layer to which to add the mask
                                                              0))) ; The type of mask
            (gimp-rect-select sfImage
                              0
                              0
                              (car (gimp-image-width sfImage))
                              (car (gimp-image-height sfImage))
                              2
                              FALSE
                              0)
            (gimp-selection-translate sfImage
                                      (* -1 (* (/ sfPercentOfTranslation 100) (car (gimp-image-width sfImage))))
                                      0)
            (gimp-context-set-foreground "#000000")
            (gimp-edit-fill (car (gimp-layer-get-mask (car (gimp-image-get-active-layer sfImage))))
                            0) ; Foreground fill
            (gimp-selection-none sfImage)
        )
        ((2) (gimp-image-add-layer sfImage ; The image
                                  (car
                                       (gimp-layer-new-from-visible sfImage ; The source image from where the content is copied
                                                                    sfImage ; The destination image to which to add the layer
                                                                    sfLayerName) ; The layer name
                                 )
                                 -1) ; The layer position
            (gimp-layer-translate (car (gimp-image-get-active-layer sfImage))
                                  (* -1 (car (gimp-image-width sfImage)) (/ sfPercentOfTranslation 100))
                                  0)
            (gimp-image-resize sfImage
                               (+ (car (gimp-image-width sfImage)) (* (car (gimp-image-width sfImage)) (/ sfPercentOfTranslation 100)))
                               (car (gimp-image-height sfImage))
                               (* (car (gimp-image-width sfImage)) (/ sfPercentOfTranslation 100))
                               0)
        )
            ; !!!!!!!! Copy Paste !!!!!!!!
        ((3) (gimp-image-add-layer sfImage ; The image
                                  (car
                                       (gimp-layer-new-from-visible sfImage ; The source image from where the content is copied
                                                                    sfImage ; The destination image to which to add the layer
                                                                    sfLayerName) ; The layer name
                                 )
                                 -1) ; The layer position
            (gimp-layer-translate (car (gimp-image-get-active-layer sfImage))
                                  (* (car (gimp-image-width sfImage)) (/ sfPercentOfTranslation 100))
                                  0)
            (gimp-image-resize sfImage
                               (+ (car (gimp-image-width sfImage)) (* (car (gimp-image-width sfImage)) (/ sfPercentOfTranslation 100)))
                               (car (gimp-image-height sfImage))
                               0;(* -1 (car (gimp-image-width sfImage)) (/ sfPercentOfTranslation 100))
                               0)
            ; !!!!!!!! /Copy Paste !!!!!!!!
        )
    )

    (gimp-context-pop)
    (gimp-displays-flush)
    (gimp-image-undo-group-end sfImage)
)

(script-fu-register "script-fu-initial-image"
    "Initial image"
    "Copies automaticly the visible and insert this. Than it creates a Layer-Mask and makes a part of the insert image invisible."
    "Mr-L"
    "Copyright (c) 2010 by Mr-L"
    "06.02.2010"
    "*"
    SF-IMAGE "Image" 0
    SF-STRING "Name of layer" "Original"
    SF-ADJUSTMENT "Percent of invisible/translation" '(50 0 100 5 10 0 0)
    SF-OPTION "Position of new layer" '("Left (make invisible)" "Right (make invisible)" "Left (translate)" "Right (translate)"))
(script-fu-menu-register "script-fu-initial-image" "<Image>/Filters")