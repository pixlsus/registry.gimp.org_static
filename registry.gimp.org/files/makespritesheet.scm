;;Autor: Markus Rietz 

(script-fu-register 
 "make-sprite-sheet"
 "Make Sprite Sheet"
 "Auto Offsets all Layers to the Size of the Selected Layer"
 "Markus Rietz"
 "Markus Riet, Licence: GNU 2.0"
 "04/04/2012"
 ""
 
 SF-IMAGE    "select prime image" 0;Image_AImage
 SF-DRAWABLE "select layer" 0;DrawableA_Layer
 SF-ADJUSTMENT    "Spalten" '(1 1 1000 1 1 0 1)
 SF-ADJUSTMENT    "Zeilen" '(1 1 1000 1 1 0 1)
 SF-TOGGLE "Flip X" 0
 SF-TOGGLE "Flip Y" 0
 )
(script-fu-menu-register "make-sprite-sheet"
                         "<Image>/File/Create/")

(define (make-sprite-sheet ORG_Image
                           MRG_Layer
                           spalten
                           zeilen
                           flipX
                           flipY)
  (let* (
           (LayerCount (car (gimp-image-get-layers ORG_Image)))
           (NewImageWidth   (car(gimp-drawable-width MRG_Layer)))
           (NewImageHeight  (car(gimp-drawable-height MRG_Layer)))
           (AllLayerList  (vector->list(cadr (gimp-image-get-layers ORG_Image))))
           (countspalten 0)
           (countzeilen 0)
           (var_drawable)
           (InsertLayer)
           (MRGLayerCopy)
           (invertX)
           (invertY)
           (MovX)
           (MovY)
           (NewImage (car (gimp-image-new (* NewImageWidth spalten)  (* NewImageHeight zeilen) RGB)) )
           )
    
    (if (= flipX TRUE)
        (set! invertX -1)
        (set! invertX 1))
    (if (= flipY TRUE)
        (set! invertY -1)
        (set! invertY 1))
    (while  (and(> LayerCount 0)(or (> spalten countspalten) (> zeilen (+ countzeilen 1)))) 
           (begin
             
             
             (while (and (> spalten countspalten ) (> LayerCount 0))
                    
                    (set! var_drawable (car AllLayerList))
                    (set! MRGLayerCopy (car(gimp-layer-new-from-drawable var_drawable NewImage)))
                    (gimp-image-insert-layer NewImage MRGLayerCopy 0 -1)
                    (set! MovX (* (* NewImageWidth countspalten) invertX))
                    (set! MovY (* (* NewImageHeight countzeilen) invertY))
                    (gimp-layer-set-offsets MRGLayerCopy MovX MovY)
                    (set! AllLayerList (cdr AllLayerList))
                    (set! LayerCount (- LayerCount 1))                        
                    (set! countspalten (+ countspalten 1))
                    )
             (if (and (> LayerCount 0) (> zeilen (+ countzeilen 1) ))
                 (begin
                   (set! var_drawable (car AllLayerList))
                   (set! MRGLayerCopy (car(gimp-layer-new-from-drawable var_drawable NewImage)))
                   (set! countzeilen (+ countzeilen 1))
                   (gimp-image-insert-layer NewImage MRGLayerCopy 0 -1)
                   (set! MovX 0)
                   (set! MovY (* (* NewImageHeight countzeilen) invertY))
                   (gimp-layer-set-offsets MRGLayerCopy MovX MovY)
                   (set! AllLayerList (cdr AllLayerList))
                   (set! LayerCount (- LayerCount 1))
                   
                   (set! countspalten 1)
                   )
                 )
             )
           )
    (gimp-image-resize-to-layers NewImage)
    (gimp-display-new NewImage)
    )
  )

