(define (leon-sub-pencil-draw-selection img lay ang len opc)
  (let* (
        (width (car (gimp-drawable-width lay)))
        (height (car (gimp-drawable-height lay)))
	(nl 0)
        ) 
   (gimp-context-push) 
   (gimp-image-undo-group-start img)
   (gimp-image-add-layer img (car (gimp-layer-new img width height 1 "drawing" opc 0)) -1)
   (plug-in-rgb-noise 1 img (car (gimp-image-get-active-drawable img)) 0 0 0.5 0.5 0.5 0.5)
   (gimp-displays-flush)
   (plug-in-mblur-inward 1 img (car (gimp-image-get-active-drawable img)) 0 len ang 0 0)
   (gimp-displays-flush)
   (gimp-image-undo-group-end img)
   (gimp-context-pop)
  )
)

(script-fu-register "leon-sub-pencil-draw-selection"
                    "Pencil-draw subfunction"
                    "Íatching of the current selection in new layer"
                    "Leonid Koninin"
                    "Leonid Koninin"
                    "2011"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to split" 0
                    SF-VALUE    "Angle" "20"
		    SF-VALUE    "Length" "10"
		    SF-VALUE    "Opacity" "35"
		    )
(script-fu-menu-register "leon-sub-pencil-draw-selection"
                         "<Image>/Filters/Leon")
