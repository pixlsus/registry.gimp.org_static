(define (script-fu-grain-colors img
                             drawable
                             blur-radius)
    ( let ( 
        ( new-layer 0 ) 
        ( new-layer-2 0 )
    ) 
  ; Create a new layer
  (gimp-context-push)
  (gimp-image-undo-group-start img)

  (set! new-layer (car (gimp-layer-copy drawable 0)))
  (gimp-drawable-set-name new-layer "colors")
  (gimp-image-add-layer img new-layer -1)
  (plug-in-despeckle 1 img new-layer blur-radius 0 0 255)
  (gimp-layer-set-mode new-layer 20)
  (set! new-layer-2 (car (gimp-layer-new-from-visible img img "grain")))
  (gimp-image-add-layer img new-layer-2 -1)
  (gimp-layer-set-mode new-layer-2 21)
  (gimp-layer-set-mode new-layer 0)
  
  ; Flush the display
  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
  (gimp-context-pop)
  
  )
)

(script-fu-register "script-fu-grain-colors"
                    "Grain-Colors"
                    "Split image to the grain and colors layers"
                    "Leonid Koninin"
                    "Leonid Koninin"
                    "2011"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to split" 0
                    SF-VALUE    "Colors mix strength" "5")
(script-fu-menu-register "script-fu-grain-colors"
                         "<Image>/Filters/Leon")

