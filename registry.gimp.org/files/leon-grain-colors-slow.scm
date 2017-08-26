; version 2, for a slower computers
(define (script-fu-grain-colors-slow img
                             drawable
                             blur-radius)
    ( let ( 
        ( new-layer 0 ) 
        ( new-layer-2 0 )
		( new-layer-3 0 )
    ) 
  ; Create a new layer
  (gimp-context-push)
  (gimp-image-undo-group-start img)

  (set! new-layer (car (gimp-layer-copy drawable 0)))
  (gimp-drawable-set-name new-layer "colors")
  (gimp-image-add-layer img new-layer -1)
  (plug-in-despeckle 1 img new-layer blur-radius 0 -1 256)

  (gimp-image-add-layer img (car (gimp-layer-copy drawable 0)) -1)
  (set! new-layer-3 (car (gimp-layer-copy new-layer 0)))
  (gimp-image-add-layer img new-layer-3 -1)
  (gimp-layer-set-mode new-layer-3 20)
  (gimp-displays-flush)
  (set! new-layer-3 (car (gimp-image-merge-down img new-layer-3 0)))
  (gimp-drawable-set-name new-layer-3 "grain")
  
  (gimp-layer-set-mode new-layer-3 21)
  (gimp-layer-set-mode new-layer 0)
  
  ; Flush the display
  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
  (gimp-context-pop)
  
  )
)

(script-fu-register "script-fu-grain-colors-slow"
                    "Grain-Colors (Slow)"
                    "Split image to the grain and colors layers"
                    "Leonid Koninin"
                    "Leonid Koninin"
                    "2011"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to split" 0
                    SF-VALUE    "Colors mix strength" "5")
(script-fu-menu-register "script-fu-grain-colors-slow"
                         "<Image>/Filters/Leon")

