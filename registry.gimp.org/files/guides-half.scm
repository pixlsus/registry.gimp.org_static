(define (script-fu-half-guides InImage)
  (gimp-image-undo-group-start InImage)
  (let* (
        (width (car (gimp-image-width InImage)))
      	(height (car (gimp-image-height InImage)))
        )
        
	(gimp-image-add-hguide InImage (/ height 2))
	(gimp-image-add-vguide InImage (/ width 2))

  (gimp-image-undo-group-end InImage)
  (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-half-guides"
  "<Image>/Image/Guides/Halfies"
  "Add guides in half of image"
  "Paxvlae"
  "Public Domain"
  "October 2013"
  "*"
  SF-IMAGE      "Image"   0
)
